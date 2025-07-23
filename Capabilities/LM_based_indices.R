#'
#' @title  Section 4.2.1: Transformer-based LM Topic Exposure and Topic-specific Tone
#'
#' @description This code takes a template for topic column names and thresholds (based on LM output columns) to build an index reflecting
#' Topic exposure (how often topic is discussed) and tone (sentiment of discussion) measures using LM output.
#'
#' @author Nicholas Gray
#'
#' @param file_location (string): location of uncertainty dictionary.
#' @param output_dir (string or path): location to save BLUI output file.
#'
#' @output excel file with aggregated Topic exposure and topic-specific tone measures using LM output.

## Import packages (see requirements.txt for replication package version)
req_pack <- c(
  "tidyverse", "scales", "glue", "odbc",
  "data.table", "DBI", "RSQLite",
  "furrr", "writexl", "readxl"
)
for (p in req_pack) {
  # if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Import user-defined functions
source("data_gen_utils.R")


#### PARAM for input and output ####
dict_dir <- "../Data/LM_dictionary.xlsx"
output_dir <- "../Data/LM_topic_measures.xlsx"


###### Import guide template for filtering topic thresholds
guide <- read_excel(path = dict_dir, sheet = "Wage")

# Initialize an empty list to store keywords
keywords <- list()

# Loop through each sheet and extract the required information
for (i in c(1:nrow(guide))) {
  keywords[[guide$Name[i]]] <- list(guide$Column[i], guide$Threshold[i])
}


###### Import all Liaison body text (text from paragraphs, dot points, etc)
# initialise connection to SQLite database
mydb <- dbConnect(SQLite(), "../Data/liaison.sqlite")
# extract data
content <- dbGetQuery(mydb, "SELECT * FROM liaison_data WHERE category = 'BODY'")

## Liaison-level aggregation before time period, other option is straight to month or quarter
agg_by <- "file_id"

## Save liaison level date column to join on later
liaisons <- content |>
  mutate(
    quarter = ceiling_date(ymd(ContactDate), "quarter") - 1,
    month = ceiling_date(ymd(ContactDate), "month") - 1,
    para = map(text, ~ list(.))
  ) |>
  summarise(
    quarter = max(quarter),
    month = max(month),
    .by = all_of(agg_by)
  )


###### Topic Exposure
## create measurement function
fmap <- imap(keywords, ~ LM_topic_exp(.y, .x))

## apply each topic measure function to dataframe
dataframes <- map(fmap, ~ .x(content, groupings = agg_by))

## join each measure to date column for liaison
exp_indexes <- liaisons
for (i in seq_along(dataframes)) {
  exp_indexes <- left_join(exp_indexes, dataframes[[i]], by = agg_by)
}

## aggregate up to quarterly
exp_indexes_q <- exp_indexes |>
  group_by(quarter) |>
  summarise(across(contains(".LM"), ~ mean(., na.rm = T))) |>
  rename_with(~ tolower(gsub(".LM", "_lm_exposure", .x, fixed = TRUE)))

## monthly
exp_indexes_m <- exp_indexes |>
  group_by(month) |>
  summarise(across(contains(".LM"), ~ mean(., na.rm = T))) |>
  rename_with(~ tolower(gsub(".LM", "_lm_exposure", .x, fixed = TRUE)))


###### Topic-specific Tone
## create measurement function
fmap <- imap(keywords, ~ LM_topic_tone(.y, .x))

## apply each topic tone measure function to dataframe
dataframes <- map(fmap, ~ .x(content, groupings = agg_by))

## join each measure to date column for liaison
tone_indexes <- liaisons
for (i in seq_along(dataframes)) {
  tone_indexes <- left_join(tone_indexes, dataframes[[i]], by = agg_by)
}

## aggregate up to quarterly
tone_indexes_q <- tone_indexes |>
  group_by(quarter) |>
  summarise(across(contains(".LM"), ~ mean(., na.rm = T))) |>
  rename_with(~ tolower(gsub(".LM", "_lm_tone", .x, fixed = TRUE)))

## monthly
tone_indexes_m <- tone_indexes |>
  group_by(month) |>
  summarise(across(contains(".LM"), ~ mean(., na.rm = T))) |>
  rename_with(~ tolower(gsub(".LM", "_lm_tone", .x, fixed = TRUE)))


###### Join together
indexes_q <- exp_indexes_q |>
  left_join(tone_indexes_q, by = "quarter") |>
  ungroup() |>
  mutate(across(contains("_lm"), ~ standardise(.x)))

indexes_m <- exp_indexes_m |>
  left_join(tone_indexes_m, by = "month") |>
  ungroup() |>
  mutate(across(contains("_lm"), ~ standardise(.x)))


###### Save outputs
write_xlsx(
  list(
    "monthly" = indexes_m,
    "quarterly" = indexes_q
  ),
  path = output_dir
)
