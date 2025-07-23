#'
#' @title  Section 4.3: Dictionary-based Business Liaison Uncertainty Index
#'
#' @description This code takes a template dictionary of terms that relate to firms discussion of uncertainty. 
#' Search for and build an index that tracks firms mentions of uncertainty over time (the BLUI).
#'
#' @author Nicholas Gray
#'
#' @param dict_dir (string or path): location of uncertainty dictionary.
#' @param output_dir (string or path): location to save BLUI output file. 
#'
#' @output excel file with Business Liaison Uncertainty Index

## Import packages (see requirements.txt for replication package version)
req_pack <- c(
  "tidyverse", "scales", "glue", "odbc",
  "DBI", "RSQLite", "tidytext", "furrr",
  "data.table", "writexl", "readxl"
  
)
for (p in req_pack) {
  #if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Import user-defined functions
source("data_gen_utils.R")

#### PARAM for input and output ####
dict_dir <- '../Data/uncertainty_dict.xlsx'
output_dir <- '../Data/business_liaison_uncertainty_index.xlsx'

#### Import dictionary and make apostrophes the generic one
dictionary <- read_excel(dict_dir, col_names = c("BLUI"))
dictionary <- dictionary |> mutate(BLUI = sapply(BLUI, replace_apostrophes))

#### Import all Liaison from TARS
# initialise connection to SQLite database  
mydb <- dbConnect(SQLite(), "../Data/liaison.sqlite")
#extract data
content <- dbGetQuery(mydb, "SELECT * FROM liaison_data WHERE category = 'BODY'")


## Liaison-level aggregation before time period, other option is straight to month or quarter
agg_by <- "file_id" 

## Create liaison level version of table without text (for downstream aggregation)
liaisons <- content |>
  mutate(    
    quarter = ceiling_date(ymd(ContactDate), "quarter") - 1,
    month = ceiling_date(ymd(ContactDate), "month") - 1
  ) |>
  summarise(quarter = max(quarter),
            month = max(month),
            total_words = sum(nwords),
            .by = agg_by)

## Split paragraphs into sentences (only to be consistent with RDP - doesn't change results)
content_sentences <- content |>
  unnest_tokens(sentences, text, token = "sentences")

## Create regex filters for counting terms
filt <- list_to_regex(dictionary$BLUI, boundary = F)
filt_b <- list_to_regex(dictionary$BLUI) ## boundary = T will prevent hits of substrings of words, e.g. oil in boiler

#### Filter by sentences and count uncertain terms
df <- content_sentences |> 
  filter(str_detect(pattern = regex(filt), sentences)) |> ## prefilter without boundary marker on terms
  mutate(count = str_count(sentences, pattern = regex(filt_b))) |>
  summarise(topic_count = sum(count), .by = agg_by)

## join to date and total count series
indices <- liaisons |>
  left_join(df, by = agg_by) |> 
  mutate(topic_count = coalesce(topic_count,0),
         BLUI = topic_count/total_words) 

#### Quarterly series
indices_q <- indices |>
  summarise(BLUI = mean(BLUI), .by = "quarter")

#### Monthly series
indices_m <- indices |>
  summarise(BLUI = mean(BLUI), .by = "month")


#### Output file
write_xlsx(list("monthly" = indices_m,
                "quarterly" = indices_q),
                path = output_dir)
