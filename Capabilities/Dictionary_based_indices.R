#'
#' @title  Section 4.2.2: Dictionary-based Topic Exposure and Tone measures
#'
#' @description
#'
#' @author Nicholas Gray
#'
#' @param dict_dir (string or path): location of uncertainty dictionary.
#' @param output_dir (string or path): location to save topic measure output file.
#'
#' @output excel file with Topic exposure and tone measures at monthly and quarterly levels

## Import packages (see requirements.txt for replication package version)
req_pack <- c(
  "tidyverse", "scales", "glue", "odbc",
  "DBI", "RSQLite", "tidytext", "furrr",
  "data.table", "writexl", "readxl"
)
for (p in req_pack) {
  # if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Import user-defined functions
source("data_gen_utils.R")


#### PARAM for input and output ####
dict_dir <- "../Data/dictionary.xlsx"
output_dir <- "../Data/Dictionary_topic_measures.xlsx"


#### Import dictionary
dictionary <- load_dictionary_index(dict_dir)

## create measurement function
fmap <- lapply(names(dictionary$searches), function(search_key) {
  k <- dictionary$searches[[search_key]][[1]]
  q <- dictionary$searches[[search_key]][[2]]
  count_topic_keyword_func(k, q)
})

## Set the names of the list to the search keys
names(fmap) <- names(dictionary$searches)


#### Import all Liaison body text (text from paragraphs, dot points, etc)
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
    total_words = sum(nwords),
    .by = all_of(agg_by)
  )

## Split paragraphs into sentences to narrow qualifier window for each keyword
content_sentences <- content |>
  unnest_tokens(sentences, text, token = "sentences")


#### Apply each topic measure function to dataframe
aggs <- apply_wordsearches(content_sentences,
  groupby = agg_by,
  dictionary = dictionary,
  fmap = fmap,
  column = "sentences"
)

## join each measure to date column for liaison
for (i in seq_along(aggs)) {
  liaisons <- left_join(liaisons, aggs[[i]], by = agg_by)
}

## fill missing data with zeros (since no topic words appear)
liaisons <- liaisons |> mutate(across(names(dictionary$searches), ~ coalesce(., 0)))


##### Aggregate each keyword/qualifier pair up to Topic level
# Loop through each index category in the dictionary
for (inds in names(dictionary$indices)) {
  print(paste0("Generating indices for: ", inds))

  # Loop through each subcategory (Series) within the current index
  for (Series in names(dictionary$indices[[inds]])) {
    # Construct a new column name using the index and series
    col_name <- paste0(inds, "(", Series, ")")

    # Get the list of column names to aggregate for this series
    agg_cols <- dictionary$indices[[inds]][[Series]]

    # Aggregate the specified columns by row-wise summation and assign to the new column
    liaisons[[col_name]] <- rowSums(liaisons[, agg_cols])
  }

  # Clean the index name by removing ".WC" if present, for use in naming new columns
  new_set <- gsub("\\.WC", "", inds)

  # If there's an "I" (intensity or incidence) column for this index, calculate topic exposure
  if (any(grepl("\\w+\\(I\\)", x = names(liaisons), perl = TRUE))) {
    liaisons[[paste0(new_set, "_topic_exposure")]] <- liaisons[[paste0(inds, "(I)")]] / liaisons[["total_words"]]
  }

  # If there are both positive (+) and negative (-) sentiment columns, calculate topic tone
  if (any(grepl("\\w+\\([+-]\\)", x = names(liaisons), perl = TRUE))) {
    liaisons[[paste0(new_set, "_topic_tone")]] <- (liaisons[[paste0(inds, "(+)")]] - liaisons[[paste0(inds, "(-)")]]) / liaisons[[paste0(inds, "(I)")]]
  }
}

## fill missing data with zeros (since no topic words appear)
liaisons <- liaisons |> mutate(across(contains("topic"), ~ coalesce(., 0)))


#### take mean across all indices
# quarterly
indices_q <- liaisons |>
  select(file_id, quarter, month, contains("topic")) |>
  summarise(across(contains("topic"), mean), .by = "quarter")

# monthly
indices_m <- liaisons |>
  select(file_id, quarter, month, contains("topic")) |>
  summarise(across(contains("topic"), mean), .by = "month")


#### Save outputs
write_xlsx(
  list(
    "monthly" = indices_m,
    "quarterly" = indices_q
  ),
  path = output_dir
)
