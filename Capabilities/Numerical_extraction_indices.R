#'
#' @title  Section 4.4:	Extracting precise numerical quantities
#'
#' @description This code pulls the extracted numerical quantities from the TARS database and creates a trimmed time series.
#' The actual numerical extraction step using a suite of Transformer-based LMs can be found in the `backend` folder of the repository
#'
#' @author Nicholas Gray
#'
#' @param output_dir (string or path): location to save aggregated measures.
#'
#' @output excel file with trimmed time series of prices and wages numerical quantites.
#'

## Import packages (see requirements.txt for replication package version)
req_pack <- c(
  "tidyverse", "tidytext", "DBI", "odbc",
  "writexl", "scales", "glue", "RSQLite",
  "data.table", "purrr", "readxl"
)
for (p in req_pack) {
  # if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

## Import user-defined functions
source("data_gen_utils.R")


#### PARAM, input sql query and output dir ####
output_dir <- "../Data/numerical_extraction_series.xlsx"


### import liaison content from TARS
# initialise connection to SQLite database
mydb <- dbConnect(SQLite(), "../Data/liaison.sqlite")
# extract data
content <- dbGetQuery(mydb, "SELECT * FROM liaison_data WHERE category = 'BODY'")


### Price quantities extraction
df_price_q <- numerical_extraction(content, "PricesExtract", "quarter") |>
  rename_with(.fn = ~ paste0("Price_", .x), .cols = contains("num"))

df_price_m <- numerical_extraction(content, "PricesExtract", "month", remove_partial = T) |>
  rename_with(.fn = ~ paste0("Price_", .x), .cols = contains("num"))


### Wages quantities extraction
df_wages_q <- numerical_extraction(content, "WagesExtract", "quarter") |>
  rename_with(.fn = ~ paste0("Wages_", .x), .cols = contains("num"))

df_wages_m <- numerical_extraction(content, "WagesExtract", "month", remove_partial = T) |>
  rename_with(.fn = ~ paste0("Wages_", .x), .cols = contains("num"))


### Join each aggregate series into one dataframe
# monthly
df_numext_m <- df_price_m |>
  left_join(df_wages_m, by = "Date") |>
  filter(Date > ymd("2006-01-01"))

# quarterly
df_numext_q <- df_price_q |>
  left_join(df_wages_q, by = "Date") |>
  filter(Date > ymd("2006-01-01"))


#### Save outputs
write_xlsx(
  list(
    "monthly" = indices_m,
    "quarterly" = indices_q
  ),
  path = output_dir
)
