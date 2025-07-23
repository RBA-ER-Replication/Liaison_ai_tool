###### Create Word2Vec Model using liaison text data  #####
library(word2vec)
library(tidyverse)
library(stringr)
library(DBI)
library(RSQLite)


# initialise connection to SQLite database  
mydb <- dbConnect(SQLite(), "liaison.sqlite")


df <- dbGetQuery(mydb, "SELECT text FROM liaison_data WHERE category = 'BODY'")

## Clean the text
cleaned_text <- df |> 
  mutate(text = str_to_lower(text)) |>
  mutate(text = str_replace_all(text, "[^\\P{P}-]+", "")) |> ## remove punctuation except hyphens
  mutate(text = str_replace_all(text, "[\\^$<>[:digit:]\\p{No}+]", "")) |> ## remove numbers and other special characters
  mutate(text = str_replace_all(text, "(\\W-)|(-\\W)| - ", " ")) |> ## remove hyphens that don't link two words together
  mutate(text = str_trim(str_replace_all(text, "  ", " "))) |> ## remove excess whitespace
  mutate(text_list = str_split(text, " ")) |>
  rowwise() |>
  filter(length(text_list) > 3) |>
  pull(text_list)



set.seed(123456789)
model <- word2vec(x = cleaned_text, 
                  type="skip-gram", 
                  dim = 300, 
                  window = 10,
                  iter = 30,
                  hs = TRUE, 
                  lr = 0.001,
                  threads = 6)
write.word2vec(model, "Code/w2v.bin")
