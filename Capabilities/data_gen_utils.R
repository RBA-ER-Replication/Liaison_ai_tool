#'
#' @title  RDP Section 4: Capabilities - Function file
#'
#' @description Utility functions used in each Capabilities files 
#'
#' @author Nicholas Gray



#' Parse Keyword Table into Structured Lookup Lists
#'
#' This function processes a data frame of keywords and optional qualifiers to create
#' structured lookup lists for keyword-based searching and indexing.
#'
#' @param df A data frame containing at least a 'keyword' column, and optionally 'qualifiers' and 'subindex' columns.
#' @return A list containing:
#'   \item{key2search}{A named list mapping unique keys to keyword and qualifier combinations.}
#'   \item{index2key}{A list mapping index names (e.g., "WC", subindex names) to associated keys.}
#'   \item{prefilter}{A character vector of original keywords for prefiltering.}
#'
#' @export
parse_keyword_table <- function(df) {
  # Initialize lookup lists
  key2search <- list()
  index2key <- list(WC = character())

  # Extract original keywords for prefilter
  prefilter <- df |> pull(keyword)

  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
    row <- df[i, ]

    # Normalize and trim the keyword
    keyword <- str_trim(tolower(row$keyword))

    # Process qualifiers: use keyword itself if missing, otherwise split and normalize
    if (is.na(row$qualifiers)) {
      qualifiers <- keyword
    } else {
      qualifiers <- str_trim(tolower(unlist(strsplit(row$qualifiers, ","))))
    }

    # Sort qualifiers for consistent key generation
    qualifiers <- sort(qualifiers)

    # Create a unique key from keyword and qualifiers
    key <- paste0(keyword, ":", paste(qualifiers, collapse = ","))

    # Store keyword and qualifiers under the generated key
    key2search[[key]] <- list(keyword = keyword, qualifiers = qualifiers)

    # Add key to the default "WC" index
    index2key$WC <- c(index2key$WC, key)

    # If subindex column exists, add key to the corresponding subindex
    if ("subindex" %in% colnames(df)) {
      current_subindex_name <- str_replace_all(row$subindex, "\\s", "")
      if (!current_subindex_name %in% names(index2key)) {
        index2key[[current_subindex_name]] <- character()
      }
      index2key[[current_subindex_name]] <- c(index2key[[current_subindex_name]], key)
    }
  }

  # Return the structured lookup lists and prefilter vector
  return(list(key2search = key2search, index2key = index2key, prefilter = prefilter))
}



#' Load and Parse Dictionary Index from Excel Workbook
#'
#' This function reads all sheets from an Excel workbook, parses keyword tables from each sheet,
#' and organizes them into structured search and index lists. Sheet names must follow the pattern
#' "IndexName(Direction)", where Direction is one of '+', '-', 'I', or 'U'.
#'
#' @param xls Path to an Excel workbook containing keyword tables in named sheets.
#' @return A list containing:
#'   \item{searches}{A named list of keyword and qualifier combinations for searching.}
#'   \item{indices}{A nested list mapping index and subindex names to direction-specific keyword keys.}
#'   \item{prefilter}{A nested list of original keywords for each index and direction.}
#'
#' @export
load_dictionary_index <- function(xls) {
  # Regular expression to extract index name and direction from sheet name
  p <- "([a-zA-Z]{1,100})\\(([+-IU])\\)"

  # Initialize containers for results
  indices <- list()
  searches <- list()
  prefilter <- list()

  # Get all sheet names from the Excel file
  sheet_names <- excel_sheets(xls)

  # Iterate over each sheet
  for (name in sheet_names) {
    m <- str_match(name, p)

    # Skip sheets that don't match the naming convention
    if (is.na(m[1])) {
      cat(sprintf("Warning, sheet %s doesn't follow naming standards. Skipping it.\n", name))
    } else {
      cat(sprintf("Processing sheet %s\n", name))

      # Extract index name and direction from sheet name
      index <- m[2]
      direction <- m[3]

      # Read the sheet into a data frame
      df <- read_excel(xls, sheet = name)

      # Parse the keyword table into structured lookup lists
      parsed <- parse_keyword_table(df)
      key2search <- parsed$key2search
      index2key <- parsed$index2key

      # Merge parsed keyword search entries into the global search list
      for (key in names(key2search)) {
        searches[[key]] <- key2search[[key]]
      }

      # Organize keys under index.subindex and direction
      for (subindex in names(index2key)) {
        index_name <- paste0(index, ".", subindex)

        if (!index_name %in% names(indices)) {
          indices[[index_name]] <- list()
        }
        indices[[index_name]][[direction]] <- index2key[[subindex]]

        # Store original keywords for prefiltering
        prefilter[[index_name]][[direction]] <- parsed$prefilter
      }
    }
  }

  # Return the structured search and index data
  return(list(searches = searches, indices = indices, prefilter = prefilter))
}



#' Convert List of Strings to Regex Pattern
#'
#' This function takes a list of strings and converts them into a single regex pattern,
#' optionally adding word boundaries to match whole words only.
#'
#' @param ls A character vector of strings to be combined into a regex pattern.
#' @param boundary Logical; if TRUE, adds word boundaries (\\b) around each string. Defaults to TRUE.
#' @return A single regex pattern string combining all input strings with '|' as the separator.
#' @examples
#' list_to_regex(c("apple", "banana", "cherry"))
#' list_to_regex(c("apple", "banana"), boundary = FALSE)
#' @export
list_to_regex <- function(ls, boundary = TRUE) {
  # Add word boundaries and convert to lowercase if boundary is TRUE
  if (boundary) {
    ls <- paste0("\\b", paste0(str_to_lower(ls), "\\b"))
  } else {
    ls <- str_to_lower(ls)
  }

  # Combine all strings into a single regex pattern separated by '|'
  return(paste(ls, collapse = "|"))
}



#' Replace Various Types of Apostrophes with Standard Apostrophe
#'
#' This function standardizes apostrophes in a given text by replacing various Unicode
#' apostrophe-like characters with the standard ASCII apostrophe (').
#'
#' @param text A character string potentially containing different types of apostrophes (used in dplyr::mutate).
#' @return A character string with all apostrophes replaced by the standard apostrophe.
#'
#' @export
replace_apostrophes <- function(text) {
  gsub("[‘’ʼ＇]", "'", text)
}



#' Standardise a Numeric Series
#'
#' This function standardises a numeric vector by subtracting the mean and dividing by the standard deviation.
#' Missing values (NA) are ignored in the calculation of mean and standard deviation.
#'
#' @param series A numeric vector to be standardised.
#' @return A numeric vector with mean 0 and standard deviation 1 (excluding NAs).
#'
#' @export
standardise <- function(series) {
  (series - mean(series, na.rm = TRUE)) / sd(series, na.rm = TRUE)
}



#' Create a Keyword Counting Function for a Topic with Qualifiers
#'
#' This function returns another function that, when applied to a data frame and column name,
#' counts the number of occurrences of a specified topic and its associated qualifiers.
#'
#' @param topic A character string representing the main keyword to search for.
#' @param qualifiers A character vector of qualifier terms to count when the topic is present.
#' @return A function that takes a data frame and column name, and returns a summary with the count of qualifier matches.
#' @examples
#' count_func <- count_topic_keyword_func("inflation", c("core", "headline"))
#' df <- data.frame(text = c("Core inflation rose", "Headline inflation dropped", "No mention"))
#' count_func(df, "text")
#' @export
count_topic_keyword_func <- function(topic, qualifiers) {
  # Convert the list of qualifiers into a regex pattern with word boundaries
  qual_p <- list_to_regex(qualifiers)

  # Return a function that operates on a data frame and column name
  f <- function(df, col) {
    # Filter rows containing the topic and count qualifier matches
    df_grouped <- df |>
      filter(str_detect(pattern = regex(topic), !!sym(col))) |> # Broad match for topic, two step search is faster
      filter(str_detect(pattern = regex(paste0("\\b", topic, "\\b")), !!sym(col))) |> # Exact word match
      mutate(count = str_count(!!sym(col), pattern = regex(qual_p))) |> # Count qualifier matches
      summarise(topic_count = sum(count)) # Sum counts across rows

    return(df_grouped)
  }

  return(f)
}



#' Apply Word Search Functions Across Dictionary Indices
#'
#' This function applies a set of keyword counting functions (from a dictionary and function map)
#' to a data frame, grouped by a specified column. It filters and processes only indices ending in ".WC".
#'
#' @param df A data frame containing the text data to be searched.
#' @param groupby A string specifying the column name to group results by.
#' @param dictionary A structured dictionary object containing indices and prefilter terms.
#' @param fmap A named list of functions, each corresponding to a keyword key in the dictionary. Generated by `count_topic_keyword_func`
#' @param column A string specifying the column in `df` to search within. Defaults to "text".
#'
#' @return A named list of data frames, each containing grouped counts for a specific keyword.
#'
#' @export
apply_wordsearches <- function(df, groupby, dictionary, fmap, column = "") {
  # Initialize result container
  content <- list()

  # Default to "text" column if none specified
  if (column == "") {
    column <- "text"
  }

  # Iterate over each index in the dictionary
  for (inds in names(dictionary$indices)) {
    # Only process indices with names ending in ".WC"
    if (str_detect(inds, "\\.WC$")) {
      print(paste0("Searching dictionary for: ", inds))

      # Iterate over each series (e.g., direction) in the index
      for (Series in names(dictionary$indices[[inds]])) {
        # Create regex patterns for prefiltering, one with word boundaries, one without
        prefilter <- list_to_regex(dictionary$prefilter[[inds]][[Series]], boundary = FALSE)
        prefilter_b <- list_to_regex(dictionary$prefilter[[inds]][[Series]])

        # Filter and group the data frame based on prefilter matches
        df_subset <- df |>
          filter(str_detect(pattern = regex(prefilter), !!sym(column))) |> # two step search is faster
          filter(str_detect(pattern = regex(prefilter_b), !!sym(column))) |>
          group_by(!!sym(groupby))

        # Apply each keyword counting function to the filtered data
        for (f_name in dictionary$indices[[inds]][[Series]]) {
          count <- fmap[[f_name]](df_subset, column)
          names(count) <- c(groupby, f_name)
          content[[f_name]] <- count
        }
      }
    } else {
      print(paste0("Skipping search for: ", inds))
    }
  }

  # Return the list of keyword count data frames
  return(content)
}



#' Create a Topic Exposure Generator Based on a Topic Threshold
#'
#' This function generates a function that calculates the proportion of rows in a data frame
#' where a specified column exceeds a given threshold for the topic. The result is grouped by a specified column.
#'
#' @param key_name A string used as the name of the resulting exposure column.
#' @param value A list where the first element is the column name (or topic) to evaluate, and the second is the threshold value.
#' @return A function that takes a data frame and a grouping column, and returns a summary of exposure proportions.
#' @examples
#' @export
LM_topic_exp <- function(key_name, value) {
  # Return a function that calculates exposure based on threshold comparison
  f <- function(df, groupings) {
    print(key_name)

    # Create binary exposure variable and compute mean exposure by group
    count <- df |>
      mutate(exposure = as.integer(df[value[[1]]] > value[[2]])) |>
      group_by(!!sym(groupings)) |>
      summarise(!!sym(key_name) := mean(exposure))

    return(count)
  }

  return(f)
}



#' Create a Topic-specific Tone generator Based on a Topic Threshold
#'
#' This function generates another function that filters a data frame based on a threshold
#' for a specified column (or topic), then calculates the average sentiment score for each group.
#'
#' @param key_name A string used as the name of the resulting tone column.
#' @param value A list where the first element is the column name (or topic) to filter on, and the second is the threshold value.
#' @return A function that takes a data frame and a grouping column, and returns a summary of average sentiment scores.
#' @examples
#' @export
LM_topic_tone <- function(key_name, value) {
  # Return a function that filters by threshold and computes average sentiment score by group
  f <- function(df, groupings) {
    print(key_name)

    # Filter rows where the specified column exceeds the threshold, then compute mean sentiment
    count <- df |>
      filter(!!sym(value[[1]]) > !!value[[2]]) |>
      group_by(!!sym(groupings)) |>
      summarise(!!sym(key_name) := mean(sentiment_score))

    return(count)
  }

  return(f)
}


#' Take upper/lower decile of vector, used in summarise of a column(s)
#'
#' @param x a vector of numerics
#'
#' @return a numeric representing decile value or vector
#'
#' @examples
q10 <- function(x) {
  quantile(x, 0.1)
}
q90 <- function(x) {
  quantile(x, 0.9)
}

#' Take the mean/max/min of a set after removing NA values, if all NA, through NA without warning
#'
#' @param vect a vector of numerical values, usually used to mutate a column in dataframe
#'
#' @return a single value, either numeric or NA
#'
safe_max <- function(vect) {
  tryCatch(max(vect, na.rm = T), warning = function(w) {
    NA
  })
}
safe_mean <- function(vect) {
  tryCatch(mean(vect, na.rm = T), warning = function(w) {
    NA
  })
}
safe_min <- function(vect) {
  tryCatch(min(vect, na.rm = T), warning = function(w) {
    NA
  })
}


#' Create trimmed time series for extracted numerical quantities data
#'
#' @param df (dataframe): data pulled from TARS containing numerical extraction columns
#' @param NumExtract (string): column name for numerical quatitiy extracted from liaison text, in df.
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#'
#' @return a dataframe containing a trimmed times series of aggregate numerical extractions
#'
numerical_extraction <- function(df, NumExtract, aggre, remove_partial = F) {
  ### Group by year to calculate the statistics for outlier detection
  df_num <- df[!is.na(df[[NumExtract]]), ] |>
    mutate(
      Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1,
      y = ceiling_date(ymd(as.Date(ContactDate)), unit = "year") - 1
    )

  # Calculate top and bottom deciles of each year
  df_stats <- df_num |>
    filter(!is.na(!!sym(NumExtract))) |>
    summarise(
      median = median(!!sym(NumExtract)),
      std = sd(!!sym(NumExtract)),
      max = safe_max(!!sym(NumExtract)),
      min = safe_min(!!sym(NumExtract)),
      q10 = q10(!!sym(NumExtract)),
      q90 = q90(!!sym(NumExtract)),
      .by = "y"
    )


  # Merge the original dataframe with the statistics dataframe
  df_p <- left_join(df_num, df_stats, by = "y")


  # Filter out the outliers based on top and bottom deciles
  df_p <- df_p %>%
    filter(!(!!sym(NumExtract) > q90 | !!sym(NumExtract) < q10))

  ### Group by 'LiaisonID' for liaison-level numerical quantitie,
  # then group by 'aggre' and calculate the mean time series
  extracted <- df_p %>%
    select(Date = Contact_Date, !!sym(NumExtract), file_id) %>%
    summarise(
      num_rate_ext = safe_mean(!!sym(NumExtract)),
      .by = c("Date", "file_id")
    ) %>%
    summarise(
      num_extract = safe_mean(num_rate_ext),
      extract_counts = n(),
      .by = "Date"
    ) |>
    filter(Date > ymd("2006-12-31")) |>
    arrange(Date)

  if (remove_partial) {
    # get last point in time series
    last_val <- extracted[nrow(extracted), ] |> pull(Date)

    # if last date in future, remove this partial data point
    if (last_val > ymd(Sys.Date())) {
      extracted <- extracted[1:(nrow(extracted) - 1), ]
    }
  }

  return(extracted)
}
