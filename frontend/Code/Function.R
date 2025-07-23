####################################################
##### Function File for RIA Liaison Dashboard ######
####################################################
print("Loading UDF suite...")


#' Take the mean/max of a set after removing NA values, if all NA, through NA without warning
#'
#' @param vect a vector of numerical values, usually used to mutate a column in dataframe
#'
#' @return a single value, either numeric or NA
#'
safe_max <- function(vect) {tryCatch(max(vect, na.rm = T), warning = function(w) {NA})}
safe_mean <- function(vect) {tryCatch(mean(vect, na.rm = T), warning = function(w) {NA})}



#' Extract theme name from theme.txt file in directory of dashboard
#'
#' @param file a string for a file containing theme names
#'
#' @return a string that is a name of a valid theme from shinythemes package (see https://bootswatch.com/3/)
#'
choosetheme <- function(file) {
  text <- readLines(file)

  theme <- str_trim(str_split_1(text[1], ":")[2])

  if (theme %in% c(
    "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal",
    "lumen", "paper", "readable", "sandstone", "simplex", "slate",
    "spacelab", "superhero", "united", "yeti"
  )) {
    return(theme)
  } else {
    return("cerulean")
  }
}



#' Take a constraint on a category column and expand out to the wide-format constraint
#'
#' @param input
#' @param no_unders boolean, if TRUE, will remove underlines from string
#'
#' @return a single element list mapping the column name to a value
#'
check_input <- function(input, no_unders = F) {
  output <- c()
  if (length(input) == 0) {
    output <- "ANY"
  } else if (no_unders) {
    for (cols in input) {
      output <- append(output, cols)
    }
  } else {
    for (cols in input) {
      output <- append(output, gsub(" ", "_", cols))
    }
  }
  return(output)
}



#' Group of functions that clean/convert text input
#' such as split if see's multiple words separated by comma,
#' or trim white space, make lower case and add wildcards and other elements
#'
#' @param text text or list of text to be cleaned
#'
#' @return text or list of text that has been cleaned and/or converted to required format
#'
split_text <- function(text) {
  text_list <- str_split(text, pattern = ",")
  text_list <- lapply(text_list, trimws)
  return(text_list[[1]])
}
add_wildcard <- function(tl) {
  out <- paste("%", tolower(trimws(tl)), "%", sep = "")
  return(out)
}
to_first_upper <- function(words) {
  substring(words, 1, 1) <- str_to_upper(substring(words, 1, 1))
  return(words)
}
highlight_words <- function(wlist, text) {
  if (all(is.null(wlist))) {
    wlist <- "aaabbbccc"
  } else if (all(wlist == "")) {
    wlist <- "aaabbbccc"
  }
  wlist <- if (length(wlist) > 1) {
    wlist
  } else {
    trimws(str_split_1(wlist, pattern = ","))
  }
  wlist <- c(wlist, to_first_upper(tolower(wlist)), str_to_upper(wlist), tolower(wlist))
  wlist_regex <- paste("\\b", wlist, "\\b", sep = "")
  for (words in seq_along(wlist)) {
    text <- gsub(wlist_regex[words], sprintf("<span style='background-color:yellow;'>%s</span>", wlist[words]), text, perl = TRUE)
  }
  return(text)
}
check_text <- function(text) {
  if (length(text) == 0) {
    return("")
  } else {
    return(text)
  }
}



#' Create nice string to copy to clipboard
#'
#' @param text single word or list of words
#' @param Rcode boolean to state whether output should be nice R code or just text list
#'
#' @return a single element list mapping the column name to a value
#'
create_output_clip <- function(text, Rcode = F) {
  text <- check_text(text)

  if (Rcode == T) {
    return(paste('word_list <- c("', paste(text, collapse = '", "'), '")', sep = ""))
  } else if (length(text) == 1) {
    return(text)
  } else {
    return(paste(text, collapse = ", "))
  }
}



#' Take a constraint on a category column and expand out to the wide-format constraint
#'
#' @param prefix
#' @param column_suffix
#' @param threshold
#' @param drop
#'
#' @return a single element list mapping the column name to a value
#'
weight_constraint <- function(prefix, column_suffix, threshold, drop = c("ANY", "", NULL)) {
  result <- list()
  for (cols in column_suffix) {
    if (cols %in% drop) {
      return(result)
    }
    column <- glue("{prefix}_{cols}")
    result[column] <- threshold
  }

  return(result)
}



#' Build up there where clause for an SQL query from a set of constraints.
#'
#' @param equals A list that maps from column name to the value that column must equal.
#' @param greater A list that maps from columns names to a value the column must be greater than.
#' @param less A list that maps from column names to a value the column must be less than.
#' @param drop
#'
#' @return A string containing the filters in SQL eg "State=NSW AND wages>1000".
#'
#' @examples
build_query_filter <- function(equals, greater, less, drop = c("ANY", "", NULL)) {
  constraints <- list(equals, greater, less)

  symbols <- c("=", ">=", "<=")

  result <- c()
  for (i in 1:length(symbols)) {
    symbol <- symbols[i]

    # ignore values that don't imply a constraint
    constraint <- discard(constraints[[i]], function(x) sum(x %in% drop) == 1)

    # build up query
    for (name in names(constraint)) {
      value <- constraint[[name]]
      if (symbol == "=") {
        value <- paste("'", value, "'", sep = "")
      }
      qry <- glue("{name}{symbol}{value}")

      result <- append(result, qry)
    }
  }
  constraint_groups <- c(result[grepl("^Contact", result)])

  groups <- c("^cat", "^ind", "^State", "^Industry")

  for (name in groups) {
    group <- result[grepl(name, result)]
    if (length(group) > 0) {
      constraint_groups <- append(constraint_groups, paste("(", paste(group, collapse = " OR "), ")", sep = ""))
    }
  }
  constraint_str <- paste(constraint_groups, collapse = " AND ")

  return(constraint_str)
}



#' Build up column list based on filters applied
#'
#' @param prefix A list of strings of the prefixes for each column type
#' @param cat a list of the strings of lists of strings: contains columns of interest
#' @param all_cols a bool to indicate whether only specific or all fields will be include
#' @param tablename string specifying table to be extracted from, specified in Global.R
#' @param drop
#' @param conn an open DBI/pool ODBC connection
#'
#' @return a string of columns to be inserted in a SQL string
#'
#' @examples
build_columns <- function(prefix = "", cat = "", all_cols = F, tablename = "", drop = c("ANY", "", NULL), conn = "") {
  # print(all_cols)
  new_cols <- ""
  columns <- paste("file_id", "seq_id", "ContactDate",
    "CompanyID", "sentiment",
    "sentiment_score", "nwords", "PricesExtract",
    sep = ", "
  )
  if (all_cols == F) { ## If only showing specific columns ##
    result <- c()
    for (i in 1:length(prefix)) {
      cols <- cat[i]
      prf <- prefix[i]
      # ignore values that don't imply a constraint
      cols <- discard(cols[[1]], function(x) x %in% drop)
      # build up query
      for (name in cols) {
        qry <- glue("{prf}_{name}")
        result <- append(result, qry)
      }
    }
    new_cols <- paste(result, collapse = ", ")
  } else if (all_cols == T) { ## If showing ALL columns ##
    col_names <- dbListFields(conn, tablename)
    ## Extract only the score fields (minus "top" fields)
    regex <- paste0(paste(prefix, collapse = "_|"), "_")
    new_cols <- col_names[grepl(regex, col_names)]
    ## Include "top" fields at the end (due to SQL server requirement)
    regex <- paste0("top_", paste(prefix, collapse = "|top_"))
    top_cols <- col_names[grepl(regex, col_names)]
    new_cols <- append(new_cols, top_cols)
    new_cols <- new_cols[!grepl("score", new_cols)]

    # put is all together
    new_cols <- paste(new_cols, collapse = ", ")
    # print(new_cols)
  }

  if (new_cols == "") {
    all_cols <- paste(columns, "title", "IndustryGroupName", "State", "text", "CompanyName", "last_heading", sep = ", ")
  } else {
    all_cols <- paste(columns, new_cols, "title", "IndustryGroupName", "State", "text", "CompanyName", "last_heading", sep = ", ")
  }
  return(all_cols)
}



#' Cleanup SQL Injection with wildcard step in the following function
#'
#' @param sql a string or SQL query to join to Like statement
#' @param join string that will join current sql string to new LIKE search
#' @param tx text string for LIKE search
#' @param heading_search
#'
#' @return A safe SQL string
#'
sql_clean <- function(query, join, tx, heading_search = FALSE) {
  if (heading_search == FALSE) {
    col <- "text"
  } else {
    col <- "last_heading"
  }

  if (tx == "") {
    query <- paste(as.character(query), join, glue("{col} IS NOT NULL"))
  } else {
    sql <- paste(as.character(query), join, glue("({col} LIKE ?text1)"))
    query <- sqlInterpolate(ANSI(), sql, text1 = glue("%{tx}%"))
  }
  return(query)
}



#'
#'
#' @param words
#' @param inclusive
#'
#' @return A safe SQL string
#'
create_regex_string <- function(words, inclusive = FALSE) {
  if (inclusive == TRUE) {
    # Use str_c to concatenate the words with the regex word boundaries
    regex_string <- str_c("(?=.*\\b", tolower(words), "\\b)", collapse = "")
  } else {
    regex_string <- str_c("\\b", tolower(words), "\\b", collapse = "|")
  }

  # Return the regex string
  return(regex_string)
}



#'
#'
#' @param df
#' @param col description
#' @param words
#' @param inclusive
#'
#' @return A safe SQL string
#'
remove_substrings <- function(df, col, words, inclusive = FALSE) {
  if (all(words == "")) {
    return(df)
  }

  regstr <- create_regex_string(words, inclusive = inclusive)

  df_lower <- df |> mutate(lower = tolower(.data[[col]]))

  df_out <- df[grepl(regstr, df_lower$lower, perl = T), ]

  return(df_out)
}



#' Build up text search SQL query that is safe from SQL injection attacks.
#'
#' @param columns text string containing columns to be included in SELECT statement
#' @param constraints text string containing constraints to be included in WHERE statement
#' @param input_text A string to be inserted into wildcard position for text search
#' @param inclusive
#' @param tablename string specifying table to be extracted from, specified in Global.R
#' @param full_input
#' @param heading_search
#' @param price_extract
#'
#' @return A safe SQL string
#'
#' @examples
build_safe_query <- function(columns = "", constraints = "", tablename = "",
                             input_text = "",heading_text = "",
                             price_extract = FALSE,inclusive = FALSE) {


  ## Start with pre-built filters
  if(constraints == "") {
    query <- paste(
      "SELECT",columns,
      "FROM", tablename)
    filter_start <- TRUE
  } else {
    query <- paste(
      "SELECT",columns,
      "FROM", tablename,
      "WHERE", constraints)
    filter_start <- FALSE
  }

  ## Add text filter
  if (!all(input_text=="")) {

    sql_filt <- if(filter_start) {"WHERE ("} else {"AND ("}
    filter_start <- FALSE

    query <- sql_clean(query, sql_filt, input_text[1])

    if (inclusive & length(input_text) > 1) {
      for (tx in input_text[-1]) {
        query <- sql_clean(query, "AND", tx)
      }
    } else if (length(input_text) > 1) {
      for (tx in input_text[-1]) {
        query <- sql_clean(query, "OR", tx)
      }
    }
    query <- paste(as.character(query), " )", sep = "")
  }

  ## Add price extraction filter
  if (price_extract) {
    sql_filt <- if(filter_start) {"WHERE"} else {"AND"}
    filter_start <- FALSE

    query <- paste(query, sql_filt, "PricesExtract IS NOT NULL")
  }

  ## Add heading filter
  if (!all(heading_text=="")) {

    sql_filt <- if(filter_start) {"WHERE ("} else {"AND ("}
    filter_start <- FALSE

    query <- sql_clean(query, sql_filt, heading_text[1], heading_search = TRUE)

    if (length(heading_text) > 1) {
      for (tx in heading_text[-1]) {
        query <- sql_clean(query, "OR", tx, heading_search = TRUE)
      }
    }
    query <- paste(as.character(query), " )", sep = "")
  }

  if (price_extract | !all(heading_text=="") | !all(input_text=="")) {
    query <- paste(query, "AND category = 'BODY'")
  }

  return(query)

}



#' Summarise data for basic stats or word count
#'
#' @param df a dataframe generated by sql query
#' @param type a string that specifies what columns are being grouped on, also used to define number of plots (1 or 2)
#' @param text a string value that is used when plotting word count (should be same word as word count plot)
#' @param dates description
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
summary_plot <- function(df, type, text = "", dates = "", aggre = "Quarter") {
  if (length(type) == 1 & (all(text == "") | is.null(text))) {
    # This is purely to prevent the app front printing this out at opening
    check <- nrow(df)
    type <- str_to_title(gsub("_", " ", type))
    df <- data.frame()
    p <- ggplot(df) +
      geom_point() +
      xlim(0, 10) +
      ylim(0, 10) +
      annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("No", type, "Selected")) +
      labs(y = "", x = "")
    p <- ggplotly(p)
  } else if (length(text) > 5) {
    ## Printed out when no data exists for wordcount (prevents app from taking too long)
    df_blank <- data.frame()
    p <- ggplot(df_blank) +
      geom_point() +
      xlim(0, 10) +
      ylim(0, 10) +
      annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("Too many words searched, must be 5 or less to see plot")) +
      labs(y = "", x = "")
    p <- ggplotly(p)
  } else if (length(type) == 1) {
    p <- plot_ly() %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          title = "",
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        yaxis = list(
          title = str_to_title(gsub("_", " ", type)),
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        plot_bgcolor = "#e5ecf6"
      )
    for (tx in text) {
      p <- p %>% add_trace(
        data = df[df$word == tx, ] %>% arrange(Contact_Date), x = ~Contact_Date, y = ~ .data[[type[1]]],
        name = tx, text = tx, type = "scatter", mode = "lines",
        hovertemplate = "Word Search: %{text}<extra></extra>"
      )
    }
  } else if (length(type) == 2) {
    df_sum <- df %>%
      mutate(Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1) %>%
      group_by(Contact_Date) %>%
      summarise(
        Count = n(),
        `Distinct Notes` = n_distinct(file_id),
        `Net Sentiment` = sum(sentiment, na.rm = T),
        `Mean Sentiment` = safe_mean(sentiment_score)
      ) %>%
      as.data.frame()
    if (nrow(df_sum) == 0) {
        df <- data.frame()
        p <- ggplot(df) +
            geom_point() +
            xlim(0, 10) +
            ylim(0, 10) +
            annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("No results returned, try expanding your search.")) +
            labs(y = "", x = "")
    } else if (nrow(df_sum) < 3) {
      df <- data.frame()
      p <- ggplot(df) +
        geom_point() +
        xlim(0, 10) +
        ylim(0, 10) +
        annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("Too few results to plot, see other pages for data.")) +
        labs(y = "", x = "")
    } else {
      p1 <- ggplot(df_sum, aes(x = Contact_Date, y = .data[[type[1]]])) +
        geom_line() +
        xlab("") +
        ylab("Number of Paragraphs") + 
        xlim(c(ymd(dates[1]), ymd(dates[2])))

      fig1 <- plotly_build(p1)

      p2 <- ggplot(df_sum, aes(x = Contact_Date, y = .data[[type[2]]])) +
        geom_line() +
        xlab("") +
        ylab("Mean Paragraph Sentiment") +
        xlim(c(ymd(dates[1]), ymd(dates[2])))

      fig2 <- plotly_build(p2)

      p <- subplot(fig1, fig2,
        nrows = 2,
        titleY = TRUE,
        shareX = TRUE,
        margin = 0.01
      ) %>%
        layout(
          hovermode = "x unified",
          plot_bgcolor = "#e5ecf6",
          showlegend = FALSE
        )
    }
  } else {
    print(paste("Cannot plot", length(type), "number of graphs"))
    p <- NULL
  }
  return(p)
}



#' Plot aggregated data
#'
#' @param df dataframe containing aggregated data
#' @param type a string, the prefix of the columns that are being plotted
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
aggregate_plot <- function(df, type, aggre = "Quarter") {
  name <- case_when(type == "cat" ~ "Category", type == "ind" ~ "Industry", TRUE ~ "Other")

  if (nrow(df) == 0) {
    df <- data.frame()
    p <- ggplot(df) +
      geom_point() +
      xlim(0, 10) +
      ylim(0, 10) +
      annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("No", name, "Selected")) +
      labs(y = "", x = "")
    p <- ggplotly(p)
  } else {

    p <- plot_ly() %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          title = "",
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        yaxis = list(
          title = "Share of Paragraphs (%)",
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        plot_bgcolor = "#e5ecf6"
      )

    zsc_set <- df["col_type"] |> distinct() |> unlist()
    for (tx in zsc_set) {
      p <- p %>% add_trace(
        data = df[df$col_type == tx, ] %>% arrange(Contact_Date), x = ~Contact_Date, y = ~ Paragraph_share,
        name = tx, text = tx, type = "scatter", mode = "lines",
        hovertemplate = paste(name, "%{text}<extra></extra>", sep = ": ")
      )
    }

  }
  return(p)
}



#' Aggregate data
#'
#' @param conn an open DBI/pool ODBC connection
#' @param query a sql query generated with build_safe_query function
#' @param col a string, a column to aggregated data on
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#'
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
get_aggregate_data <- function(col, query, conn, aggre, thresh) {
  
  group_date <- create_grouped_date(aggre)
  
  sql <- sprintf("SELECT Contact_Date,
              '%s' as col_type,
              Count(%s) as Para_count
              FROM (SELECT %s AS Contact_Date, %s
                  FROM (%s) a
                WHERE %s > %s) a
              GROUP BY Contact_Date", col, col, group_date, col, as.character(query), col, thresh)
  ## Send sql string to database
  df <- dbGetQuery(mydb, sql)
  df <- df %>%
    mutate(Contact_Date = ceiling_date(ymd(as.Date(Contact_Date)), unit = str_to_lower(aggre)) - 1) %>%
    arrange(Contact_Date)
  return(df)
}



#' Plot aggregated data
#'
#' @param query a sql query generated with build_safe_query function
#' @param columns a string of columns as used for as sql query, as built by build_columns function
#' @param type a string, the prefix of the columns that are being aggregated
#' @param conn an open DBI/pool ODBC connection
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
aggregate_data <- function(query, columns, type, conn, aggre = "Quarter", thresh) {
  prefix <- paste0(type, "_")
  
  columns <- str_split(columns, pattern = ",")[[1]]
  columns <- trimws(columns)
  columns <- columns[grepl(paste0("^", prefix), columns, perl = TRUE)]
  
  output <- data.frame()
  if (length(columns) > 0) {
    for (col in columns) {
      df <- get_aggregate_data(col, query, conn, aggre, thresh)
      
      output <- rbind(output, df)
    }
    # output <- pivot_wider(output,'Contact_Date',names_from = 'col_type',values_from = 'ave_weight')
  }
  
  return(output)
}



#' Create binned date for ContactDate based on type of aggregation selected
#'
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a string that will generate binning for ContactDate
#'
create_grouped_date <- function(aggre) {
  
  if (str_to_lower(aggre) == 'month') {
    
    group_date <- "date(ContactDate, 'start of month', '+1 month', '-1 day')"
    
  } else if (str_to_lower(aggre) == 'quarter') {
    
    group_date <- "CASE
    WHEN strftime('%m', ContactDate) IN ('01', '02', '03') THEN strftime('%Y-03-31', ContactDate)
    WHEN strftime('%m', ContactDate) IN ('04', '05', '06') THEN strftime('%Y-06-30', ContactDate)
    WHEN strftime('%m', ContactDate) IN ('07', '08', '09') THEN strftime('%Y-09-30', ContactDate)
    WHEN strftime('%m', ContactDate) IN ('10', '11', '12') THEN strftime('%Y-12-31', ContactDate) 
    END"
    
  } else if (str_to_lower(aggre) == 'year') {
    
    group_date <- "strftime('%Y-12-31', ContactDate)"
    
  }
  
  return(group_date)
  
}



#' Get widely used summary data
#'
#' @param conn an open DBI/pool ODBC connection
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#'
#' @return a table with summarised data at the input aggregation period
#'
#' @examples
get_summary_data <- function(conn, table, aggre) {
  
  group_date <- create_grouped_date(aggre)
  
  sql <- sprintf("SELECT Contact_Date,
              sum(nwords) as Word_count,
              Count(file_id) as Tot_para_count
              FROM (SELECT %s AS Contact_Date, *
                  FROM %s a) a
              GROUP BY Contact_Date", group_date, table)
  ## Send sql string to database
  df <- dbGetQuery(mydb, sql)
  df <- df %>%
    mutate(Contact_Date = ceiling_date(ymd(as.Date(Contact_Date)), unit = str_to_lower(aggre)) - 1) %>%
    arrange(Contact_Date)
  return(df)
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



#' Aggregate data
#'
#' @param df a dataframe containing ContactDate and PricesExtract columns
#' @param query a sql query generated
#' @param col a string, a column to aggregated data on
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#'
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
price_series <- function(df, aggre) {

  if (nrow(df[!is.na(df$PricesExtract),]) == 0){
    return(data.frame(Contact_Date="",price_rate_ext=0))
  }

  # Group by 'y' and calculate the statistics
  df_price <- df |>
    mutate(Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1,
                      y = ceiling_date(ymd(as.Date(ContactDate)), unit = "year") - 1)

  df_stats <- df_price |>
    filter(!is.na(PricesExtract)) |>
    summarise(
      median = median(PricesExtract),
      std = sd(PricesExtract),
      max = max(PricesExtract),
      min = min(PricesExtract),
      q10 = q10(PricesExtract),
      q90 = q90(PricesExtract),
      .by = "y"
    )

  # Merge the original dataframe with the statistics dataframe
  df_p <- left_join(df_price, df_stats, by = "y")

  # Filter out the outliers
  df_p <- df_p %>%
    filter(!(PricesExtract > q90 | PricesExtract < q10))

  # Group by 'q' and 'LiaisonID', calculate the mean, then group by 'q' and calculate the mean again
  extracted <- df_p %>%
    select(Contact_Date, PricesExtract, file_id) %>%
    summarise(price_rate_ext = safe_mean(PricesExtract), .by = c("Contact_Date", "file_id")) %>%
    summarise(
      price_rate_ext = safe_mean(price_rate_ext),
      number_extracts = n(),
      .by = "Contact_Date"
    ) |>
    filter(Contact_Date > ymd("2006-12-31")) |>
    arrange(Contact_Date)

  if (nrow(extracted) == 0){
    return(data.frame(Contact_Date="",price_rate_ext=0))
  }

  last_val <- extracted[nrow(extracted), "Contact_Date"]

  if (last_val > ymd(Sys.Date())) {
    extracted <- extracted[1:(nrow(extracted) - 1), ]
  }

  return(extracted)
}



#' Plot aggregated data
#'
#' @param df dataframe containing aggregated data
#' @param type a string, the prefix of the columns that are being plotted
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a plot object of aggregated data or blank if no columns selected
#'
#' @examples
price_plot <- function(df, aggre = "Quarter") {
  if (nrow(df) < 5) {
    df <- data.frame()
    p <- ggplot(df) +
      geom_point() +
      xlim(0, 10) +
      ylim(0, 10) +
      annotate("text", x = 5, y = 5, size = 6, col = "blue", label = paste("Too few price extractions to plot")) +
      labs(y = "", x = "")
    p <- ggplotly(p)
  } else {

    p <- plot_ly() %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          title = "",
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        yaxis = list(
          title = "Price Change (%)",
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        plot_bgcolor = "#e5ecf6"
      )

    p <- p %>% add_trace(
        data = df, x = ~Contact_Date, y = ~ price_rate_ext,
         type = "scatter", mode = "lines")
  }
  return(p)
}



#' Build up safe word count query and run query on database.
#'
#' @param constraints text string containing constraints to be included in WHERE statement
#' @param input_text A string to be inserted into wildcard position for text search
#' @param tablename string specifying table to be extracted from, specified in Global.R
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a dataframe with word counts over time
#'
#' @examples
build_safe_wc_query <- function(constraints, input_text, tablename, aggre = "Quarter") {
  
  group_date <- create_grouped_date(aggre)
  
  columns <- sprintf("%s AS Contact_Date,
                    nwords,
                    file_id,
                    CASE WHEN text GLOB ?text1 THEN 1
                     WHEN text GLOB  ?text2 THEN 1
                     WHEN text GLOB  ?text3 THEN 1
                     ELSE 0 END AS search_word_count", group_date)

  ## build final sql string
  if (constraints == "") {
    sql <- paste(
      "SELECT",
      columns,
      "FROM", tablename
    )
  } else {
    sql <- paste(
      "SELECT",
      columns,
      "FROM", tablename, "WHERE",
      constraints
    )
  }
  ## Apply SQL Interpolation
  query <- sqlInterpolate(ANSI(), sql,
    text1 = glue("*[^0-9a-z]{input_text}[^0-9a-z]*"),
    text2 = glue("{input_text}[^0-9a-z]*"),
    text3 = glue("*[^0-9a-z]{input_text}")
  )

  query <- paste("SELECT Contact_Date,
        sum(nwords) as Tot_Word_Count,
        sum(search_word_count) as search_word_count,
        count(file_id) as Tot_Para_Count,
        count(distinct file_id) as Tot_Liaison_Count
        FROM (", query, ") a
        GROUP BY Contact_Date")

  return(query)
}



#' Build up safe word count query and run query on database.
#'
#' @param df a data frame containing time series data
#' @param df.weights
#' @param df.text
#' @param input_text
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period, default is Quarter
#'
#' @return a dataframe with summarized data grouped to quarter
#'
#' @examples
summarise_data <- function(df, df.weights, df.text, input_text = "", aggre = "Quarter") {#, likerts = "") {

  if (nrow(df) == 0){
    return(data.frame(Contact_Date=""))
  }

  df_sum <- df %>%
    mutate(
      Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1,
      sentiment_score = na.fill(sentiment_score, fill = 0)
    ) %>%
    group_by(Contact_Date) %>%
    summarise(
      Para_Count = n(),
      Liaison_Count = n_distinct(file_id),
      #Mean_Paragraph_Sentiment = safe_mean(sentiment_score),
      Tot_Word_Count = sum(nwords)
    ) #%>%
    #mutate(Mean_Paragraph_Sentiment = round(Mean_Paragraph_Sentiment, 3))

  df_liaison_sum <- df %>%
    mutate(
      Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1,
      sentiment_score = na.fill(sentiment_score, fill = 0)
    ) %>%
    group_by(file_id) %>%
    summarise(
      Contact_Date = safe_max(Contact_Date),
      sentiment_score = safe_mean(sentiment_score)
    ) %>%
    group_by(Contact_Date) %>%
    summarise(Mean_Liaison_Sentiment = safe_mean(sentiment_score)) %>%
    mutate(Mean_Liaison_Sentiment = round(Mean_Liaison_Sentiment, 3))

  df_sum <- left_join(df_sum, df_liaison_sum, by = "Contact_Date")

  if (!all(input_text == "")) {
    df_text <- word_count(df, input_text, aggre)
    df_sum <- left_join(df_sum, df_text, by = "Contact_Date")
  }

  ## aggregates zero-shot models output
  if (nrow(df.weights$cat) > 0) {
    dfw.cat <- df.weights$cat %>% mutate(Paragraph_share = round(Paragraph_share, 3))
    dfw.cat <- pivot_wider(dfw.cat, id_cols = "Contact_Date", names_from = "col_type", values_from = "Paragraph_share")
    df_sum <- left_join(df_sum, dfw.cat, by = "Contact_Date")
  }
  if (nrow(df.weights$ind) > 0) {
    dfw.ind <- df.weights$ind %>% mutate(Paragraph_share = round(Paragraph_share, 3))
    dfw.ind <- pivot_wider(dfw.ind, id_cols = "Contact_Date", names_from = "col_type", values_from = "Paragraph_share")
    df_sum <- left_join(df_sum, dfw.ind, by = "Contact_Date")
  }

  return(df_sum)
}



#' Build up safe word count query and run query on database.
#'
#' @param df a data frame containing liaison text data
#' @param terms string containing words to search text data (will be split if multiple words)
#' @param aggre a string, either Quarter, Month or Year, to be used as aggregation period
#'
#' @return a dataframe with summarized data grouped to quarter
#'
word_count <- function(df, words, aggre) {
  df_out <- data.frame()

  #### Create count columns
  # for each term in uncertainty words, create a new column containing the count of that word in each paragraph.
  for (tx in words) {
    df_agg <- df |>
      mutate(
        Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(aggre)) - 1,
        word_count = str_count(text, regex(tx, ignore_case = TRUE))
      ) |>
      summarise(
        word_count = sum(word_count),
        .by = Contact_Date
      ) |>
      mutate(word = str_replace(as.character(tx), " ", "_"))

    df_out <- rbind(df_out, df_agg)
  }

  df_out <- df_out |>
    pivot_wider(id_cols = Contact_Date, values_from = word_count, names_from = word, names_prefix = "Count_") %>%
    mutate(Combine_Word_Count = rowSums(select(., starts_with("Count_"))))

  return(df_out)
}



#' filter_builder
#'
#' iterate through list of columns to create filter parameter for queryBuilder function
#'
#' @param filter_list i list of pre-created filter parameters from queryBuiler
#' @param col_name string of column name to filter on
#'
#' @return filter list with new column added
#'
filter_builder <- function(filter_list, col_name) {
  add_col <- list(name = col_name, type = "double", default_value = 0.9)

  # print(add_col)

  filter_list <- append(filter_list, list(add_col))

  return(filter_list)
}



#' Load in word2vec model.
#'
#' @param file File path to directory containing saved word2vec model.
#'
#' @return word2vec model
#'
#' @keywords internal
get_w2v <- function(file = "w2v.bin") {
  model <- read.word2vec(file)
  embed <- as.matrix(model)
  w2v <- list(model = model, emb = embed, words = rownames(embed))
}



#' Predict the n nearest terms to a vector of terms using w2v model.
#' Given terms, calculates the element-wise average of those terms
#' that exist in the word2vec model. Then, the n nearest terms to that average
#' vector is calculated. Any terms which were not in the input terms are
#' returned.
#'
#' @param terms vector of terms to find words near to
#' @param w2v word2vec model object
#' @param n number of terms to return.
#'
#' @return vector of words/terms predicted by the model.
#'
#' @keywords internal

w2v_pred <- function(terms, drop_terms, w2v, n = 5) {
  
  terms <- terms |> tolower()
  terms_ix <- which(terms %in% w2v$words)
  
  if (all(!terms_ix)) { # If no terms, return empty vector
    return(c())
  }
  
  embed <- w2v$emb
  terms_embed <- lapply(terms[terms_ix], function(term) embed[term, ])
  mean <- Reduce("+", terms_embed) / length(terms_embed)
  predictions <- predict(w2v$model, mean, type = "nearest", top_n = n + length(terms_embed) + length(drop_terms))
  predictions <- predictions[which(!(predictions$term %in% terms | predictions$term %in% drop_terms)), ]
  predictions$term
  
}

