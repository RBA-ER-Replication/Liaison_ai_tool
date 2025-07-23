########################################################################
# server ---------------------------------------------------------------
########################################################################

server <- function(input, output, session) {

  #-------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------- Simple Filtering ---------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------------------
  ### Build Datasets ---------------------------
  datasetInput <- eventReactive(input$suppdate, {
    # --- Build SQL compatible parameters
    parameters <- list(
      category = check_input(input$sup_category),
      industry = check_input(input$sup_industry),
      heading = check_text(input$heading_text),
      text = check_text(input$sup_text),
      industrygrp = check_input(input$sup_industrygrp, TRUE),
      state = check_input(input$sup_state)
    )
    date_range <- ymd(input$sup_daterange[2]) - ymd(input$sup_daterange[1])
    # Check if any filters applied before proceeding
    req((!all(parameters == "" | parameters == "ANY")) | input$price_extract == TRUE | date_range < 366 * 3)
    # also build date filter
    parameters$start_date <- as.character(input$sup_daterange[1])
    parameters$end_date <- as.character(input$sup_daterange[2])

    # --- Set the contraints
    greater <- c(
      list("date(ContactDate)" = parameters$start_date),
      weight_constraint("cat", parameters$category, input$sup_cat_threshold),
      weight_constraint("ind", parameters$industry, input$sup_industry_threshold)
    )
    less <- list("date(ContactDate)" = parameters$end_date)
    equal <- list(
      "IndustryGroupName" = parameters$industrygrp,
      "State" = parameters$state
    )
    # --- Build SQL Query
    # build constraints
    constraint <- build_query_filter(equal, greater, less)
    # build columns list
    columns <- build_columns(
      list("cat", "ind"),
      list(parameters$category, parameters$industry),
      FALSE, # input$supcols,
      tablename = DSH_tablename,
      conn = conn
    )
    ## Combine each element of sql string and
    ## Ensure query is safe from sql injection attacks
    query <<- build_safe_query(columns, constraint, DSH_tablename,
      input_text = parameters$text,
      heading_text = parameters$heading,
      price_extract = input$price_extract,
      inclusive = input$supinclusive
    )
    ## Send sql string to database
    df <- dbGetQuery(mydb, query)
    ## remove cases of substrings extracted rather than whole words (e.g. keep oil, remove bOILer)
    df <- df |>
      remove_substrings("text", parameters$text, input$supinclusive) |>
      remove_substrings("last_heading", parameters$heading, FALSE)
    ## Clean data
    df_numeric <- df %>%
      select(matches("^cat_|^ind_", perl = TRUE)) %>%
      round(4)
    df_other <- df %>%
      select(-matches("^cat_|^ind_", perl = TRUE)) %>%
      mutate(
        sentiment_score = round(sentiment_score, 4),
        ContactDate = lubridate::ymd(as.Date(ContactDate))
      )
    ## Make prettier output format
    df_out <- cbind(df_other, df_numeric) |>
      select(file_id,
        title,
        para_id = seq_id,
        ContactDate,
        CompanyName,
        text,
        last_heading,
        IndustryGroupName,
        State,
        sentiment,
        sentiment_score,
        nwords,
        PricesExtract,
        matches("^cat_|^ind_", perl = TRUE)
      ) |>
      filter(text != "")

    df_out
  })



  ### Build Agg plots ----------------------------
  datasetAggregate <- eventReactive(input$suppdate, {
    # --- Build SQL compatible parameters
    data <- list()
    parameters <- list(
      category = check_input(input$sup_category),
      industry = check_input(input$sup_industry)
    )
    # check if any relevant filters set, else return empty dataframe
    if (!all(parameters == "" | parameters == "ANY")) {
      # --- Set the contraints
      greater <- c("ANY" = 0)
      less <- c("ANY" = 0)
      equal <- list("IndustryGroupName" = "", "State" = "")
      # --- Build SQL Query
      ## build constraints
      constraint <- build_query_filter(equal, greater, less)
      ## build columns
      columns <- build_columns(
        prefix = list("cat", "ind"),
        cat = list(parameters$category, parameters$industry),
        tablename = DSH_tablename,
        conn = conn
      )
      ## --- Build query
      query <- build_safe_query(columns, constraint, DSH_tablename)
      ## --- Make Plots
      data$cat <- aggregate_data(query, columns = columns, "cat", conn, input$aggr, input$sup_cat_threshold) 
      if (nrow(data$cat) > 0) {
        data$cat <- data$cat |>
        left_join(agg_data[[tolower(input$aggr)]], by = "Contact_Date") |>
        mutate(Paragraph_share = Para_count/Tot_para_count)
      }
      data$ind <- aggregate_data(query, columns = columns, "ind", conn, input$aggr, input$sup_industry_threshold)
      if (nrow(data$ind) > 0) {
        data$ind <- data$ind |>
          left_join(agg_data[[tolower(input$aggr)]], by = "Contact_Date") |>
          mutate(Paragraph_share = Para_count/Tot_para_count)
      }
      
    } else {
      data <- list(cat = data.frame(), ind = data.frame())
    }
    return(data)
  })



  ### Build word count time series ----------------------------
  datasetWordCount <- eventReactive(input$suppdate, {
    # --- Build SQL compatible parameters
    parameters <- list(
      text = check_text(input$sup_text)
    )
    # --- Build SQL Query
    ## build constraints (there are none)
    constraint <- ""
    ## build columns
    columns <- "ContactDate, nwords, file_id, text"
    # check if any relevant filters set, else return empty dataframe
    if (length(parameters$text) <= 5 & !all(parameters$text == "")) {
      df_out <- data.frame()
      # build query, first without text filter
      query <- build_safe_query(columns, constraint, DSH_tablename,
        input_text = parameters$text
      )
      ## Send sql string to database
      df <- dbGetQuery(mydb, query)
      # Add each text filter to a query, then run and save count over time
      for (tx in parameters$text) {
        ## remove cases of substrings, then aggregate over selected period
        df_sub <- df |>
          remove_substrings("text", tx, FALSE) |>
          mutate(
            Contact_Date = ceiling_date(ymd(as.Date(ContactDate)), unit = str_to_lower(input$aggr)) - 1,
            word_count = str_count(text, regex(tx, ignore_case = TRUE))
          ) |>
          summarise(
            word_count = sum(word_count),
            nwords = sum(nwords),
            Tot_Para_Count = n(),
            Tot_Liaison_Count = n_distinct(as.character(file_id)),
            .by = Contact_Date
          ) |>
          mutate(
            word_frequency = case_when(word_count == 0 ~ 0, TRUE ~ word_count / nwords),
            word = as.character(tx)
          )
        # Add to final dataframe
        df_out <- rbind(df_out, df_sub)
      }
      rm(df)
    } else {
      df_out <- data.frame()
    }
    df_out
  })

  ### Generate suggested similar words -------
  observeEvent(input$sup_text, {
    predictions <- w2v_pred(input$sup_text, "",w2v_mod)
    output$sup_suggested_phrases <- renderText(sprintf("Suggested Words: %s", paste0(predictions, collapse = ", ")))
  })

  ### Generate Price Extract ------
  datasetPricesExtract <- reactive({
    # Generate series from simple Filter dataset
    price_series(datasetInput(), input$aggr)
  })


  #----------------------------  Simple Filtering OUTPUTS  ----------------------------
  ### Plots ----------------------------
  # Top summary plot of filtered output
  output$countplot <- bindEvent(renderPlotly({
    # if no data, output empty plot
    if (input$suppdate == 0) {
      suppressWarnings(summary_plot("", "Data"))
    } else {
      p <- suppressWarnings(summary_plot(datasetInput(),
        type = c("Count", "Mean Sentiment"),
        text = "",
        dates = input$sup_daterange,
        aggre = input$aggr
      ))
      p
    }
  }), datasetInput())
  # Category NLP (independent of other filters) plot
  output$catplot <- bindEvent(renderPlotly({
    # if no data, output empty plot
    if (input$suppdate == 0) {
      suppressWarnings(summary_plot("", "Category"))
    } else {
      df <- datasetAggregate()
      p <- aggregate_plot(df$cat, "cat", aggre = input$aggr)
      p
    }
  }), datasetAggregate())
  # Industry NLP (independent of other filters) plot
  output$indplot <- bindEvent(renderPlotly({
    # if no data, output empty plot
    if (input$suppdate == 0) {
      suppressWarnings(summary_plot("", "Industry"))
    } else {
      df <- datasetAggregate()
      p <- aggregate_plot(df$ind, "ind", aggre = input$aggr)
      p
    }
  }), datasetAggregate())
  # Word counts (independent of other filters) plot
  output$wcplot <- bindEvent(renderPlotly({
    # if no data, output empty plot
    if (input$suppdate == 0) {
      suppressWarnings(summary_plot("", "Word"))
    } else {
      p <- summary_plot(datasetWordCount(), c("word_count"), text = check_text(input$sup_text), input$aggr)
      p
    }
  }), datasetWordCount())
  # Price Extraction Plot
  output$peplot <- bindEvent(renderPlotly({
    # if no data, output empty plot
    if (input$suppdate == 0) {
      suppressWarnings(price_plot(data.frame()))
    } else {
      p <- price_plot(datasetPricesExtract(), input$aggr)
      p
    }
  }), datasetPricesExtract())

  ### Data table ----------------------------
  ## Aggregated Data ##
  output$aggtable <- renderDT(datatable(
    {
      summarise_data(datasetInput(), datasetAggregate(), datasetWordCount(), check_text(input$sup_text), input$aggr)#, input$likerts)
    },
    ## data.table UI options
    options = list(
      autoWidth = TRUE,
      dom = "",
      pageLength = 500
    ),
    rownames = FALSE,
    selection = "none",
    escape = FALSE
  ) %>%
    formatStyle(columns = c("Contact_Date"), fontWeight = "Bold"))
  ## Download Data
  output$aggdownload <- downloadHandler(
    filename = function() {
      paste("RIA_dashboard_AGG_data_", gsub("-", "_", Sys.Date()), "_", sample(1, x = c(1000:9999)), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(
        {
          summarise_data(datasetInput(), datasetAggregate(), datasetWordCount(), check_text(input$sup_text), input$aggr)#, input$likerts)
        },
        path = file
      )
    }
  )

  ## Raw Data by paragraph ##
  output$suptable <- renderDT(datatable(
    {
      datasetInput() %>%
        mutate(
          file_id = paste("<a href='", "https://trimweb.rba.gov.au/record/", file_id, "' target='_blank'>", file_id, "</a>", sep = ""),
          text = highlight_words(check_text(input$sup_text), text)
        ) %>%
        select(-c("title", "nwords")) %>%
        arrange(desc(ContactDate), CompanyName, para_id)
    },
    ## data.table UI options
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 800 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 800) + '...</span>' : data;",
          "}"
        ),
        width = "500px",
        targets = c(4)
      )),
      pageLength = 50, lengthMenu = c(10, 50, 100, 200, 500)
    ),
    rownames = FALSE,
    selection = "none",
    escape = FALSE
  ))
  ## Download Raw Data by paragraph
  output$supdownload <- downloadHandler(
    filename = function() {
      paste("RIA_dashboard_data_", gsub("-", "_", Sys.Date()), "_", sample(1, x = c(1000:9999)), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(
        datasetInput() %>%
          mutate(file_id = xl_hyperlink(paste("https://trimweb.rba.gov.au/record/", file_id, sep = ""))) %>%
          arrange(desc(ContactDate), CompanyName, para_id),
        path = file
      )
    }
  )

  ## Raw Data by Liaison ##
  output$suplsntable <- renderDT(datatable(
    {
      suppressWarnings(datasetInput() |>
            mutate(
                file_id = paste("<a href='", "https://trimweb.rba.gov.au/record/", file_id, "' target='_blank'>", file_id, "</a>", sep = ""),
                text = highlight_words(check_text(input$sup_text), text)
            ) |>
            group_by(file_id) |>
            arrange(para_id) |>
            summarise(
                ContactDate = safe_max(ContactDate),
                CompanyName = safe_max(CompanyName),
                text = paste("<ul>
                             <li>", paste0(text, collapse = "</li>
                                          <li>"), "</li>
                             </ul>"),
                included_headings = paste0(unique(last_heading), collapse = ", "),
                IndustryGroupName = safe_max(IndustryGroupName),
                State = safe_max(State),
                PricesExtract = round(safe_mean(PricesExtract), 2)
                ) |>
            arrange(desc(ContactDate)))
    },
    ## data.table UI options
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = "700px",
        targets = c(3)
      )),
      pageLength = 50, lengthMenu = c(10, 50, 100, 200, 500)
    ),
    rownames = FALSE,
    selection = "none",
    escape = FALSE
  ))
  ## Download Raw Data by Liaison
  output$suplsndownload <- downloadHandler(
    filename = function() {
      paste("RIA_dashboard_data_byliaison_", gsub("-", "_", Sys.Date()), "_", sample(1, x = c(1000:9999)), ".xlsx", sep = "")
    },
    content = function(file) {
      suppressWarnings(write_xlsx(
        datasetInput() %>%
          mutate(file_id = xl_hyperlink(paste("https://trimweb.rba.gov.au/record/", file_id, sep = ""))) %>%
          group_by(file_id) %>%
          arrange(para_id) %>%
          summarise(
            ContactDate = max(ContactDate, na.rm = T),
            CompanyName = max(CompanyName, na.rm = T),
            text = paste0("* ", paste0(text, collapse = "
* ")),
            included_headings = paste0(unique(last_heading), collapse = ", "),
            IndustryGroupName = max(IndustryGroupName, na.rm = T),
            State = max(State, na.rm = T),
            PricesExtract = round(safe_mean(PricesExtract), 2)
          ) %>%
          arrange(desc(ContactDate)),
        path = file
      ))
    }
  )

  ## Price Extraction ##
  output$price_ext_tbl <- renderDT(datatable(
    {
      datasetPricesExtract() |> mutate(price_rate_ext = round(price_rate_ext, 3))
    },
    ## data.table UI options
    options = list(
      dom = "",
      pageLength = 500
    ),
    rownames = FALSE,
    selection = "none",
    escape = FALSE
  ) %>%
    formatStyle(columns = c("Contact_Date"), fontWeight = "Bold"))
  ## Download Price Extract Data
  output$price_ext_dl <- downloadHandler(
    filename = function() {
      paste("RIA_dashboard_price_extraction_", gsub("-", "_", Sys.Date()), "_", sample(1, x = c(1000:9999)), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(
        {
          datasetPricesExtract() |> mutate(price_rate_ext = round(price_rate_ext, 6))
        },
        path = file
      )
    }
  )



  #------------------------------------------------------------------------------------------------------------
  # -------------------------------------------- Topic Builder ------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  ## --- Build Dataset
  topbuilddataset <- eventReactive(input$tbupdate, {
    # check if exactly one word is being searched before proceeding
    req(!all(check_text(input$tb_text) == "") & length(check_text(input$tb_text)) < 2)
    # --- Build SQL compatible parameters
    ## build columns (only need default)
    columns <- build_columns()
    ## Combine each element of sql string and
    ## Ensure query is safe from sql injection attacks
    query <- build_safe_query(columns,
      tablename = DSH_tablename,
      input_text = check_text(input$tb_text)
    )
    ## Send sql string to database
    df <- dbGetQuery(mydb, query)
    ## Remove substring cases, e.g. dont include oil in bOILer as search
    df <- df |>
      remove_substrings("text", check_text(input$tb_text), FALSE)
    ## Final prettier Output
    df <- df %>%
      select(
        file_id,
        para_id = seq_id,
        ContactDate,
        CompanyName,
        text,
        last_heading,
        IndustryGroupName,
        State
      ) %>%
      mutate(ContactDate = lubridate::ymd(as.Date(ContactDate))) |>
      filter(text != "")
    ## order randomly, return:
    df[sample(nrow(df)), ]
  })

  ## find similar words based on word or wordlist entered in topic builder
  observeEvent(input$phrases, {
    predictions <- w2v_pred(input$phrases, input$phrases_drop, w2v_mod)
    output$suggested_phrases <- renderText(sprintf("Suggested Words: %s", paste0(predictions, collapse = ", ")))
  })
  
  observeEvent(input$phrases_drop, {
    predictions <- w2v_pred(input$phrases, input$phrases_drop, w2v_mod)
    output$suggested_phrases <- renderText(sprintf("Suggested Words: %s", paste0(predictions, collapse = ", ")))
  })

  ### ------- Topic Builder OUTPUT -------
  ## table output in dashboard
  output$tbtable <- renderDT(datatable(
    {
      topbuilddataset() %>%
        mutate(
          file_id = paste("<a href='", "https://trimweb.rba.gov.au/record/", file_id, "' target='_blank'>", file_id, "</a>", sep = ""),
          ContactDate = lubridate::ymd(as.Date(ContactDate)),
          text = highlight_words(check_text(input$tb_text), text)
        )
    },
    ## data.table UI options
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 800 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 800) + '...</span>' : data;",
          "}"
        ),
        width = "800px",
        targets = c(4)
      )),
      pageLength = 50, lengthMenu = c(10, 50, 100, 200, 500)
    ),
    rownames = FALSE,
    selection = "none",
    escape = FALSE
  ))

  ## generate suggested similar words list
  output$txtphrasesList <- renderPrint({
    paste(input$phrases, collapse = ", ")
  })

  ## Create clipboard button for copying word list (for dashboard)
  output$clip <- renderUI({
    ## function for generating clean list
    clippy <- create_output_clip(input$phrases)
    ## clipboard object for copying
    rclipButton(
      inputId = "clipbtn",
      label = " Copy for Dashboard",
      clipText = clippy,
      icon = icon("clipboard"),
      tooltip = "Use this to copy word list for use in liaison dashboard paragraph search",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })

  ## generate nice R code word list
  output$txtphrasesR <- renderPrint({
    create_output_clip(input$phrases, Rcode = T)
  })

  ## Create clipboard button for copying word list (for dashboard)
  output$clipR <- renderUI({
    ## function for generating clean list
    clippy <- create_output_clip(input$phrases, Rcode = T)
    ## clipboard object for copying
    rclipButton(
      inputId = "clipbtn",
      label = " Copy for R",
      clipText = clippy,
      icon = icon("clipboard"),
      tooltip = "Use this to copy word list for use in R (creates a vector of words)",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })

  ## generate warning text if trying to search more than 1 word in topic builder "Word Examples" section
  observeEvent(input$tb_text, {
    ## more than one word, generate text, else no action
    if (length(check_text(input$tb_text)) > 1) {
      output$tb_warning <- renderText("Warning: Only search single words to see examples!")
    } else {
      output$tb_warning <- renderText("")
    }
  })


  ### End of server object
}
