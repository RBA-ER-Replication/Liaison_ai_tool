#'
#' @title  Section 5: Nowcasting Wages Growth - Function file
#'
#' @description Utility functions used in fit_predict.R and analysis.R
#'
#' @author Nicholas Gray



#' Impute Missing Values in Data Frames
#'
#' This function imputes missing values in the 'ins' and 'oos' data frames within a given list.
#' Missing values in the 'value' column are replaced with 0.
#'
#' @param df_window A list containing two data frames: 'ins' and 'oos'.
#' @return A list with the 'ins' and 'oos' data frames, with missing values in the 'value' column imputed.
#' @examples
#' df_window <- list(ins = data.frame(value = c(1, NA, 3)), oos = data.frame(value = c(NA, 2, 3)))
#' imputed <- impute_missing(df_window)
#' print(imputed)
#' @export
impute_missing <- function(df_window) {
  # in-sample data
  ins <- df_window$ins |>
    mutate(value = coalesce(value, 0))
  # out-of-sample data
  oos <- df_window$oos |>
    mutate(value = coalesce(value, 0))
  return(list("ins" = ins, "oos" = oos))
}



#' Function to scale the 'out of sample' data using the mean and standard deviation of the 'in sample' data
#'
#' @input oos: a dataframe with 'out of sample' data
#' @input ins: a dataframe with 'in sample' data
#' @input col_fil: a regex string for selecting series (using grepl)
#'
#' @return a dataframe with the scaled 'out of sample' data
#'
scale_apply <- function(oos, ins, col_fil) {
  col_fil <- list_to_regex(col_fil)

  ## cache data that doesn't need scaling
  non_scale_df <- oos |>
    filter(!grepl(col_fil, Series, perl = T))

  # Calculate the mean and standard deviation of the 'in sample' data
  ins_scale <- ins |>
    filter(grepl(col_fil, Series, perl = T)) |>
    dplyr::summarise(
      mean = mean(value, na.rm = T),
      std = sd(value, na.rm = T), .by = "Series"
    )

  # Scale the 'out of sample' data using the mean and standard deviation of the 'in sample' data
  oos_scaled <- oos |>
    filter(grepl(col_fil, Series, perl = T)) |>
    group_by(Series) |>
    mutate(value = (value - ins_scale$mean[ins_scale == as.character(Series)]) / ins_scale$std[ins_scale == as.character(Series)]) |>
    ungroup()

  return(bind_rows(non_scale_df, oos_scaled))
}



#' Convert List to Regular Expression Pattern
#'
#' This function converts a list of strings into a single regular expression pattern.
#' Each string in the list is treated as a separate pattern, and special characters are escaped.
#' It takes into account strings that have full stops in name
#'
#' @param ls A list of strings to be converted into a regular expression pattern.
#' @return A single string containing the combined regular expression pattern.
#' @examples
#' ls <- c("Wages.WC", "Wages.LLM", "Wages")
#' regex_pattern <- list_to_regex(ls)
#' print(regex_pattern)
#' @export
list_to_regex <- function(ls) {
  # Add start (^) and end ($) anchors to each string in the list, and makes full stops escape characters
  ls <- paste0("^", paste0(ls, "$")) |>
    str_replace_all("\\.", "\\\\\\.")

  return(paste(ls, collapse = "|"))
}



#' Create In-Sample and Out-of-Sample Data Windows
#'
#' This function creates in-sample (ins) and out-of-sample (oos) data windows based on specified date ranges.
#'
#' @param x A list containing three date elements: the start and end dates for the in-sample data, and the date for the out-of-sample data.
#' @param df A data frame containing a 'Date' column to filter on.
#' @return A list with two data frames: 'ins' for in-sample data and 'oos' for out-of-sample data.
#' @examples
#' df <- data.frame(Date = as.Date("2023-01-01") + 0:10, Value = rnorm(11))
#' x <- list(as.Date("2023-01-01"), as.Date("2023-01-05"), as.Date("2023-01-06"))
#' window <- create_window(x, df)
#' print(window)
#' @export
create_window <- function(x, df) {
  # Filter data for in-sample window based on the provided date range
  ins_data <- df |>
    filter(Date >= x[[1]] & Date <= x[[2]])
  # Filter data for out-of-sample window based on the provided date
  oos_data <- df |>
    filter(Date == x[[3]])

  return(list("ins" = ins_data, "oos" = oos_data))
}



#' Create Windowed Data for Time Series Analysis
#'
#' This function creates windowed data for time series analysis, supporting both expanding and rolling windows.
#'
#' @param df A data frame containing the time series data with 'Date' and 'Series' columns.
#' @param oos_start The start date for the out-of-sample (oos) data.
#' @param oos_end The end date for the out-of-sample (oos) data. If NULL, all dates after oos_start are included.
#' @param target The target series for which the windows are created.
#' @param scale_series A function or list of functions to scale the series. If NULL, no scaling is applied.
#' @param window A character string specifying the type of window: "expanding" or "rolling".
#' @return A list of data frames, each containing in-sample (ins) and out-of-sample (oos) data.
#'
#' @export
make_window_data <- function(df, oos_start, oos_end = NULL, target, scale_series = NULL, window = "expanding") {
  oos_seq <- if (is.null(oos_end)) {
    df$Date[df$Series == target & df$Date >= oos_start]
  } else {
    df$Date[df$Series == target & df$Date >= oos_start & df$Date <= oos_end]
  }

  if (window == "expanding") {
    ins_seq_start <- rep(df$Date[df$Series == target][1], length(oos_seq)) ## expanding window
  } else if (window == "rolling") {
    ins_seq_start <- df$Date[df$Series == target][1:length(oos_seq)] ## Rolling window
  } else {
    stop("Must be either rolling or expanding window")
  }

  ins_seq_end <- map(oos_seq, ~ tail(df$Date[df$Series == target & df$Date < .x], 1))

  # List of start and end dates to apply to data
  rolling_seq <- pmap(list(ins_seq_start, ins_seq_end, oos_seq), list)

  # Create a list of data subsetted by rolling window
  df_window <- map(rolling_seq, create_window, df)

  # scale each Series
  if (!all(is.null(scale_series))) {
    df_window <- map(df_window, ~ list(
      "ins" = scale_apply(.$ins, .$ins, scale_series), # scale_series(.$ins, target),
      "oos" = scale_apply(.$oos, .$ins, scale_series) # bind_rows(.$ins,.$oos)
    ))
  }

  return(df_window)
}



#' Get Scaling Factors for Out-of-Sample Data
#'
#' This function calculates the mean and standard deviation for each series in the in-sample data
#' and applies these scaling factors to the out-of-sample data.
#'
#' @param target_window A list containing two data frames: 'ins' (in-sample data) and 'oos' (out-of-sample data).
#' @return A data frame with the out-of-sample data, including the calculated mean and standard deviation for each series.
#' @examples
#' target_window <- list(
#'   ins = data.frame(Series = c("A", "A", "B", "B"), value = c(1, 2, 3, 4)),
#'   oos = data.frame(Series = c("A", "B"), value = c(5, 6))
#' )
#' scaled_data <- get_scaling(target_window)
#' print(scaled_data)
#' @export
get_scaling <- function(target_window) {
  # Calculate mean and standard deviation for each series in the in-sample data
  scales <- target_window$ins |>
    dplyr::summarise(mean = mean(value, na.rm = T), std = sd(value, na.rm = T), .by = "Series") |>
    as.list()

  # Apply the calculated mean and standard deviation to the out-of-sample data
  output <- target_window$oos |> mutate(mean = scales$mean, std = scales$std)

  return(output)
}



#' Complete Diebold-Mariano tests agaisnt specified model and all other models
#'
#' @param df_se: dataframe of saved model squared errors over out of sample period
#' @param mdl1name: string specifying baseline model column in df_se to dm test against all other models in df_se
#' @param period: vector of integers to slice df_se by, if NULL then uses all values
#'
#' @return list of p values from dm tests
#'
#' @export
dm.test.all <- function(df_se, mdl1name, period = NULL) {
  dmtests <- list()
  if (!is.null(period)) {
    rows <- period
  } else {
    rows <- 1:nrow(df_se)
  }
  mdl1 <- sqrt(df_se[rows, mdl1name])
  models <- names(df_se %>% select(-Dates))
  for (i in models) {
    print(i)
    mdl2 <- sqrt(df_se[rows, i])
    if (i == mdl1name) {
      dmtests[i] <- NA
    } else {
      res <- dm.test(e1 = mdl1, e2 = mdl2, h = 1, alternative = "greater")
      # print(res)
      dmtests[i] <- res$p.value
    }
  }
  return(dmtests)
}



#' Create Maximum Splits for Time Series Cross-Validation
#'
#' This function creates a trainControl object for time series cross-validation with the maximum number of splits.
#' It leverages the trainControl function from the caret package to configure the cross-validation settings.
#'
#' @param data A data frame containing the time series data.
#' @param num_splits The number of splits for cross-validation.
#' @return A trainControl object configured for time series cross-validation.
#' @examples
#' data <- data.frame(Date = as.Date("2023-01-01") + 0:10, Value = rnorm(11))
#' num_splits <- 5
#' control <- create_max_splits(data, num_splits)
#' print(control)
#' @export
create_max_splits <- function(data, num_splits) {
  l <- nrow(data) - (num_splits)

  return(trainControl(method = "timeslice", initialWindow = l, number = num_splits))
}




#' Fit Regularized Regression Model
#'
#' This function fits a regularized regression model using elastic net regularization.
#' It leverages the train function from the caret package to perform the model training.
#'
#' @param x A data frame containing the predictor variables and target variable.
#' @param target (string) Name of target variable (LHS)
#' @param grid A data frame specifying the tuning grid for the elastic net parameters alpha and lambda.
#' @param standardize A logical value indicating whether to standardize the predictors.
#' @param ... Additional arguments passed to the train function.
#' @return A trained model object.
#'
#' @export
fit_regularised <- function(x, target, grid = NULL, standardize = TRUE, ...) {
  # Create the formula for the regression model (usually take all)
  form <- as.formula(paste(target, "~ ."))

  # Reshape the data to wide format
  x <- x |>
    pivot_wider(id_cols = Date, names_from = Series, values_from = value) |>
    select(-Date)

  # Define the tuning grid if not provided
  ## taken from https://www.projectpro.io/recipes/implement-elastic-net-regression-r
  if (any(is.null(grid))) {
    grid <- expand.grid(
      alpha = seq(0, 1, length = 5),
      lambda = seq(0.0001, 0.2, length = 10)
    )
  }

  # Train the model using the caret package's train function
  train(form,
    data = x,
    method = "glmnet",
    standardize = standardize,
    ...,
    tuneGrid = grid,
    trControl = create_max_splits(x, num_splits = 10),
     # FALSE
    
  )
}



#' Map and Predict Using a Trained Model
#'
#' This function maps the input data to the required format and makes predictions using a trained caret model (fit using fit_regularised function).
#'
#' @param x A data frame containing the predictor variables.
#' @param y A trained model object.
#' @param standardize A logical value indicating whether to standardize the predictors.
#' @return A vector of predictions.
#'
#' @export
map_predict <- function(x, y, target, standardize = TRUE) {
  # Reshape the data to wide format and convert to matrix
  newx <- x |>
    pivot_wider(id_cols = Date, names_from = Series, values_from = value) |>
    select(-all_of(c("Date", target))) |>
    data.matrix()

  # Make predictions using the trained model
  return(predict(y, s = y$lambdaOpt, newx = newx, standardize = standardize))
}



#' Plotting function with pre-defined parameters for pre and post covid splitting of plot
#'
#' @input model: a numerical vector containing predicted values to be plotted
#' @input real: a numerical vector, must be same length as model
#' @input youtdates: a vector of dates (format yyyy-mm-dd), must be same length as model
#' @input model_name: a string that will be inserted into title of plot
#'
#' @return a plot object
#'
oos_plot <- function(model,real,youtdates,model_name) {

  
  p <- ggplot() +
    geom_line(aes(y = real, x = youtdates, colour = "Real")) +
    geom_point(aes(y = real, x = youtdates, colour = "Real")) +
    geom_line(aes(y = model, x = youtdates,colour="Model")) +
    geom_point(aes(y = model, x = youtdates,colour="Model")) +
    labs(y="WPI Quarterly Growth", x="",colour="",
         title = model_name) +
    theme(legend.position = "none")# +
  
  return(p)
}

