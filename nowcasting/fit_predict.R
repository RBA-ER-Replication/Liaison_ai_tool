#'
#' @title  Section 5: Nowcasting Wages Growth - Out-of-sample performance testing
#'
#' @description Code that takes dataset of Phillip's Curve, Staff Score and Text-based variables and undertakes out-of-sample performance testing of each model type.
#' The script will first fit manual OLS models, then will fit all regressors using regularisation models.
#' The regularised model will first gridsearch to select optimal alpha and lambda values, then use this to fit data and calculate OOS model performance.
#'
#' @author Nicholas Gray
#'
#' @param dataset_name (string): location of input dataframe containing target and regressor variables (ideally quarterly values).
#' @param suffix (string): name added to output files to define different variations of parameters selected.
#' @param baseline (boolean): If TRUE, code will only run OLS and regularised regressions for the Baseline model specification
#' @param oos_start, oos_end (date): Start and end quarters to be used for out of sample periods, 2015Q1 to 2024Q3 used in original paper.
#' @param target (string): Names of variable used as target variable in nowcasting.
#' @param manual models (list): named list of equations that will be fit as OLS models, must have one model named "Baseline".
#' @param lambda_seq (list): list of numerical values for lambda that will be grid-searched for choosing best regularisation specification
#' @param ridge_grid,lasso_grid,elasticnet_grid (list): numerical values for alpha (0=ridge, 1=lasso, list=elastic net) and lambda that
#' will be grid-searched for choosing best regularisation specification
#'
#'
#' @output excel file with save results for OLS and regularisation model OOS performance
#' @output RDS file with save model outputs for analysis

req_pack <- c(
  "forecast", "zoo", "lmtest","scales","lubridate",
  "furrr", "ggplot2", "writexl", "glue", "readxl",
  "data.table", "tidyverse", "glmnet", "caret"
)

for (p in req_pack) {
  # if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

source("Functions.R")

###################################################################################
############################ Adjustable Parameters ################################
###################################################################################
dataset_name <- "../Data/nowcasting_df.xlsx"

## for output file if making variations to parameters
suffix <- ""

## Whether or not only running with baseline variables
baseline <- FALSE

# First and last out of sample Quarters to roll over, oos_end = Null will use latest quarter available
oos_start <- ymd("2015-03-31")
oos_end <- ymd("2024-09-30")

## choose target variable
target <- "WPI"

## Choose specific models
manual_models <- list(
  Baseline = paste(target, "~  WPI_lag + Unemp_gap_lag + Underutil_gap_lag + Trend_inf_exp_lag") #,
)
if (!baseline) {manual_models["All"] <- paste(target, "~ .")}

lambda_seq <- c(0,seq(0.004, 0.5, length = 30),seq(0.51, 1, length = 30), seq(1.5, 7, length = 40), seq(8, 16, length = 9)) #seq(0.004, 0.5, length = 30)

ridge_grid <- expand.grid(
  alpha = 0,
  lambda = lambda_seq
)

lasso_grid <- expand.grid(
  alpha = 1,
  lambda = lambda_seq
)

elasticnet_grid <- expand.grid(
  alpha = seq(0, 1, length = 10),
  lambda = lambda_seq
)

###################################################################################
################################# DATA IMPORT #####################################
###################################################################################

# import data from local
df <- read_excel(dataset_name)

# if running baseline variables only
if (baseline) {
  bs_var <- str_split_1(manual_models$Baseline, pattern = "[~+]") |> str_trim()
  df <- select(df, Date, all_of(c(bs_var)))
}

# pivot to long for data,
df_long <- df |>
  mutate(Date = ymd(Date)) |>
  pivot_longer(cols = -Date, names_to = "Series") 

# select variables to scale (usually all)
scale_all <- df_long |>
  distinct(Series) |>
  pull(Series)

# split long data into modelling windows, and scale in each window
df_window <- make_window_data(df_long, oos_start, oos_end, target = target, scale_series = scale_all)
target_window <- make_window_data(filter(df_long, Series == target), oos_start, oos_end, target = target)

## impute any missing values to zero
df_window <- map(df_window, impute_missing)

## Inititalise result dataframe
target_window <- map(target_window, get_scaling)
yout <- as_tibble(target_window |> bind_rows())

###################################################################################
################################## OLS Models #####################################
###################################################################################

# fit all ols models and calculate one step forward OOS prediction
fit_models <- function(x) {
  map(df_window, ~ lm(x, .x$ins |>
    pivot_wider(id_cols = Date, names_from = Series, values_from = value) |>
    select(-Date)))
}
fit <- map(manual_models, fit_models)

# apply model to out of sample period
pred_models <- function(x) {
  map2(df_window, x, ~ predict(.y, .x$oos |>
    pivot_wider(id_cols = Date, names_from = Series, values_from = value) |>
    select(-Date)))
}
pred <- map(fit, pred_models)

# save to results dataframe
results <- map(pred, function(x) {
  map(x, ~ .[[1]]) |> unlist()
})

###################################################################################
############################## Ridge regression ###################################
###################################################################################

# set up models and fit to data
mod_fit <- map(df_window, ~ fit_regularised(.$ins,
  target = target,
  grid = ridge_grid,
  standardize = TRUE,
  family = "gaussian",
  lambda = lambda_seq
))
mod_ridge <- mod_fit

# save coeffiecients and model parameters
ridge_lambda <- map(mod_fit, ~ .x$bestTune) |>
  bind_rows() |>
  mutate(Date = yout$Date) |>
  select(Date, lambda)
ridge_coefs <- map(mod_fit, ~ coef(.x$finalMode, s = .x$finalModel[["lambdaOpt"]]))
ridge_coefs <- map(ridge_coefs, ~ as.data.frame(as.matrix(.x) |> t())) |>
  bind_rows() |>
  mutate(Date = yout$Date)

# make oos prediction with best model
pred_vals <- map2(df_window, mod_fit, ~ map_predict(.x$oos, .y$finalModel, target = target))

# save to results dataframe
results <- append(results, list(ridge = map(pred_vals, ~ .[[1]]) |> unlist()))

###################################################################################
############################### Lasso regression ##################################
###################################################################################

# set up models and fit to data
mod_fit <- map(df_window, ~ fit_regularised(.$ins,
  target = target,
  grid = lasso_grid,
  standardize = TRUE,
  family = "gaussian",
  lambda = lambda_seq
))
mod_lasso <- mod_fit

# save coeffiecients and model parameters
lasso_lambda <- map(mod_fit, ~ .x$bestTune) |>
  bind_rows() |>
  mutate(Date = yout$Date) |>
  select(Date, lambda)
lasso_coefs <- map(mod_fit, ~ coef(.x$finalMode, s = .x$finalModel[["lambdaOpt"]]))
lasso_coefs <- map(lasso_coefs, ~ as.data.frame(as.matrix(.x) |> t())) |>
  bind_rows() |>
  mutate(Date = yout$Date)

# make oos prediction with best model
pred_vals <- map2(df_window, mod_fit, ~ map_predict(.x$oos, .y$finalModel, target = target))

# save to results dataframe
results <- append(results, list(lasso = map(pred_vals, ~ .[[1]]) |> unlist()))

###################################################################################
######################### Elastic net regularisation ##############################
###################################################################################

# set up models and fit to data
mod_fit <- map(df_window, ~ fit_regularised(.$ins,
  target = target,
  grid = elasticnet_grid,
  standardize = TRUE,
  family = "gaussian",
  lambda = lambda_seq
))
mod_lnet <- mod_fit

# save coeffiecients and model parameters
alphalambda <- map(mod_fit, ~ .x$bestTune) |>
  bind_rows() |>
  mutate(Date = yout$Date)
elasticnet_coefs <- map(mod_fit, ~ coef(.x$finalMode, s = .x$finalModel[["lambdaOpt"]]))
elasticnet_coefs <- map(elasticnet_coefs, ~ as.data.frame(as.matrix(.x) |> t())) |>
  bind_rows() |>
  mutate(Date = yout$Date)

# make oos prediction with best model
pred_vals <- map2(df_window, mod_fit, ~ map_predict(.x$oos, .y$finalModel, target = target))

results <- append(results, list(elasticnet = map(pred_vals, ~ .[[1]]) |> unlist()))


########################################################################################
####################### Calculate results for full sample ##############################
########################################################################################
# initialise rmse dataframe
df_results <- data.frame(list("Model" = NULL, "RMSE" = NULL))

# initialise squared errors dataframe
df_se <- data.frame(list("Dates" = yout$Date))

# calculate Out of sample RMSE
mod_RMSE <- map(results, ~ accuracy(yout$value, (. * yout$std + yout$mean))[2]) |>
  as.data.frame() |>
  pivot_longer(cols = everything(), names_to = "Model", values_to = "RMSE")
df_results <- df_results |> bind_rows(mod_RMSE)

# save SE over time
mod_SE <- map(results, ~ (yout$value - (. * yout$std + yout$mean))**2) |>
  as.data.frame()

# save errors over time
df_residuals <- df_se |> cbind(map(results, ~ ((. * yout$std + yout$mean) - yout$value)) |> as.data.frame())
df_se <- df_se |> cbind(mod_SE)

## Get RMSE ratio to benchmark model
Baseline <- df_results[df_results$Model == "Baseline", "RMSE"]
df_results <- df_results |> mutate(
  `RMSE/Baseline` = RMSE / Baseline
)

### diabold-mariano test
dm.test1 <- dm.test.all(df_se, "Baseline")
dm.tests <- as.data.frame(list(
  "Model" = names(dm.test1),
  "Baseline" = as.numeric(dm.test1)
))

## Merge results
results_out <- merge(df_results, dm.tests, by = "Model")

########################################################################################
######################## Calculate results for pre-covid ###############################
########################################################################################

# set count for number of pre-covid periods
m <- sum(yout$Date <= ymd("2019-12-31"))

## create RSME dataframe
df_results_pre <- data.frame(list("Model" = NULL, "RMSE" = NULL))

## Calculate Out of sample RMSE
mod_RMSE <- map(results, ~ accuracy(yout$value[1:m], (. * yout$std + yout$mean)[1:m])[2]) |>
  as.data.frame() |>
  pivot_longer(cols = everything(), names_to = "Model", values_to = "RMSE")
df_results_pre <- df_results_pre |> bind_rows(mod_RMSE)

## Get RMSE ratio to benchmark model
Baseline <- df_results_pre[df_results_pre$Model == "Baseline", "RMSE"]
df_results_pre <- df_results_pre %>% mutate(
  `RMSE/Baseline(%)` = 100*RMSE / Baseline
)

### dm test
dm.test1 <- dm.test.all(df_se, "Baseline", c(1:m))
dm.tests <- as.data.frame(list(
  "Model" = names(dm.test1),
  "DM_ptest" = as.numeric(dm.test1)
))

## Merge results
results_out_pre <- merge(df_results_pre, dm.tests, by = "Model")

########################################################################################
################################## Save Results ########################################
########################################################################################

# ensure if running baseline, that suffix contains evidence of this
if (baseline & !grepl("baseline",suffix)) {suffix <- paste0(suffix,"_baseline")}

## Save results to file
write_xlsx(
  list(
    "Results" = results_out, #full history RMSE and p-test results
    "Results pre-COVID" = results_out_pre, #Pre-COVID RMSE and p-test results
    "Residuals" = df_residuals, ## unstandardised residuals
    "Elasticnet Alpha Lambda" = alphalambda, #selected alpha and lambda for all-variable elastic net model
    "Elasticnet Coefs" = elasticnet_coefs, #fitted coefficients for all-variable elastic net model
    "Lasso Lambda" = lasso_lambda, #selected lambda for all-variable lasso model
    "Lasso Coefs" = lasso_coefs, #fitted coefficients for all-variable lasso model
    "Ridge Lambda" = ridge_lambda, #selected lambda for all-variable ridge model
    "Ridge Coefs" = ridge_coefs #fitted coefficients for all-variable ridge model
  ),
  ## Name the results based on settings used in modelling
  path = glue("../Data/OOS_results_{target}_all_models{suffix}.xlsx")
)

## Save model outputs for later analysis
output <- list(
  results = results, #standardised target predictions
  coefs = list("ridge" = ridge_coefs, "lasso" = lasso_coefs, "elasticnet" = elasticnet_coefs), #fitted coefficients for all-variable regularised models
  yout = yout, #real values for out-of-sample target variables
  models = mod_fit[[39]]$results #last nowcasting window model file
)

saveRDS(output, file = glue("../Data/modeldata_{target}_all_models{suffix}.Rdata"))

## Save models
## Save model output objects for later analysis
models <- list(
  ridge = mod_ridge,
  lasso = mod_lasso,
  elasticnet = mod_lnet
)

saveRDS(models, file = glue("../Data/models_{target}_all_models{suffix}.Rdata"))



