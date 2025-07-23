#'
#' @title  Nowcasting WPI with text-based liaison series: Analysis of results and output plots
#'
#' @description Plots and figures related to the output of the nowcasting exercise undertaken in fit_predict.R.
#' Plus code to plot figures 14, 15 and D1 of the related paper.
#'
#' @author Nicholas Gray
#'
#' @param suffix (string): name added to output files to define different variations of parameters selected.
#'

req_pack <- c(
  "furrr", "ggplot2", "writexl", "glue",
  "lubridate", "data.table", "tidyverse",
  "readxl"
)

for (p in req_pack) {
  # if (!require(p, character.only = TRUE, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)
}

source("Functions.R")


##### PARAMS #####

suffix <- ""

##################

## Import data
df <- read_excel("../Data/nowcasting_df.xlsx")
output <- readRDS(glue("../Data/modeldata_WPI_all_models{suffix}.Rdata"))

## Extract data from modelling output object
yout <- output$yout #real out-of-sample period target values
coefs_all <- output$coefs #fitted regularised model coefficients
models <- output$models #fitted regularised model objects
results <- output$results #standardised target predictions


############################################################################################################################################
# plot predictions against real (unstandardised)
plots <- imap(results, ~ oos_plot(.x * yout$std + yout$mean, yout$value, ymd(yout$Date), .y))
eval(plots)

############################################################################################################################################
### Figure 14 - Regularisation Cross-validation
std <- output$yout$std[[39]] ## This is standard deviation of last df_window for unscaling data
models <- output$models |>
  mutate(
    alpha = round(alpha, 3),
    Model = case_when(alpha == 0 ~ "Ridge", alpha == 0.556 ~ "Elastic net*", alpha == 1 ~ "Lasso", TRUE ~ "Other")
  ) |>
  filter(!Model == "Other") |>
  mutate(RMSE = RMSE * std)

# Get lambdas for Minimum RMSE
minlambda <- models |>
  group_by(Model) |>
  filter(RMSE == min(RMSE))

## line colour palette
pal <- c("Ridge" = "brown", "Elastic net*" = "lightblue", "Lasso" = "purple")

## plot
cv_plot <- ggplot(data = models, aes(x = lambda, y = RMSE, colour = Model)) +
  geom_line() +
  scale_colour_manual(values = pal)

# add vertical lines for minimum lambdas
for (nm in names(pal)) {
  min_val <- minlambda |>
    ungroup() |>
    filter(Model == nm) |>
    pull(lambda)
  cv_plot <- cv_plot + geom_vline(xintercept = min_val, linetype = "longdash", colour = pal[[nm]], linewidth = 0.8)
}

## Add labels and reverse x axis
cv_plot <- cv_plot + scale_x_continuous(transform = "reverse", limits = c(0.51, -0.01)) +
  theme_light() +
  labs(
    title = "Figure 14: Optimal Hyperparameter Selection",
    subtitle = "Cross-validated RMSEs for the September 2024 nowcast",
    x = "λ", y = "RMSE (ppt)", colour = "",
    caption = "Note:The mixing weight for the elastic net regression is fixed at 0.56.\n        For illustrative purposes, the optimal value for the ridge regression (lambda[ridge] = 6.9) is not shown.)"
  ) +
  theme(plot.caption = element_text(hjust = 0))
cv_plot
ggsave("../Data/Figure_14.png")

############################################################################################################################################
# plot error over time from results file
errors <- map(output$results, ~ ((. * output$yout$std + output$yout$mean) - output$yout$value)) |>
  as.data.frame() |>
  mutate(Date = output$yout$Date) |>
  select(Date, Baseline, ElasticNet = elasticnet, LASSO = lasso) |>
  pivot_longer(cols = -Date, names_to = "Model", values_to = "Error")

## create error plot over time
error_plot <- errors |>
  ggplot(aes(x = Date, y = Error, colour = Model)) +
  geom_line() +
  scale_colour_manual(values = c("Baseline" = "brown", "ElasticNet" = "blue", "LASSO" = "green")) +
  labs(
    title = "Nowcasting Errors",
    y = "RMSE (ppt)"
  ) +
  geom_hline(yintercept = 0)

error_plot


##############################################################################################################################
### Figure 15 - Variables selected for Nowcasting
coefs <- coefs_all$lasso

df_coefs_useful <- coefs |>
  pivot_longer(cols = -Date, names_to = "Series", values_to = "value") |>
  filter(Series != "(Intercept)") |>
  mutate(is_non_zero = case_when(value != 0 ~ 1, TRUE ~ 0)) |>
  dplyr::summarise(num_non_zero = sum(is_non_zero), .by = "Series") |>
  filter(num_non_zero > 10)

### CREATE Regressor selection tile compatible dataframe
coef_tile <- coefs |>
  pivot_longer(cols = -Date, names_to = "Series", values_to = "value") |>
  group_by(Series) |>
  filter(Series %in% df_coefs_useful$Series) |>
  mutate(
    is_non_zero = case_when(value != 0 ~ 1, TRUE ~ 0),
    Date = floor_date(Date, "month") - 9
  ) |>
  left_join(df_coefs_useful, by = "Series")

# variable groups
baseline <- df_coefs_useful |>
  arrange(num_non_zero) |>
  filter(grepl("WPI_lag|Unemp_gap_lag|Underutil_gap_lag|Trend_inf_exp_lag", Series)) |>
  distinct(Series) |>
  pull(Series)
liaison <- df_coefs_useful |>
  arrange(num_non_zero) |>
  filter(grepl("llm|dict|extract|hand_collected", Series)) |>
  distinct(Series) |>
  pull(Series)
likerts <- df_coefs_useful |>
  arrange(num_non_zero) |>
  filter(!(Series %in% liaison | Series %in% baseline)) |>
  distinct(Series) |>
  pull(Series)
# assign to groups and order
coef_tile <- coef_tile |>
  mutate(Variable = case_when(
    Series %in% baseline ~ "Baseline",
    Series %in% likerts ~ "Staff scores",
    Series %in% liaison ~ "Text based",
    TRUE ~ "Other"
  )) |>
  arrange(num_non_zero)


## Shift to better align on ggrba plot
coef_tile$Series <- factor(coef_tile$Series, levels = c(liaison, likerts, baseline)) ## levels by variable type

## Plot tile graph
var_plot <- ggplot(data = filter(coef_tile, is_non_zero == 1), aes(x = Date, y = Series, fill = Variable)) +
  geom_tile(width = 99, height = 1) +
  theme_light() +
  scale_fill_manual(values = c("Baseline" = "brown", "Staff scores" = "lightblue", "Text based" = "purple")) +
  scale_y_discrete(expand = c(0, 0)) + # ensure sitting flush w
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "", fill = "",
    title = "Figure 7: Variables Selected by Models",
    subtitle = "Variables selected in-sample using lasso"
  )
## Add lines between each row for clarity
for (i in seq(from = 1.5, by = 1, length.out = nrow(coef_tile |> distinct(Series)) - 1)) {
  var_plot <- var_plot +
    geom_hline(yintercept = i, color = "black", linewidth = 0.3)
}

var_plot
ggsave("../Data/Figure_14.png")

##############################################################################################################################
### Figure D1: Variables selected for Nowcasting
coefs <- coefs_all$lasso

### CREATE Regressor selection tile compatible dataframe
df_coefs <- coefs |>
  pivot_longer(cols = -Date, names_to = "Series", values_to = "value") |>
  filter(Series != "(Intercept)") |>
  mutate(is_non_zero = case_when(value != 0 ~ 1, TRUE ~ 0)) |>
  dplyr::summarise(num_non_zero = sum(is_non_zero) / length(is_non_zero), .by = "Series") |>
  arrange(-num_non_zero) |>
  mutate(num_non_zero = case_when(num_non_zero == 0 ~ 0.005, TRUE ~ num_non_zero))

# variable groups
baseline <- df_coefs |>
  arrange(num_non_zero) |>
  filter(grepl("WPI_lag|Unemp_gap_lag|Underutil_gap_lag|Trend_inf_exp_lag", Series)) |>
  distinct(Series) |>
  pull(Series)
liaison <- df_coefs |>
  arrange(num_non_zero) |>
  filter(grepl("llm|dict|extract|hand_collected", Series)) |>
  distinct(Series) |>
  pull(Series)
likerts <- df_coefs |>
  arrange(num_non_zero) |>
  filter(!(Series %in% liaison | Series %in% baseline)) |>
  distinct(Series) |>
  pull(Series)

# assign to groups and order
df_coefs <- df_coefs |>
  mutate(
    liaison_based = case_when(
      Series %in% baseline ~ "Baseline",
      Series %in% likerts ~ "Staff scores",
      Series %in% liaison ~ "Text based",
      TRUE ~ "Other"
    ),
    regressor = c(1:nrow(df_coefs)),
    num_non_zero = num_non_zero * 100
  )


## Shift to better align on ggrba plot
df_coefs$Series <- factor(df_coefs$Series, levels = c(liaison, likerts, baseline)) ## levels by variable type

## Plot tile graph
var_plot <- ggplot(df_coefs, aes(x = num_non_zero, y = reorder(Series, num_non_zero), fill = liaison_based)) +
  geom_col() +
  theme_light() +
  scale_fill_manual(values = c("Baseline" = "brown", "Staff scores" = "lightblue", "Text based" = "purple")) +
  scale_y_discrete(expand = c(0, 0)) + # ensure sitting flush w
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = "", x = "Selected (%)",
    title = "Figure D1: Variables Selected for Nowcasting", fill = "",
    subtitle = "Per cent of nowcasts that select the variable; lasso specification"
  )

var_plot
ggsave("../Data/Figure_D1.png")