#'
#' @title  Plotting Script for RDP figures
#'
#' @description This scripts allows for easy plotting of Figures 1, 4, 7-9 in the associated RDP
#'
#' @author Nicholas Gray
#'
#' @param data_dir (string or path): location of input data, and where to save the resultant plots
#'
#' @output png files of the plots similar to the ones found in the RDP
#'
#'


## Load packages (see requirements.txt for versions used to create original plots)
library(tidyverse)
library(readxl)
library(zoo)
library(glue)
library(gridExtra)

#### PARAMS ####
data_dir <- "../Data/"

## file location
file <- paste0(data_dir, "rdp-2025-06-graph-data.xlsx")

## Load in Data
fig1_data <- read_excel(file, sheet = 1, skip = 11) |> filter(!is.na(Date))
fig4_data <- read_excel(file, sheet = 4, skip = 11) |> filter(!is.na(Date))
fig7a_data <- read_excel(file, sheet = 7, range = "A12:C88") |> filter(!is.na(Date))
fig7b_data <- read_excel(file, sheet = 7, range = "D12:F88") |> filter(!is.na(Date))
fig8_data <- read_excel(file, sheet = 8, skip = 11) |> filter(!is.na(Date))
fig9_data <- read_excel(file, sheet = 9, skip = 11) |> filter(!is.na(Date))


#### Figure 1: Mentions of Liaison in the Statement on Monetary Policy
fig1 <- fig1_data |>
  mutate(Date = ymd(Date)) |>
  ggplot(aes(x = Date, y = Frequency)) +
  geom_line(colour = "orange") +
  geom_vline(xintercept = ymd("2022-12-31"), linetype = "dashed", colour = "grey") +
  theme_light() +
  labs(
    title = "Figure 1: Mentions of Liaison in the Statement on Monetary Policy",
    subtitle = "Share of total words",
    y = "Share (%)", x = ""
  )
fig1
ggsave(paste0(data_dir, "Figure_1.png"))



#### Figure 4: Term Frequencies – Selected Supply Chain References
fig4 <- fig4_data |>
  pivot_longer(
    cols = c("Supply", "Shipping", "Delays"),
    names_to = "word", values_to = "share"
  ) |>
  ggplot(aes(x = Date, y = share, colour = word)) +
  geom_line() +
  theme_light() +
  labs(
    title = "Figure 4: Term Frequencies – Selected Supply Chain References",
    subtitle = "Share of total words, quarterly",
    colour = "", y = "Share (%)", x = ""
  )
fig4
ggsave(paste0(data_dir, "Figure_4.png"))



#### Figure 7: Tracking the Discussion of Wages
### Tpic exposure measures
p1 <- fig7a_data |>
  pivot_longer(cols = c("Dictionary", "LM"), names_to = "Series", values_to = "value") |>
  ggplot(aes(x = ymd(Date), y = value, colour = Series)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  theme_light() +
  theme(legend.position = "none") +
  ylim(c(-6, 4)) +
  labs(subtitle = "Topic exposure", x = "", y = "std dev")

## topic-specific tone measures
p2 <- fig7b_data |>
  pivot_longer(cols = c("Dictionary", "LM"), names_to = "Series", values_to = "value") |>
  ggplot(aes(x = ymd(Date), y = value, colour = Series)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  theme_light() +
  theme(
    legend.position = "inside",
    legend.background = element_blank(),
    legend.position.inside = c(0.2, 0.15)
  ) +
  ylim(c(-6, 4)) +
  labs(subtitle = "Topic-specific tone", x = "", y = "std dev", colour = "")


fig7 <- arrangeGrob(p1, p2, nrow = 1, top = "Figure 7: Tracking the Discussion of Wages")
plot(fig7)
ggsave(filename = paste0(data_dir, "Figure_7.png"), plot = fig7)


#### Figure 8: Business Liaison Uncertainty Index
fig8 <- ggplot(fig8_data, aes(x = ymd(Date, y = Uncertainty))) +
  geom_line(data = fig8_data, aes(x = ymd(Date), y = Uncertainty), colour = "lightblue", alpha = 0.9) +
  geom_line(data = fig8_data, aes(x = ymd(Date), y = Trend), colour = "blue") +
  geom_hline(yintercept = 0) +
  theme_light() +
  labs(
    title = "Figure 8: Business Liaison Uncertainty Index", x = "", y = "std dev",
    caption = "Note:Series is standardised to show how many standard deviations it is from its mean value; series is monthly \nwith a 13-month Henderson trend"
  ) +
  theme(plot.caption = element_text(hjust = 0))

fig8
ggsave(paste0(data_dir, "Figure_8.png"))


#### Figure 8: Business Liaison Uncertainty Index
fig9 <- fig9_data |>
  pivot_longer(cols = -Date, names_to = "Series", values_to = "stddev") |>
  ggplot(aes(x = ymd(Date), y = stddev, colour = Series)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_light() +
  theme(
    legend.position = "inside",
    legend.background = element_blank(),
    legend.position.inside = c(0.2, 0.9)
  ) +
  labs(
    title = "Figure 9: Benchmarking our Price Inflation Extractions",
    subtitle = "Two-quarter rolling average", x = "", y = "std dev", colour = ""
  )

fig9
ggsave(paste0(data_dir, "Figure_9.png"))