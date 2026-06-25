# Load libraries ----------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)


# Import data -------------------------------------------------------------

dta <- read_csv("../data/drc_sitrep.csv")


# Calculate daily incidence ----------------------------------------------
# Zero cumulative records are treated as missing reporting days.
# Negative differences are treated as reporting corrections and set to zero.

dat_i <- dta |>
  arrange(date) |>
  mutate(
    suspect_cases_clean = na_if(suspect_cases, 0),
    suspect_death_clean = na_if(suspect_death, 0),
    confirmed_cases_clean = na_if(confirmed_cases, 0),
    confirmed_death_clean = na_if(confirmed_death, 0)
  ) |>
  fill(
    suspect_cases_clean,
    suspect_death_clean,
    confirmed_cases_clean,
    confirmed_death_clean,
    .direction = "down"
  ) |>
  mutate(
    suspect_cases_clean = replace_na(suspect_cases_clean, 0),
    suspect_death_clean = replace_na(suspect_death_clean, 0),
    confirmed_cases_clean = replace_na(confirmed_cases_clean, 0),
    confirmed_death_clean = replace_na(confirmed_death_clean, 0),
    raw_new_suspect_cases = suspect_cases_clean -
      lag(suspect_cases_clean, default = 0),
    raw_new_suspect_deaths = suspect_death_clean -
      lag(suspect_death_clean, default = 0),
    raw_new_confirmed_cases = confirmed_cases_clean -
      lag(confirmed_cases_clean, default = 0),
    raw_new_confirmed_deaths = confirmed_death_clean -
      lag(confirmed_death_clean, default = 0),
    new_suspect_cases = pmax(raw_new_suspect_cases, 0),
    new_suspect_deaths = pmax(raw_new_suspect_deaths, 0),
    new_confirmed_cases = pmax(raw_new_confirmed_cases, 0),
    new_confirmed_deaths = pmax(raw_new_confirmed_deaths, 0)
  )


# Check dates with negative raw incidence --------------------------------

negative_records <- dat_i |>
  filter(
    raw_new_suspect_cases < 0 |
      raw_new_suspect_deaths < 0 |
      raw_new_confirmed_cases < 0 |
      raw_new_confirmed_deaths < 0
  ) |>
  select(
    date,
    suspect_cases,
    suspect_death,
    confirmed_cases,
    confirmed_death,
    starts_with("raw_new")
  )

print(negative_records)


# Prepare data for plotting ----------------------------------------------

dat_plot <- dat_i |>
  select(
    date,
    new_suspect_cases,
    new_suspect_deaths,
    new_confirmed_cases,
    new_confirmed_deaths
  ) |>
  pivot_longer(
    cols = -date,
    names_to = "data_type",
    values_to = "incidence"
  ) |>
  mutate(
    data_type = recode(
      data_type,
      new_suspect_cases = "Suspect Cases",
      new_suspect_deaths = "Suspect Deaths",
      new_confirmed_cases = "Confirmed Cases",
      new_confirmed_deaths = "Confirmed Deaths"
    )
  )


# Plot incidence ----------------------------------------------------------

dta_plot <- ggplot(
  dat_plot,
  aes(x = date, y = incidence, colour = data_type, linetype = data_type)
) +
  geom_line(
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  scale_colour_manual(
    values = c(
      "Suspect Cases" = "blue",
      "Suspect Deaths" = "pink",
      "Confirmed Cases" = "darkblue",
      "Confirmed Deaths" = "darkred"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Suspect Cases" = "dashed",
      "Suspect Deaths" = "dashed",
      "Confirmed Cases" = "solid",
      "Confirmed Deaths" = "solid"
    )
  ) +
  labs(
    title = "Plot of Suspected and Confirmed Incident Ebola Deaths and Cases",
    subtitle = "Zero cumulative records treated as missing; negative corrections set to zero",
    x = "Date",
    y = "Number of new cases and deaths",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(colour = "steelblue"),
    legend.position = "bottom"
  )

print(dta_plot)
