library(readr)
library(tidyr)
library(dplyr)
library(ggplot2); 


PLOT_MODE <- "report"  # "report" or "slides"

BASE_SIZE <- switch(
  PLOT_MODE,
  report = 14,
  slides = 18
)

theme_set(
  theme_minimal(base_size = BASE_SIZE)
)


dta <- read_csv("../data/drc_sitrep.csv")

dta_long <- dta |>
  pivot_longer(
    cols = c(suspect_cases, suspect_death, confirmed_cases, confirmed_death),
    names_to = "series",
    values_to = "count"
  ) |>
  mutate(
    surveillanceType = if_else(grepl("^suspect", series), "Suspect", "Confirmed"),
    eventType = if_else(grepl("cases$", series), "Cases", "Deaths")
  )

head(dta_long)

sim_plot <- (ggplot(dta_long)
	+ aes(date, count, color=eventType, linetype=surveillanceType)
	+ geom_line()
)

print(sim_plot)
print(sim_plot+scale_y_log10())
print(sim_plot+scale_y_log10() + dta_long |> filter(surveillanceType=="Confirmed"))

dta_plot <- ggplot(dta_long, aes(x = date, y = count, 
                                color = surveillanceType, 
                                 linetype = eventType)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(values = c("Suspect" = "#E68A00", "Confirmed" = "#2C5F8A"))

print(dta_plot)
print(dta_plot + scale_y_log10())



fig1 <- ggplot(dta_long, aes(x = date, y = count, color = eventType)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(values = c("Cases" = "#2C5F8A", "Deaths" = "#B23A48")) +
  facet_wrap(~ surveillanceType, ncol = 1, scales = "free_y")

print(fig1)


fig2 <- ggplot(dta_long, aes(x = date, y = count, color = surveillanceType)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(values = c("Suspect" = "#E68A00", "Confirmed" = "#2C5F8A")) +
  facet_wrap(~ eventType, ncol = 1, scales = "free_y")

print(fig2)

