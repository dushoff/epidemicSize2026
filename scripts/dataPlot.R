library(readr)
library(tidyr)
library(dplyr)
## maybe base_size=18 for slides?
library(ggplot2); theme_set(theme_minimal(base_size=14))
pdf(width=10)

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
  scale_color_manual(values = c("Suspect" = "#E68A00", "Confirmed" = "#2C5F8A")) +
  labs(
    title = "Suspected and Confirmed Ebola Cases and Deaths",
    x = "Date", y = "Number of cases / deaths",
    color = "Surveillance type", linetype = "Event type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(dta_plot)
print(dta_plot + scale_y_log10())



fig1 <- ggplot(dta_long, aes(x = date, y = count, color = eventType)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(values = c("Cases" = "#2C5F8A", "Deaths" = "#B23A48")) +
  facet_wrap(~ surveillanceType, ncol = 1, scales = "free_y") +
  labs(
    title = "Ebola Cases and Deaths, by Surveillance Type",
    x = "Date", y = "Count", color = "Event type"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(fig1)


fig2 <- ggplot(dta_long, aes(x = date, y = count, color = surveillanceType)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_color_manual(values = c("Suspect" = "#E68A00", "Confirmed" = "#2C5F8A")) +
  facet_wrap(~ eventType, ncol = 1, scales = "free_y") +
  labs(
    title = "Ebola Suspect vs Confirmed, by Event Type",
    x = "Date", y = "Count", color = "Surveillance type"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(fig2)
