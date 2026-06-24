
# Load Library
library(readr)
library(ggplot2)

# Import Data set

dta <- read_csv("../data/drc_sitrep.csv")

# Plot by data type

## Consider using pivot_longer and then use one geom per logical item

dta_plot <- ggplot(dta, aes(x = date)) +

  # Suspect cases
  geom_line(
    aes(y = suspect_cases, colour = "Suspect Cases"),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  
  # Suspect death
  geom_line(
    aes(y = suspect_death, colour = "Suspect Deaths"),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  
  # Confirmed Cases
  geom_line(
    aes(y = confirmed_cases, colour = "Confirmed Cases"),
    linewidth = 1.1,
    ## linetype = "dashed",
    na.rm = TRUE
  ) +
  
  # Confirmed Deaths
  geom_line(
    aes(y = confirmed_death, colour = "Confirmed Deaths"),
    linewidth = 1.1,
    ## linetype = "dashed",
    na.rm = TRUE
  ) +
  
  scale_colour_manual(
    values = c(
      "Suspect Cases" = "blue",
      "Suspect Deaths" = "darkblue",
      "Confirmed Cases" = "pink",
      "Confirmed Deaths" = "red"
    )
  ) +
  
  labs(
    title = "Plot of Suspected and Confirmed Ebola Deaths and Cases",
    subtitle = "",
    x = "Date",
    y = "Number of cases and Deaths",
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(colour = "steelblue"),
    axis.title.y.right = element_text(colour = "darkred"),
    legend.position = "bottom"
  )

print(dta_plot)

print(dta_plot + scale_y_log10())
