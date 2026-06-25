#explore data
#see what we have 
#and what we do not have

#install.packages("MASS")

library(MASS)
library(ggplot2)
library(gridExtra)

# Read data

## OK
## setwd("../data")

## Better
data <- read.csv("../data/drc_sitrep.csv")

data$date <- as.Date(data$date)
data$day <- as.numeric(data$date - data$date[1])

#pwd# ==== FIT NEGATIVE BINOMIAL GLM ====
glm_fit <- glm.nb(confirmed_cases ~ day, data = data)

# Extract parameters
small_r <- coef(glm_fit)[2]
intercept <- coef(glm_fit)[1]
doubling <- log(2) / small_r 

# Get confidence intervals
CI <- confint(glm_fit)
r_CI <- CI[2, ]

## Our actual result is CI for r; let's plot that first

# ==== CALCULATE R0 FOR 3 SI SCENARIOS ====
## Calculate R is a good idea but a little bit complicated
## You are showing CI but you are not reflecting uncertainty in SI
## This calculation is sensitive to a distribution assumption
## You can generalize this if you estimate the variation in SI
## To get CIs, you would then want uncertainty in variation

## Sensitivity analysis using Ebola SI from literature
## Primary estimate: SI = 15.4 days (best estimate from meta-analysis)
## Sensitivity bounds: 95% CI = 13.2 - 17.5 days

SI_scenarios <- data.frame(
  scenario = c("Low (95% CI Lower)", "Primary Estimate", "High (95% CI Upper)"),
  SI = c(13.2, 15.4, 17.5)
)

# Calculate R0 and CI for each scenario
SI_scenarios$R0 <- exp(small_r * SI_scenarios$SI)
SI_scenarios$R0_lower <- exp(r_CI[1] * SI_scenarios$SI)
SI_scenarios$R0_upper <- exp(r_CI[2] * SI_scenarios$SI)

# ==== PRINT RESULTS ====
print("===== SMALL r ESTIMATE =====")
print(paste("Small r:", round(small_r, 4), "per day"))
print(paste("95% CI: [", round(r_CI[1], 4), ", ", round(r_CI[2], 4), "]"))
print(paste("Doubling time:", round(doubling, 2), "days"))
print("")

print("===== R0 ESTIMATES FOR 3 SI SCENARIOS (SENSITIVITY ANALYSIS) =====")
for (i in 1:nrow(SI_scenarios)) {
  if (i == 2) {
    print(paste("*** PRIMARY ESTIMATE *** ", SI_scenarios$scenario[i], " SI (", SI_scenarios$SI[i], 
                " days): R0 =", round(SI_scenarios$R0[i], 3),
                " 95% CI: [", round(SI_scenarios$R0_lower[i], 3), 
                ", ", round(SI_scenarios$R0_upper[i], 3), "]"))
  } else {
    print(paste(SI_scenarios$scenario[i], " SI (", SI_scenarios$SI[i], 
                " days): R0 =", round(SI_scenarios$R0[i], 3),
                " 95% CI: [", round(SI_scenarios$R0_lower[i], 3), 
                ", ", round(SI_scenarios$R0_upper[i], 3), "]"))
  }
}
print("")

# ==== CREATE PREDICTIONS FOR PLOTTING ====
newdata <- data.frame(day = seq(min(data$day), max(data$day), by = 0.5))
pred <- predict(glm_fit, newdata = newdata, type = "response", se.fit = TRUE)

pred_df <- data.frame(
  day = newdata$day,
  fit = pred$fit,
  se = pred$se.fit
)

# ==== FIGURE 1: GLM FIT ====
fig1 <- ggplot(data, aes(x = day, y = confirmed_cases)) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  geom_line(data = pred_df, aes(y = fit), color = "blue", size = 1.2) +
  geom_ribbon(data = pred_df, 
              aes(y = fit, 
                  ymin = fit - 1.96*se, 
                  ymax = fit + 1.96*se),
              alpha = 0.2, fill = "blue") +
  labs(title = "Negative Binomial GLM Fit",
       subtitle = paste("r =", round(small_r, 4), "/day [95% CI: ", round(r_CI[1], 4), ", ", round(r_CI[2], 4), "], Doubling time =", round(doubling, 2), "days"),
       x = "Days since first case",
       y = "Confirmed Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# ==== FIGURE 2: LOG SCALE (EXPONENTIAL GROWTH) ====
fig2 <- ggplot(data, aes(x = day, y = confirmed_cases)) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  geom_line(data = pred_df, aes(y = fit), color = "blue", size = 1.2) +
  geom_ribbon(data = pred_df, 
              aes(y = fit, 
                  ymin = fit - 1.96*se, 
                  ymax = fit + 1.96*se),
              alpha = 0.2, fill = "blue") +
  scale_y_log10() +
  labs(title = "Exponential Growth (Log Scale)",
       subtitle = paste("Linear trend on log scale shows exponential growth"),
       x = "Days since first case",
       y = "Confirmed Cases (log10)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# ==== FIGURE 3: GROWTH RATE WITH CI ====
fig3 <- ggplot() +
  geom_hline(yintercept = small_r, color = "green", size = 1.2, linetype = "solid") +
  geom_hline(yintercept = r_CI[1], color = "lightgreen", size = 1, linetype = "dashed") +
  geom_hline(yintercept = r_CI[2], color = "lightgreen", size = 1, linetype = "dashed") +
  geom_rect(aes(xmin = 0, xmax = max(pred_df$day), ymin = r_CI[1], ymax = r_CI[2]),
            alpha = 0.2, fill = "green") +
  annotate("text", x = max(pred_df$day) * 0.5, y = small_r * 1.85,
           label = paste("r =", round(small_r, 4), "/day"),
           size = 5, color = "black", fontface = "bold") +
  annotate("text", x = max(pred_df$day) * 0.5, y = small_r * 1.70,
           label = paste("95% CI: [", round(r_CI[1], 4), ", ", round(r_CI[2], 4), "]"),
           size = 4, color = "black", fontface = "bold") +
  xlim(0, max(pred_df$day)) +
  ylim(0, small_r * 2) +
  labs(title = "Growth Rate (r) with 95% Confidence Interval",
       subtitle = paste("Doubling every", round(doubling, 2), "days"),
       x = "Days since first case",
       y = "Growth Rate per day") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# ==== FIGURE 4: R0 WITH 3 SI SCENARIOS ====
SI_scenarios$scenario <- factor(SI_scenarios$scenario, 
                                levels = c("Low (95% CI Lower)", "Primary Estimate", "High (95% CI Upper)"))

fig4 <- ggplot(SI_scenarios, aes(x = scenario, y = R0, color = scenario)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = R0_lower, ymax = R0_upper), 
                width = 0.2, size = 1.5) +
  geom_hline(yintercept = 1, color = "red", size = 1.2, linetype = "dotted") +
  scale_color_manual(values = c("Low (95% CI Lower)" = "#3498db", "Primary Estimate" = "#f39c12", "High (95% CI Upper)" = "#e74c3c"),
                     guide = "none") +
  annotate("text", x = 0.6, y = 1.1, 
           label = "Critical\nthreshold",
           size = 3, color = "red") +
  ylim(0, max(SI_scenarios$R0_upper) * 1.2) +
  labs(title = "R0 Estimates: Sensitivity Analysis Using Serial Interval 95% CI",
       subtitle = "Primary estimate SI = 15.4 days | Sensitivity bounds: 13.2 - 17.5 days (95% CI from literature)",
       x = "Serial Interval Scenario",
       y = "R0") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 11, face = "bold"))

# ==== FIGURE 5: PROJECTED EPIDEMIC UNDER DIFFERENT R0 SCENARIOS ====

# Use a reasonable starting point for projection
# Project forward from the last observed day
last_day <- max(data$day)
last_cases <- data$confirmed_cases[data$day == last_day]

# Create projection data from last observed day forward
projection_days <- seq(last_day, last_day + 30, by = 1)

projections_list <- list()

for (i in 1:nrow(SI_scenarios)) {
  scenario_name <- SI_scenarios$scenario[i]
  SI_value <- SI_scenarios$SI[i]
  R0_value <- SI_scenarios$R0[i]
  
  # Calculate growth rate for this R0: r = ln(R0) / SI
  r_from_R0 <- log(R0_value) / SI_value
  
  # Project cases using exponential model
  projected_cases <- last_cases * exp(r_from_R0 * (projection_days - last_day))
  
  projections_list[[i]] <- data.frame(
    day = projection_days,
    cases = projected_cases,
    scenario = scenario_name,
    R0 = R0_value,
    SI = SI_value,
    type = "Projected"
  )
}

projections_df <- do.call(rbind, projections_list)

# Add observed data
observed_df <- data.frame(
  day = data$day,
  cases = data$confirmed_cases,
  scenario = "Observed",
  R0 = NA,
  SI = NA,
  type = "Observed"
)

# Combine observed and projected
combined_df <- rbind(
  data.frame(observed_df, stringsAsFactors = FALSE),
  projections_df
)

# Create the plot
fig5 <- ggplot() +
  geom_point(data = observed_df, aes(x = day, y = cases), 
             color = "darkred", size = 2, alpha = 0.6, name = "Observed") +
  geom_line(data = projections_df, aes(x = day, y = cases, color = scenario, linetype = scenario), 
            size = 1.2) +
  scale_color_manual(values = c("Low (95% CI Lower)" = "#3498db", "Primary Estimate" = "#f39c12", "High (95% CI Upper)" = "#e74c3c"),
                     name = "Scenario") +
  scale_linetype_manual(values = c("Low (95% CI Lower)" = "solid", "Primary Estimate" = "dashed", "High (95% CI Upper)" = "dotted"),
                        name = "Scenario") +
  scale_y_log10() +
  labs(title = "Observed Data + Projected Epidemic Curves: Sensitivity Analysis",
       subtitle = paste("Primary estimate SI = 15.4 days (R0 = ", round(SI_scenarios$R0[2], 2), 
                        ") | Low SI = 13.2 days (R0 = ", round(SI_scenarios$R0[1], 2),
                        ") | High SI = 17.5 days (R0 = ", round(SI_scenarios$R0[3], 2), ")"),
       x = "Days since first case",
       y = "Cases (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.position = "right")

# ==== DISPLAY ALL FIGURES ====
grid.arrange(fig1, fig2, fig3, fig4, fig5, nrow = 3, ncol = 2)

# ==== SAVE FIGURES ====
ggsave("01_GLM_Fit.png", fig1, width = 8, height = 6, dpi = 300)
ggsave("02_Log_Scale.png", fig2, width = 8, height = 6, dpi = 300)
ggsave("03_Growth_Rate_CI.png", fig3, width = 8, height = 6, dpi = 300)
ggsave("04_R0_Scenarios.png", fig4, width = 8, height = 6, dpi = 300)
ggsave("05_Projected_Epidemics.png", fig5, width = 10, height = 6, dpi = 300)

print("Figures saved to your working directory!")

