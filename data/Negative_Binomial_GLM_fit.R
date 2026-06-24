
#explore data
#see what we have 
#and what we do not have



#install.packages("MASS")


library(MASS)
library(ggplot2)
library(gridExtra)

# Read data
setwd("C:/Users/nkgomelengl/Desktop/MMEDgit/epidemicSize2026/data")
data <- read.csv("drc_sitrep.csv")
data$date <- as.Date(data$date)
data$day <- as.numeric(data$date - data$date[1])

# ==== FIT NEGATIVE BINOMIAL GLM ====
glm_fit <- glm.nb(confirmed_cases ~ day, data = data)

# Extract parameters
small_r <- coef(glm_fit)[2]
intercept <- coef(glm_fit)[1]
SI <- 10  # Ebola serial interval
big_R <- exp(small_r * SI)
doubling <- log(2) / small_r 

# Get confidence intervals
CI <- confint(glm_fit)
r_CI <- CI[2, ]
R0_lower <- exp(r_CI[1] * SI)
R0_upper <- exp(r_CI[2] * SI)

# ==== PRINT RESULTS ====
print("===== GROWTH PARAMETERS =====")
print(paste("Small r:", round(small_r, 4), "per day"))
print(paste("Doubling time:", round(doubling, 2), "days"))
print(paste("Big R0:", round(big_R, 3)))
print(paste("R0 95% CI: [", round(R0_lower, 3), ", ", round(R0_upper, 3), "]"))
print(paste("Serial Interval:", SI, "days"))
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
       subtitle = paste("r =", round(small_r, 4), "/day, Doubling time =", round(doubling, 2), "days"),
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

# ==== FIGURE 3: GROWTH RATE VISUALIZATION ====
growth_data <- data.frame(
  day = pred_df$day,
  growth_rate = small_r,
  doubling_time = doubling
)

fig3 <- ggplot(growth_data, aes(x = day, y = growth_rate)) +
  geom_hline(yintercept = small_r, color = "green", size = 1.2, linetype = "solid") +
  annotate("text", x = max(growth_data$day) * 0.5, y = small_r + 0.01,
           label = paste("r =", round(small_r, 4), "/day"),
           size = 4, color = "green") +
  ylim(0, small_r * 2) +
  labs(title = "Growth Rate (r) Over Time",
       subtitle = paste("Doubling every", round(doubling, 2), "days"),
       x = "Days since first case",
       y = "Growth Rate per day") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# ==== FIGURE 4: R0 VISUALIZATION ====
fig4 <- ggplot(data.frame(x = c(-1, 1)), aes(x)) +
  geom_segment(aes(x = -0.5, xend = 0.5, y = big_R, yend = big_R),
               size = 2, color = "darkblue") +
  geom_segment(aes(x = -0.5, xend = 0.5, y = R0_lower, yend = R0_lower),
               size = 1, color = "lightblue", linetype = "dashed") +
  geom_segment(aes(x = -0.5, xend = 0.5, y = R0_upper, yend = R0_upper),
               size = 1, color = "lightblue", linetype = "dashed") +
  geom_hline(yintercept = 1, color = "red", size = 1.2, linetype = "dotted") +
  annotate("text", x = 0.6, y = big_R, 
           label = paste("R0 =", round(big_R, 3)),
           size = 5, color = "darkblue", fontface = "bold") +
  annotate("text", x = 0.6, y = 1, 
           label = "Critical threshold",
           size = 4, color = "red") +
  ylim(0, max(big_R * 1.5, 3)) +
  xlim(-1, 1.5) +
  labs(title = "Basic Reproduction Number (R0)",
       subtitle = paste("95% CI: [", round(R0_lower, 3), ", ", round(R0_upper, 3), "]"),
       y = "R0") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_blank())

# ==== DISPLAY ALL FIGURES ====
grid.arrange(fig1, fig2, fig3, fig4, nrow = 2, ncol = 2)

# ==== SAVE FIGURES ====
ggsave("01_GLM_Fit.png", fig1, width = 8, height = 6, dpi = 300)
ggsave("02_Log_Scale.png", fig2, width = 8, height = 6, dpi = 300)
ggsave("03_Growth_Rate.png", fig3, width = 8, height = 6, dpi = 300)
ggsave("04_R0_Estimate.png", fig4, width = 8, height = 6, dpi = 300)

print("Figures saved to your working directory!")

