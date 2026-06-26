#======================================================================
# INTEGRATED ANALYSIS: GLM + SIRD SIMULATION COMPARISON
#======================================================================
# This script:
# 1. Fits GLM to observed data (from drc_sitrep.csv)
# 2. Runs SIRD simulation
# 3. Compares both on the same plot
#======================================================================

library(MASS)
library(ggplot2)
library(gridExtra)
library(deSolve)
library(tidyr)
library(dplyr)

#======================================================================
# PART 1: OBSERVED DATA & GLM FITTING (from your original code)
#======================================================================

# Read observed data
data <- read.csv("../data/drc_sitrep.csv")
data$date <- as.Date(data$date)
data$day <- as.numeric(data$date - data$date[[1]])

# Fit GLM
glm_fit <- glm.nb(confirmed_cases ~ day, data = data)
small_r <- coef(glm_fit)[[2]]
intercept <- coef(glm_fit)[[1]]
doubling <- log(2) / small_r 

summary(glm_fit)

# Get confidence intervals
CI <- confint(glm_fit)
r_CI <- CI[2, ]

# SI scenarios for sensitivity analysis
SI_scenarios <- data.frame(
  scenario = c("Low (95% CI Lower)", "Primary Estimate", "High (95% CI Upper)"),
  SI = c(13.2, 15.4, 17.5)
)

## We are combining lower CI with lower SI estimate
## ... and assuming kappa=0 (fixed generation interval)
R_phen <- function(rho, kappa){
	if (kappa==0) return(exp(rho))
	return((1+kappa*rho)^(1/kappa))
}

print(r_CI)

## Use kappa=1 when we are trying to match simulation
kappa=1
R0_main <- R_phen(small_r * SI_scenarios$SI[[2]], kappa)
R0_lower <- R_phen(exp(r_CI[[1]] * SI_scenarios$SI[[1]]), kappa)
R0_upper <- R_phen(exp(r_CI[[2]] * SI_scenarios$SI[[3]]), kappa)

#======================================================================
# CALCULATE BETA AND GAMMA FROM GLM DATA
#======================================================================

## Calculate our example value of beta gamma from formula using R0 and r
# Common scenario: r = β - γ

exp(intercept)

R0 <- R0_main

# Step 1: Calculate gamma
gamma <- small_r / (R0 - 1)

# Step 2: Now calculate beta
beta <- R0 * gamma

## FIXME: Get rid of beta_gamma without changing anything else
# Step 3: Calculate beta*gamma (the transmission-recovery rate product)
beta_gamma <- beta * gamma
print(beta_gamma)

# Store these for use in SIRD simulation
calculated_beta <- beta
calculated_gamma <- gamma

# Fit GLM model
#glm_fit <- glm(confirmed_cases ~ day, data = data)

ci <- confint(glm_fit)
small_r_low <- ci[2, 1]
small_r_high <- ci[2, 2]

# Create data frame
plot_data <- data.frame(
  parameter = "Growth rate (r)",
  estimate = small_r,
  lower = small_r_low,
  upper = small_r_high
)

# Create forest plot with value labels
library(ggplot2)

ggplot(plot_data, aes(x = estimate, y = parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 5, color = "darkblue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.15, size = 1.2, color = "darkblue") +
  geom_text(aes(x = estimate, label = paste0(round(estimate, 4))), 
            vjust = -0.8, size = 4, fontface = "bold") +
  geom_text(aes(x = lower, label = paste0(round(lower, 4))), 
            vjust = -5.1, size = 3.5, color = "darkblue") +
  geom_text(aes(x = upper, label = paste0(round(upper, 4))), 
            vjust = -5.1, size = 3.5, color = "darkblue") +
  xlim(0.05, NA) +  # ADD THIS LINE - starts at 0.05, NA lets it auto-adjust max
  labs(
    title = "Growth Rate (r) with 95% Confidence Interval",
    x = "Growth Rate (r)",
    y = ""
  ) +
  theme_minimal() 

# Print values to console as well
cat("Growth Rate (r) Summary:\n")
cat("Low (95% CI):  ", round(small_r_low, 4), "\n")
cat("Mid (Estimate):", round(small_r, 4), "\n")
cat("High (95% CI): ", round(small_r_high, 4), "\n")



print("===== CALCULATED BETA AND GAMMA FROM GLM DATA =====")
print(paste("Beta (transmission rate):", round(calculated_beta, 4)))
print(paste("Gamma (recovery rate):", round(calculated_gamma, 4)))
print(paste("Beta*Gamma (product):", round(beta_gamma, 6)))
print("")

print("===== GLM ANALYSIS =====")
print(paste("Small r:", round(small_r, 4), "per day"))
print(paste("95% CI: [", round(r_CI[[1]], 4), ", ", round(r_CI[[2]], 4), "]"))
print(paste("Doubling time:", round(doubling, 2), "days"))
print("")

print("===== R0 ESTIMATES FOR 3 SI SCENARIOS =====")
for (i in 1:nrow(SI_scenarios)) {
  if (i == 2) {
    print(paste("*** PRIMARY ESTIMATE *** ", SI_scenarios$scenario[i], " SI (", SI_scenarios$SI[i], 
                " days): R0 =", round(R0_main[i], 3),
                " 95% CI: [", round(R0_lower[i], 3), 
                ", ", round(R0_upper[i], 3), "]"))
  } else {
    print(paste(SI_scenarios$scenario[i], " SI (", SI_scenarios$SI[i], 
                " days): R0 =", round(R0_main[i], 3),
                " 95% CI: [", round(R0_lower[i], 3), 
                ", ", round(R0_upper[i], 3), "]"))
  }
}
print("")

#======================================================================
# PART 2: SIRD SIMULATION (using calculated beta and gamma)
#======================================================================

PLOT_MODE <- "report"
BASE_SIZE <- switch(PLOT_MODE, report = 14, slides = 18)
theme_set(theme_minimal(base_size = BASE_SIZE))

# SIRD MODEL
sird_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    I_total <- I_s + I_a
    trans <- beta * S * I_total / N
    
    dS  <- -trans
    dI_s <- p_sym * trans - gamma * I_s
    dI_a <- (1 - p_sym) * trans - gamma * I_a
    dR  <- (1 - alpha) * gamma * I_s + gamma * I_a
    dD  <- alpha * gamma * I_s
    
    list(c(dS, dI_s, dI_a, dR, dD), incidence = trans, prevalence = I_total)
  })
}

# PARAMETERS - NOW USING CALCULATED VALUES FROM GLM DATA
# (instead of placeholder values)

N <- 1e4
p_sym <- 0.6

# Use the beta and gamma calculated from GLM data
parameters <- c(beta = calculated_beta,      # NOW USING CALCULATED VALUE
                gamma = calculated_gamma,    # NOW USING CALCULATED VALUE
                alpha = 0.05,
                p_sym = 0.6,
                N = N)

# Initial conditions
I0 <- intercept
I_s0 <- p_sym * I0
I_a0 <- (1 - p_sym) * I0

initial_state <- c(S = N-I0, I_s = I_s0, I_a = I_a0, R = 0, D = 0)

# Time points - match your observed data range
t_max <- max(data$day) + 100
times <- seq(0, t_max, by = 1)

# Run SIRD simulation
sird_output <- ode(y = initial_state,
                   times = times,
                   func = sird_model,
                   parms = parameters,
                   method = "lsoda")

sird_output <- as.data.frame(sird_output)

# Extract CUMULATIVE infected: N - S (everyone who has ever been infected)
# This matches your observed data which is CUMULATIVE confirmed cases
# Cumulative = Current (I_s + I_a) + Recovered + Dead = N - S
sird_cumulative <- sird_output %>%
  mutate(cumulative_infected = N - S) %>%  # Total who have EVER been infected
  select(time, cumulative_infected) %>%
  rename(day = time, simulated_cases = cumulative_infected)

print("===== SIRD SIMULATION PARAMETERS (from GLM-derived β and γ) =====")
print(paste("Beta (transmission):", round(parameters["beta"], 4), " (from GLM data)"))
print(paste("Gamma (recovery):", round(parameters["gamma"], 4), " (from GLM data)"))
print(paste("Beta*Gamma (product):", round(calculated_beta * calculated_gamma, 6)))
print(paste("Alpha (CFR):", parameters["alpha"]))
print(paste("P_sym (proportion symptomatic):", parameters["p_sym"]))
print(paste("N (population):", N))
print(paste("Initial infected:", I0))
print("")

#======================================================================
# CALCULATE FINAL SIZE FROM THEORY (using calculated β and γ)
#======================================================================

# Calculate R0 from calculated beta and gamma
R0_theory <- parameters["beta"] / parameters["gamma"]

print("===== THEORETICAL FINAL SIZE =====")
print(paste("R0 = β/γ =", round(parameters["beta"], 4), "/", round(parameters["gamma"], 4), "=", round(R0_theory, 3)))
print(paste("Note: β*γ =", round(beta_gamma, 6), " (product used in certain transmission models)"))
print("")

# Solve final size equation: Z = 1 - exp(-R0 * Z)
# Z = attack rate (proportion of population ever infected)
# Using numerical solution
solve_final_size <- function(R0, tol = 1e-6) {
  # Newton-Raphson method to solve Z = 1 - exp(-R0 * Z)
  Z <- 0.5  # Initial guess
  for (i in 1:100) {
    f <- Z - 1 + exp(-R0 * Z)
    f_prime <- 1 + R0 * exp(-R0 * Z)
    Z_new <- Z - f / f_prime
    if (abs(Z_new - Z) < tol) break
    Z <- Z_new
  }
  return(Z)
}

Z_final <- solve_final_size(R0_theory)
attack_rate <- Z_final * 100
total_ever_infected <- N * Z_final
never_infected <- N * (1 - Z_final)

print(paste("Attack rate (Z):", round(Z_final, 4), "or", round(attack_rate, 1), "%"))
print(paste("Total ever infected:", round(total_ever_infected, 0)))
print(paste("Never infected:", round(never_infected, 0)))
print("")

# Expected recovered and deaths
expected_recovered <- total_ever_infected * (1 - parameters["alpha"])
expected_deaths <- total_ever_infected * parameters["alpha"]

print(paste("Expected recovered:", round(expected_recovered, 0)))
print(paste("Expected deaths (alpha =", parameters["alpha"], "):", round(expected_deaths, 0)))
print("")

#======================================================================
# COMPARE THEORETICAL FINAL SIZE WITH SIMULATION OUTCOME
#======================================================================

# Extract actual final values from simulation
final_row <- tail(sird_output, 1)

simulated_S_final <- final_row$S
simulated_ever_infected <- N - simulated_S_final
simulated_recovered <- final_row$R
simulated_deaths <- final_row$D

print("===== SIMULATION VS THEORY COMPARISON =====")
print("")
print("THEORETICAL (from R0):")
print(paste("  Ever infected:", round(total_ever_infected, 0)))
print(paste("  Attack rate:", round(attack_rate, 1), "%"))
print(paste("  Expected recovered:", round(expected_recovered, 0)))
print(paste("  Expected deaths:", round(expected_deaths, 0)))
print("")

print("SIMULATED (from ODE):")
print(paste("  Ever infected:", round(simulated_ever_infected, 0)))
print(paste("  Attack rate:", round((simulated_ever_infected/N)*100, 1), "%"))
print(paste("  Simulated recovered:", round(simulated_recovered, 0)))
print(paste("  Simulated deaths:", round(simulated_deaths, 0)))
print("")

print("DIFFERENCE:")
print(paste("  Ever infected diff:", round(simulated_ever_infected - total_ever_infected, 0), 
            "people (", round((simulated_ever_infected - total_ever_infected)/total_ever_infected*100, 1), "%)"))
print("")

# Create GLM predictions
newdata <- data.frame(day = seq(min(data$day), max(data$day), by = 0.5))
pred <- predict(glm_fit, newdata = newdata, type = "response", se.fit = TRUE)

pred_df <- data.frame(
  day = newdata$day,
  glm_fit = pred$fit,
  se = pred$se.fit
)

# Merge all data for comparison
comparison_df <- data.frame(
  day = data$day,
  observed = data$confirmed_cases
) %>%
  left_join(pred_df, by = "day") %>%
  left_join(sird_cumulative, by = "day")

# Calculate residuals
comparison_df$glm_residual <- comparison_df$observed - comparison_df$glm_fit
comparison_df$sird_residual <- comparison_df$observed - comparison_df$simulated_cases

print("===== GOODNESS OF FIT =====")
print("GLM Model:")
print(paste("  RMSE:", round(sqrt(mean(comparison_df$glm_residual^2, na.rm = TRUE)), 2)))
print(paste("  Mean Absolute Error:", round(mean(abs(comparison_df$glm_residual), na.rm = TRUE), 2)))

print("SIRD Simulation:")
print(paste("  RMSE:", round(sqrt(mean(comparison_df$sird_residual^2, na.rm = TRUE)), 2)))
print(paste("  Mean Absolute Error:", round(mean(abs(comparison_df$sird_residual), na.rm = TRUE), 2)))
print("")

#======================================================================
# FIGURE 7: FINAL SIZE BREAKDOWN
#======================================================================

# Create data for final size visualization
final_size_data <- data.frame(
  Compartment = c("Never Infected\n(Susceptible)", "Currently Infected\n(I_s + I_a)", 
                  "Recovered\n(R)", "Deaths\n(D)"),
  Count = c(simulated_S_final, 
            final_row$I_s + final_row$I_a,
            simulated_recovered, 
            simulated_deaths),
  Type = c("Not Infected", "Currently Infected", "Recovered", "Deaths")
)

# Create ordered factor for stacking
final_size_data$Compartment <- factor(final_size_data$Compartment,
                                      levels = c("Never Infected\n(Susceptible)", 
                                                 "Currently Infected\n(I_s + I_a)",
                                                 "Recovered\n(R)", 
                                                 "Deaths\n(D)"))

# Color palette matching your SIRD model
palette_final <- c("Not Infected" = "#3B8BD4",     # Blue - Susceptible
                   "Currently Infected" = "#E85A30",  # Orange - Infected
                   "Recovered" = "#639922",            # Green - Recovered
                   "Deaths" = "#A32D2D")               # Red - Deaths

fig7 <- ggplot(final_size_data, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", size = 1.5) +
  geom_text(aes(label = paste(Compartment, "\n(", round(Count), " people)")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = palette_final, guide = "none") +
  coord_flip() +
  labs(title = "Final Epidemic Size Breakdown",
       subtitle = paste("Total population = ", N, 
                        " | Ever infected = ", round(simulated_ever_infected, 0),
                        " (", round((simulated_ever_infected/N)*100, 1), "%)"),
       x = "", y = "Number of People") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#======================================================================
# FIGURE 8: CUMULATIVE INFECTED OVER TIME (showing approach to final size)
#======================================================================

# Add theoretical final size line to cumulative data
fig8 <- ggplot() +
  geom_line(data = sird_cumulative, aes(x = day, y = simulated_cases, color = "Simulated"), 
            size = 1.2) +
  geom_hline(yintercept = total_ever_infected, color = "#A32D2D", size = 1.2, linetype = "dashed",
             aes(color = "Theoretical Final Size")) +
  geom_point(data = data, aes(x = day, y = confirmed_cases, color = "Observed"), 
             size = 2.5, alpha = 0.7) +
  annotate("text", x = max(sird_cumulative$day) * 0.7, y = total_ever_infected + 200,
           label = paste("Theoretical final size =", round(total_ever_infected, 0), 
                         "\nAttack rate =", round(attack_rate, 1), "%"),
           size = 3.5, color = "#A32D2D", fontface = "bold") +
  scale_color_manual(name = "Data",
                     values = c("Simulated" = "#27500A", 
                                "Theoretical Final Size" = "#A32D2D",
                                "Observed" = "darkred")) +
  labs(title = "Cumulative Epidemic Curve Approaching Final Size",
       subtitle = paste("R0 =", round(R0_theory, 2), 
                        " | Predicted final size = ", round(total_ever_infected, 0), " people"),
       x = "Days since first case",
       y = "Cumulative Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

fig6_linear <- ggplot() +
  geom_point(data = data, aes(x = day, y = confirmed_cases), 
             color = "darkred", size = 2.5, alpha = 0.7, name = "Observed") +
  geom_line(data = pred_df, aes(x = day, y = glm_fit, color = "GLM Fit", linetype = "GLM Fit"), 
            size = 1.2) +
  geom_ribbon(data = pred_df, 
              aes(x = day, ymin = glm_fit - 1.96*se, ymax = glm_fit + 1.96*se),
              alpha = 0.15, fill = "blue", name = "GLM 95% CI") +
  geom_line(data = sird_cumulative, aes(x = day, y = simulated_cases, color = "SIRD Simulation", linetype = "SIRD Simulation"), 
            size = 1.2) +
  scale_color_manual(name = "Model",
                     values = c("GLM Fit" = "#378ADD", "SIRD Simulation" = "#27500A")) +
  scale_linetype_manual(name = "Model",
                        values = c("GLM Fit" = "solid", "SIRD Simulation" = "dashed")) +
  labs(title = "Comparison: Observed Data vs GLM vs SIRD Simulation",
       subtitle = "Red dots = observed cumulative confirmed, lines = model predictions (N - S)",
       x = "Days since first case",
       y = "Cumulative Confirmed Cases (Ever Infected)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Enhanced Figure 6: Standalone with Final Size (OBSERVED vs SIRD ONLY)
fig6_enhanced <- ggplot() +
  # Observed data
  geom_point(data = data, aes(x = day, y = confirmed_cases, color = "Observed"), 
             size = 3.5, alpha = 0.85) +
  # SIRD simulation
  geom_line(data = sird_cumulative, aes(x = day, y = simulated_cases, color = "SIRD Simulation", linetype = "SIRD Simulation"), 
            size = 1.4) +
  # Theoretical final size line
  geom_hline(aes(yintercept = total_ever_infected, color = "Final Size", linetype = "Final Size"), 
             size = 1.3) +
  # Scale and theme
  scale_color_manual(name = "",
                     values = c("Observed" = "#A32D2D",
                                "SIRD Simulation" = "#1E8449",
                                "Final Size" = "#E67E22")) +
  scale_linetype_manual(name = "",
                        values = c("SIRD Simulation" = "dashed",
                                   "Final Size" = "dotted")) +
  labs(title = "Observed Data vs SIRD Simulation",
       subtitle = paste("Final Size: Z = 1 - exp(-R0 × Z), where R0 =", round(R0_theory, 2), 
                        "| Final size =", round(total_ever_infected, 0), "people (",  round(attack_rate, 1), "%)"),
       x = "Days since first case",
       y = "Cumulative Confirmed Cases") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10),
        legend.position = "right",
        legend.box = "vertical",
        legend.title = element_blank(),
        axis.title = element_text(size = 11))

#======================================================================
# ALL FIGURES TOGETHER
#======================================================================

# Create GLM predictions for display
newdata_all <- data.frame(day = seq(min(data$day), max(data$day), by = 0.5))
pred_all <- predict(glm_fit, newdata = newdata_all, type = "response", se.fit = TRUE)
pred_df_all <- data.frame(day = newdata_all$day, fit = pred_all$fit, se = pred_all$se.fit)

# Original figures (abbreviated for space)
fig1 <- ggplot(data, aes(x = day, y = confirmed_cases)) +
  geom_point(color = "darkred", size = 2, alpha = 0.7) +
  geom_line(data = pred_df_all, aes(y = fit), color = "blue", size = 1) +
  geom_ribbon(data = pred_df_all, aes(y = fit, ymin = fit - 1.96*se, ymax = fit + 1.96*se),
              alpha = 0.2, fill = "blue") +
  labs(title = "GLM Fit", x = "Days", y = "Cases") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 10))

fig4 <- ggplot(SI_scenarios, aes(x = scenario, y = R0, color = scenario)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = R0_lower, ymax = R0_upper), width = 0.2, size = 1.2) +
  geom_hline(yintercept = 1, color = "red", size = 1, linetype = "dotted") +
  scale_color_manual(values = c("Low (95% CI Lower)" = "#3498db", "Primary Estimate" = "#f39c12", "High (95% CI Upper)" = "#e74c3c"),
                     guide = "none") +
  ylim(0, max(R0_upper) * 1.2) +
  labs(title = "R0 Sensitivity", x = "Scenario", y = "R0") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 10),
                          axis.text.x = element_text(size = 8))

# Display Figure 6 STANDALONE (most important figure)
print(fig6_enhanced)

# Then display other figures in grid
grid.arrange(fig1, fig4, fig7, fig8, nrow = 2, ncol = 2)

#======================================================================
# SAVE ALL FIGURES
#======================================================================

ggsave("06_MAIN_Observed_vs_SIRD_with_Final_Size.png", fig6_enhanced, width = 14, height = 8, dpi = 300)
ggsave("07_Final_Size_Breakdown.png", fig7, width = 10, height = 6, dpi = 300)
ggsave("08_Cumulative_Final_Size.png", fig8, width = 10, height = 7, dpi = 300)

print("===== MAIN FIGURES SAVED =====")
print("06_MAIN_Observed_vs_SIRD_with_Final_Size.png - MAIN FIGURE: Observed vs SIRD simulation with final size formula (14x8)")
print("07_Final_Size_Breakdown.png - Final size breakdown by compartment")
print("08_Cumulative_Final_Size.png - Cumulative curve approaching final size")

## In future, we can code like this to keep your scripts small and clear
## But not today
## saveRDS(glm_fit, final_size_data)
