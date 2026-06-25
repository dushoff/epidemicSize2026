library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)

PLOT_MODE <- "report"  # "report" or "slides"

BASE_SIZE <- switch(
  PLOT_MODE,
  report = 14,
  slides = 18
)

theme_set(
  theme_minimal(base_size = BASE_SIZE)
)

# ============================================================================
# SIRD MODEL: S → I_s (symptomatic) and I_a (asymptomatic) → R, D
# ============================================================================

sird_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    I_total <- I_s + I_a
    trans <- beta * S * I_total / N    # incidence: rate of new infections
    
    dS  <- -trans
    dI_s <- p_sym * trans - gamma * I_s
    dI_a <- (1 - p_sym) * trans - gamma * I_a
    dR  <- (1 - alpha) * gamma * I_s + gamma * I_a
    dD  <- alpha * gamma * I_s
    
    list(c(dS, dI_s, dI_a, dR, dD), incidence = trans, prevalence = I_total)
  })
}


# ============================================================================
# PARAMETERS
# ============================================================================

N <- 1e4
p_sym <- 0.6
parameters <- c(beta = 0.4,
                gamma = 0.2,
                alpha = 0.05,
                p_sym = 0.6,
                N = N)

# Initial conditions
I0 <- 1e-4 * N
I_s0 <- p_sym * I0
I_a0 <- (1 - p_sym) * I0

initial_state <- c(S = N-I0, I_s = I_s0, I_a = I_a0, R = 0, D = 0)

# Time points
t_max <- 1e2
times <- seq(0, t_max, by = 1)

# Run ODE solver
output <- ode(y = initial_state,
              times = times,
              func = sird_model,
              parms = parameters,
              method = "lsoda")

output <- (as.data.frame(output))



----------------------------------------------------------------------

head(output)


# ============================================================================
# PLOTTING
# ============================================================================

palette <- c(S = "#3B8BD4", I_s = "#E85A30", I_a = "#0F6E56", 
                                R = "#639922", D = "#A32D2D")

output_long <- pivot_longer(output, 
                            cols = c(S, I_s, I_a, R, D), 
                            names_to = "compartment", 
                            values_to = "count")

output_long <- (output_long
	|> mutate(
		compartment = factor(compartment, levels=(
			c("S", "I_s", "I_a", "R", "D")
		))
	)
)

head(output_long)

sirdPlot_log <- ggplot(output_long %>% filter(time > 2), 
                       aes(x = time, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette) +
  scale_y_log10() +
  labs(title = "SIRD Model (log scale, t > 2)",
       subtitle = sprintf(
         "beta=%.2f, gamma=%.2f, alpha=%.2f, p_sym=%.2f, N=%d",
         parameters["beta"], parameters["gamma"], parameters["alpha"],
         parameters["p_sym"], parameters["N"]),
       x = "Time (days)", y = "Number of people")


print(sirdPlot_log)

ggplot(output,
       aes(x = time, y = incidence)) +
  geom_line(linewidth = 1) +
  labs(title = "Incidence over time",
       subtitle = "Incidence = rate of new infections (beta * S * I_total / N)",
       x = "Time (days)",
       y = "New infections per day")

# Early-growth view: restricted to the early period where the 
# exponential approximation holds (before susceptible depletion kicks in)
ggplot(output |> filter(time <= 20, incidence > 0),
       aes(x = time, y = incidence)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Early incidence growth",
       x = "Time (days)",
       y = "New infections per day")

# ============================================================================
# FINAL OUTCOMES
# ============================================================================

final_row <- tail(output, 1)

epidemic_size <- N - final_row$S
total_deaths <- final_row$D
attack_rate <- (epidemic_size / N) * 100

print(paste("Final Susceptible:", round(final_row$S, 2)))
print(paste("Total Infected:", round(epidemic_size, 2)))
print(paste("Total Recovered:", round(final_row$R, 2)))
print(paste("Total Deaths:", round(total_deaths, 2)))
print(paste("Attack Rate:", round(attack_rate, 1), "%"))


# ============================================================================
# FUNCTION: Get epidemic outcomes
# ============================================================================

get_epidemic_size <- function(beta, gamma, alpha, p_sym, N, I0, t_max, 
                              tol = 1e-2) {
  
  parameters <- c(beta = beta, gamma = gamma, alpha = alpha, p_sym = p_sym, N = N, t_max=t_max)
  
  I_s0 <- p_sym * I0
  I_a0 <- (1 - p_sym) * I0
  initial_state <- c(S = N - I0, I_s = I_s0, I_a = I_a0, R = 0, D = 0)
  
  times <- seq(0, t_max, by = 1)
  
  output <- ode(y = initial_state,
                times = times,
                func = sird_model,
                parms = parameters,
                method = "lsoda")
  
  output <- as.data.frame(output)
  final_row <- tail(output, 1)
  
  # Check whether the epidemic has actually burned out by t_max.
  # If I_s + I_a is still above tolerance, the run stopped too early and 
  # S/D at final_row are NOT the true final values - more transmission and
  # deaths would still occur beyond t_max.
  I_total_final <- final_row$I_s + final_row$I_a
  converged <- I_total_final < tol
  
  if (!converged) {
    warning(sprintf(
      "Epidemic not finished by t_max = %d (I_total = %.4f at final time). 
      epidemic_size and deaths are likely UNDERESTIMATES. Increase t_max.",
      t_max, I_total_final
    ))
  }
  
  epidemic_size <- N - final_row$S
  deaths <- final_row$D
  
  return(c(epidemic_size = epidemic_size, 
           deaths = deaths, 
           converged = converged))
}


# ============================================================================
# GRID SEARCH: Beta vs Gamma
# ============================================================================

beta_values  <- seq(0.1, 0.6, by = 0.1)
gamma_values <- seq(0.1, 0.4, by = 0.1)

param_grid <- expand.grid(beta = beta_values, gamma = gamma_values)
param_grid$epidemic_size <- NA
param_grid$deaths <- NA
t_max <- 1e3

for (i in 1:nrow(param_grid)) {
  result <- get_epidemic_size(
    beta  = param_grid$beta[i],
    gamma = param_grid$gamma[i],
    alpha = 0.05,
    p_sym = 0.6,
    N     = 100,
    I0    = 10,
    t_max = t_max
  )
  
  param_grid$epidemic_size[i] <- result["epidemic_size"]
  param_grid$deaths[i] <- result["deaths"]
}

param_grid$R0 <- param_grid$beta / param_grid$gamma

head(param_grid)

# Plot 1: Epidemic size vs R0
ggplot(param_grid, aes(x = R0, y = epidemic_size)) +
  geom_point(aes(color = factor(gamma)), alpha = 0.6, size = 3) +
  labs(title = "Epidemic size vs R0",
       x = "R0 (= beta/gamma)", 
       y = "Epidemic size (out of N=100)",
       color = "gamma")

# Plot 2: Deaths vs R0
ggplot(param_grid, aes(x = R0, y = deaths)) +
  geom_point(aes(color = factor(gamma)), alpha = 0.6, size = 3) +
  labs(title = "Total deaths vs R0",
       x = "R0 (= beta/gamma)", 
       y = "Total deaths",
       color = "gamma") 


# ============================================================================
# GRID SEARCH: Alpha (CFR) vs Population Size
# ============================================================================

alpha_values <- seq(0, 0.5, by = 0.1)
N_values <- c(50, 100, 200, 500, 1000)

param_grid2 <- expand.grid(alpha = alpha_values, N = N_values)
param_grid2$I0 <- 0.1 * param_grid2$N
param_grid2$epidemic_size <- NA
param_grid2$deaths <- NA

t_max <- 1e2

for (i in 1:nrow(param_grid2)) {
  result <- get_epidemic_size(
    beta  = 0.4,
    gamma = 0.2,
    alpha = param_grid2$alpha[i],
    p_sym = 0.6,
    N     = param_grid2$N[i],
    I0    = param_grid2$I0[i],
    t_max = t_max
  )
  
  param_grid2$epidemic_size[i] <- result["epidemic_size"]
  param_grid2$deaths[i] <- result["deaths"]
}

param_grid2$epidemic_proportion <- param_grid2$epidemic_size / param_grid2$N

head(param_grid2)


# Plot 3a: Total deaths vs CFR
ggplot(param_grid2, aes(x = alpha, y = deaths, color = factor(N))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Total deaths vs alpha (CFR)",
       x = "alpha (Case Fatality Rate)",
       y = "Total deaths",
       color = "Population (N)")

# Plot 3b: Total deaths vs N, colored by alpha
ggplot(param_grid2, aes(x = N, y = deaths, color = factor(alpha))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Total deaths vs Population Size (N)",
       x = "N (Population size)",
       y = "Total deaths",
       color = "alpha (CFR)")

