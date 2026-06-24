
library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)

# ============================================================================
# SIRD MODEL: S → I_s (symptomatic) and I_a (asymptomatic) → R, D
# ============================================================================

sird_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Total infected (both transmit)
    I_total <- I_s + I_a
	 trans <-  beta * S * I_total / N
    
    # Five differential equations
    dS  <- -trans
    dI_s <- p_sym * trans - gamma * I_s
    dI_a <- (1 - p_sym) * trans - gamma * I_a
    dR  <- (1 - alpha) * gamma * I_s + gamma * I_a
    dD  <- alpha * gamma * I_s
    
    list(c(dS, dI_s, dI_a, dR, dD))
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
times <- seq(0, 100, by = 1)

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

sirdPlot <- ggplot(output_long, aes(x = time, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  labs(title = "SIRD Model: S => I_s, I_a => R, D",
       ## Do not hard code!! 🙂
       subtitle = "beta=0.4, gamma=0.2, alpha=0.05, p_sym=0.6, N=100",
       x = "Time (days)", 
       y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = palette)

print(sirdPlot)
print(sirdPlot + output_long|> filter(time>2) + scale_y_log10())

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

## How do we know we're running long enough?
get_epidemic_size <- function(beta, gamma, alpha, p_sym, N, I0, t_max = 100) {
  
  parameters <- c(beta = beta, gamma = gamma, alpha = alpha, p_sym = p_sym, N = N)
  
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
  
  epidemic_size <- N - final_row$S
  deaths <- final_row$D
  
  return(c(epidemic_size = epidemic_size, deaths = deaths))
}


# ============================================================================
# GRID SEARCH: Beta vs Gamma
# ============================================================================

beta_values  <- seq(0.1, 0.6, by = 0.1)
gamma_values <- seq(0.1, 0.4, by = 0.1)

param_grid <- expand.grid(beta = beta_values, gamma = gamma_values)
param_grid$epidemic_size <- NA
param_grid$deaths <- NA

for (i in 1:nrow(param_grid)) {
  result <- get_epidemic_size(
    beta  = param_grid$beta[i],
    gamma = param_grid$gamma[i],
    alpha = 0.05,
    p_sym = 0.6,
    N     = 100,
    I0    = 10,
    t_max = 100
  )
  
  param_grid$epidemic_size[i] <- result["epidemic_size"]
  param_grid$deaths[i] <- result["deaths"]
}

param_grid$R0 <- param_grid$beta / param_grid$gamma

head(param_grid)

# Plot 1: Epidemic size vs R0
ggplot(param_grid, aes(x = R0, y = epidemic_size)) +
  geom_point(aes(color = factor(gamma)), alpha = 0.6, size = 3) +
  geom_line(aes(color = factor(gamma))) +
  labs(title = "Epidemic size vs R0",
       x = "R0 (= beta/gamma)", 
       y = "Epidemic size (out of N=100)",
       color = "gamma") +
  theme_minimal()

# Plot 2: Deaths vs R0
ggplot(param_grid, aes(x = R0, y = deaths)) +
  geom_point(aes(color = factor(gamma)), alpha = 0.6, size = 3) +
  geom_line(aes(color = factor(gamma))) +
  labs(title = "Total deaths vs R0",
       x = "R0 (= beta/gamma)", 
       y = "Total deaths",
       color = "gamma") +
  theme_minimal()


# ============================================================================
# GRID SEARCH: Alpha (CFR) vs Population Size
# ============================================================================

alpha_values <- seq(0, 0.5, by = 0.1)
N_values <- c(50, 100, 200, 500, 1000)

param_grid2 <- expand.grid(alpha = alpha_values, N = N_values)
param_grid2$I0 <- 0.1 * param_grid2$N
param_grid2$epidemic_size <- NA
param_grid2$deaths <- NA

for (i in 1:nrow(param_grid2)) {
  result <- get_epidemic_size(
    beta  = 0.4,
    gamma = 0.2,
    alpha = param_grid2$alpha[i],
    p_sym = 0.6,
    N     = param_grid2$N[i],
    I0    = param_grid2$I0[i],
    t_max = 100
  )
  
  param_grid2$epidemic_size[i] <- result["epidemic_size"]
  param_grid2$deaths[i] <- result["deaths"]
}

param_grid2$epidemic_proportion <- param_grid2$epidemic_size / param_grid2$N

head(param_grid2)

# Plot 3: Epidemic size vs CFR
ggplot(param_grid2, aes(x = alpha, y = epidemic_size, color = factor(N))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Epidemic size vs alpha (CFR)",
       x = "alpha (Case Fatality Rate)",
       y = "Epidemic size (head count)",
       color = "Population (N)") +
  theme_minimal()

# Plot 4: Epidemic proportion vs CFR
ggplot(param_grid2, aes(x = alpha, y = epidemic_proportion, color = factor(N))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Epidemic proportion vs alpha (CFR)",
       x = "alpha (Case Fatality Rate)",
       y = "Epidemic proportion (size/N)",
       color = "Population (N)") +
  theme_minimal() +
  ylim(0, 1)

# Plot 5: Total deaths vs CFR
ggplot(param_grid2, aes(x = alpha, y = deaths, color = factor(N))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Total deaths vs alpha (CFR)",
       x = "alpha (Case Fatality Rate)",
       y = "Total deaths",
       color = "Population (N)") +
  theme_minimal()

