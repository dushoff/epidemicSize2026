library(deSolve)



sird_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - gamma * I
    dR <-  (1 - alpha) * gamma * I
    dD <-  alpha * gamma * I
    
    list(c(dS, dI, dR, dD))
  })
}


N <- 100
S <- 0.9*N
I <- 0.1*N


# parameters
parameters <- c(beta = 0.4, gamma = 0.2, alpha = 0.05, N)



# initial conditions
initial_state <- c(S, I, R = 0, D = 0)

# time points
times <- seq(0, 100, by = 1)

initial_state <- c(S = 90, I = 10, R = 0, D = 0)

output <- ode(y = initial_state,
              times = times,
              func = sird_model,
              parms = parameters,
              method = "lsoda")

output <- as.data.frame(output)


head(output)



# ploting

library(ggplot2)
library(tidyr)

output_long <- pivot_longer(output, 
                            cols = c(S, I, R, D), 
                            names_to = "compartment", 
                            values_to = "count")

ggplot(output_long, aes(x = time, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  labs(title = "SIRD model: beta=0.4, gamma=0.2, alpha=0.05, N=100",
       x = "Time", y = "Number of people") +
  theme_minimal()


final_row <- tail(output, 1)
final_row 

# epidemic size is the number of people that have been infected ever. 
# so epidemic size = population - susceptible at end
epidemic_size <- N - final_row$S
epidemic_size



# function to do the simulation and get epidemic size at the end
get_epidemic_size <- function(beta, gamma, alpha, N, I0, t_max = 100) {
  
  parameters <- c(beta = beta, gamma = gamma, alpha = alpha, N = N)
  initial_state <- c(S = N - I0, I = I0, R = 0, D = 0)
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

# grid search on beta and gamma 
# other parameters fixed alpha = 0.05 and N = 100, I0 = 10
beta_values  <- seq(0.1, 0.6, by = 0.1)
gamma_values <- seq(0.1, 0.4, by = 0.1)

param_grid <- expand.grid(beta = beta_values, gamma = gamma_values)

head(param_grid)
nrow(param_grid)

param_grid$epidemic_size <- NA  # placeholder column


for (i in 1:nrow(param_grid)) {
  param_grid$epidemic_size[i] <- get_epidemic_size(
    beta  = param_grid$beta[i],
    gamma = param_grid$gamma[i],
    alpha = 0.05,
    N     = 100,
    I0    = 10,
    t_max = 100
  )
}

head(param_grid)

# compute R0
param_grid$R0 <- param_grid$beta / param_grid$gamma
head(param_grid)


ggplot(param_grid, aes(x = R0, y = epidemic_size)) +
  geom_point(aes(color = factor(gamma)), alpha=0.3, size = 3) +
  geom_line(aes(color = factor(gamma))) +
  labs(title = "Epidemic size vs R0",
       x = "R0 (= beta/gamma)", 
       y = "Epidemic size (out of N=100)",
       color = "gamma") +
  theme_minimal()



alpha_values <- seq(0, 0.5, by = 0.1)
N_values <- c(50, 100, 200, 500, 1000)

param_grid2 <- expand.grid(alpha = alpha_values, N = N_values)

# I0 proportional to N (10%)
param_grid2$I0 <- 0.1 * param_grid2$N

param_grid2$epidemic_size <- NA
param_grid2$deaths <- NA

for (i in 1:nrow(param_grid2)) {
  result <- get_epidemic_size(
    beta  = 0.4,
    gamma = 0.2,
    alpha = param_grid2$alpha[i],
    N     = param_grid2$N[i],
    I0    = param_grid2$I0[i],
    t_max = 100
  )
  
  param_grid2$epidemic_size[i] <- result["epidemic_size"]
  param_grid2$deaths[i]        <- result["deaths"]
}

param_grid2$epidemic_proportion <- param_grid2$epidemic_size / param_grid2$N

head(param_grid2)


# Effect of alpha on epidemic_size
ggplot(param_grid2, aes(x = alpha, y = epidemic_size, color = factor(N))) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Epidemic size (head count) vs alpha (IFR)",
       x = "alpha (IFR)",
       y = "Epidemic size",
       color = "N") +
  theme_minimal()


ggplot(param_grid2, aes(x = N, y = epidemic_proportion, color = factor(alpha))) +
  geom_point(size = 2) +
  labs(title = "Epidemic proportion (size/N) vs N",
       x = "N",
       y = "Epidemic proportion",
       color = "alpha") +
  theme_minimal() +
  ylim(0, 1)
