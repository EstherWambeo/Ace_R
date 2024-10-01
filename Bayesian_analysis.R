#a

# Function to perform Bayesian analysis of count data
bayesian_binomial <- function(Y, N, a = 1, b = 1) {
  # Calculate the posterior parameters
  post_a <- Y + a
  post_b <- N - Y + b
  
  # Create a sequence of theta values
  theta <- seq(0, 1, length.out = 1000)
  
  # Prior and posterior densities
  prior_density <- dbeta(theta, a, b)
  posterior_density <- dbeta(theta, post_a, post_b)
  
  # Plot the prior and posterior densities
  plot(theta, prior_density, type = "l", col = "blue", lwd = 2, 
       ylab = "Density", xlab = expression(theta), 
       main = "Prior and Posterior Density Functions")
  lines(theta, posterior_density, col = "red", lwd = 2)
  legend("topright", legend = c("Prior", "Posterior"), 
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  # Calculate posterior mean and standard deviation
  post_mean <- post_a / (post_a + post_b)
  post_var <- (post_a * post_b) / ((post_a + post_b)^2 * (post_a + post_b + 1))
  post_sd <- sqrt(post_var)
  
  # Return posterior mean and standard deviation
  return(list(posterior_mean = post_mean, posterior_sd = post_sd))
}

#b
# Solve for a and b using the known mean and standard deviation
find_beta_params <- function(mean, sd) {
  # Function to minimize
  objective <- function(params) {
    a <- params[1]
    b <- params[2]
    est_mean <- a / (a + b)
    est_var <- (a * b) / ((a + b)^2 * (a + b + 1))
    est_sd <- sqrt(est_var)
    
    return((est_mean - mean)^2 + (est_sd - sd)^2)
  }
  
  # Optimize to find a and b
  result <- optim(c(1, 1), objective)
  return(result$par)
}

# Given prior mean = 0.7 and prior standard deviation = 0.2
params <- find_beta_params(0.7, 0.2)
a_informative <- params[1]
b_informative <- params[2]
params

##c

# Conduct Bayesian analysis with uninformative prior (a = 1, b = 1)
result_uninformative <- bayesian_binomial(Y = 20, N = 30, a = 1, b = 1)

# Conduct Bayesian analysis with informative prior (a = 11.08, b = 4.75)
result_informative <- bayesian_binomial(Y = 20, N = 30, a = a_informative, b = b_informative)

# Print results
result_uninformative
result_informative

