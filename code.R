# Load necessary libraries
library(quantmod)

# Define function to simulate HCLTECH prices using Monte Carlo
simulate_hcltech_prices <- function(start_date, end_date, num_simulations) {
  # Get historical prices for HCLTECH
  hcltech <- getSymbols("HCLTECH.NS", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  hcltech_prices <- as.numeric(hcltech[, "HCLTECH.NS.Adjusted"])
  
  # Calculate daily log returns
  hcltech_returns <- diff(log(hcltech_prices))
  
  # Calculate mean and standard deviation of daily returns
  mu <- mean(hcltech_returns)
  sigma <- sd(hcltech_returns)
  
  # Set up matrix to store simulated prices
  sim_prices <- matrix(nrow = length(hcltech_prices) + 1, ncol = num_simulations)
  sim_prices[1,] <- hcltech_prices[length(hcltech_prices)]
  
  # Simulate prices using Monte Carlo
  for (i in 2:(length(hcltech_prices) + 1)) {
    sim_prices[i,] <- sim_prices[i-1,] * exp(rnorm(num_simulations, mean = mu, sd = sigma))
  }
  
  # Return matrix of simulated prices
  return(sim_prices[-1,])
}

# Set parameters for simulation
start_date <- as.Date("2021-03-25")
end_date <- as.Date("2022-03-25")
num_simulations <- 100

# Run simulation
simulated_prices <- simulate_hcltech_prices(start_date, end_date, num_simulations)

# Plot simulated prices
matplot(simulated_prices, type = "l", lty = 1, col = "gray", main = "Simulated HCLTECH Prices")
