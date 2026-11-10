library(tidyverse)
library(tidyquant)
#PHASE 1: portfolio weighting and setup
weights <- tibble(
  symbol = c("VT", "GLD", "BTC-USD", "SRFM", "ONDS", "BRK-B", "BIL", "GOOGL"), 
  weight = c(0.589, 0.124, 0.023, 0.003, 0.032, 0.055, 0.138, 0.036)
)
#creating portfolio dataframe
portfolio <- tq_get(c("VT", "GLD", "BTC-USD", "SRFM", "ONDS", "BRK-B", "BIL", "GOOGL"), 
                    from = "2020-01-01", to = "2026-11-10")

#creating returns data for tickers
returns_data <- portfolio |> group_by(symbol) |> tq_transmute(
  select = adjusted, mutate_fun = periodReturn, 
  period = "daily",
  type = "arithmetic", 
  col_rename = "daily_return") |> 
  drop_na()

#creating portfolio returns
portfolio_returns <- returns_data |> 
  tq_portfolio(
    assets_col = symbol, 
    returns_col = daily_return, 
    weights = weights, 
    col_rename = "portfolio_return")
#Creating VaR code
var <- quantile(portfolio_returns$portfolio_return, probs = 0.05, na.rm = TRUE)

#creating histogram for VaR
portfolio_histogram <- ggplot(portfolio_returns, aes(x=portfolio_return)) +
  geom_histogram(bins=100, mapping = aes(fill = portfolio_return > var)) +
  geom_vline(xintercept = var, color = "black", linewidth = 1.2, linetype = "dashed") +
  labs(title = "Daily Portfolio Returns and 5% VaR",
       x = "Daily Portfolio Return in %/100", y = "Number of Days" )

#creating expected shortfall (es)
expected_shortfall <- portfolio_returns |> 
  filter(var > portfolio_return) |> 
  summarise(expected_shortfall = mean(portfolio_return))

#phase 2, forward looking for next 5 years via monte carlo
#calculating mean and standard deviation for the whole portfolio
portfolio_stats_return <- portfolio_returns |> 
  summarize(mean_return = mean(portfolio_return), portfolio_sd = sd(portfolio_return))
# Defining degrees of freedom
nu <- 5

#converting to log returns
log_mean_return <- log(1 + portfolio_stats_return$mean_return) - ((portfolio_stats_return$portfolio_sd)^2) / 2
log_sd <- portfolio_stats_return$portfolio_sd
#Scaling to maintain target standard deviation
scale_t <- log_sd*sqrt((nu-2)/nu)

#creating random outcome
five_year_return <- exp(cumsum(log_mean_return + scale_t * rt(1260, df = nu))) * 28100
#10,000 run simulation
portfolio_simulation <- replicate(
  10000, 
  exp(cumsum(log_mean_return + scale_t * rt(1260, df = nu))) * 28100
)
#take sample to not crash everything
simulation_sample <- portfolio_simulation[ , 1:100]

#make into a data frame or tibble so graphs can be made
simulation_sample_tibble <- simulation_sample |> as_tibble() |> mutate(day = 1:1260)

#make data longer
plotting_data <- simulation_sample_tibble |> pivot_longer(
  cols = -day, names_to = "simulation_no", values_to = "portfolio_value"
)
#plotting the graph
monte_carlo_graph <- plotting_data |> 
  ggplot(mapping = aes(x=day, y = portfolio_value, group = simulation_no)) +geom_line(alpha = 0.1)

#making graph clearer
final_graph <- monte_carlo_graph + geom_hline(yintercept = 28100, color = "red", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) + theme_minimal() +
  labs(title = "5-Year Monte Carlo Performance Simulation", subtitle = "First 100 Simulations", 
       x= "Trading Days", y = "Portfolio Value (£)")

# working out loss and doubling chance
losing_chance <- mean(portfolio_simulation[1260, ] < 28100)
doubling_chance <- mean(portfolio_simulation[1260,]  > 56200)
# calculating cumulative var
final_portfolio_values <- portfolio_simulation[1260, ]
five_year_var <- quantile(final_portfolio_values, probs = 0.05, na.rm = TRUE)
five_year_cvar <- mean(final_portfolio_values[final_portfolio_values < five_year_var])
