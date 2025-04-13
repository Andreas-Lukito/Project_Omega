# Number of observations
n <- 1000

# Set seed for reproducibility
set.seed(123)

# Generate data
# Modal (normal distribution)
modal <- rnorm(n, mean = 7500, sd = 1500)

# Stock (binomial distribution scaled)
stock <- rbinom(n, size = 200, prob = 0.5) + 20

# Demand rate (normal distribution with outliers)
demand_rate <- rnorm(n, mean = 40, sd = 10)
# Introduce outliers
outliers <- sample(1:n, size = n * 0.05) # 5% of the data
demand_rate[outliers] <- demand_rate[outliers] + runif(length(outliers), 20, 50)

# Competitor price (normal distribution)
competitor_price <- rnorm(n, mean = 47500, sd = 15000)
competitor_price <- pmax(15000, pmin(80000, competitor_price)) # Keep within bounds

# Profit margin (uniform distribution)
profit_margin <- runif(n, min = 0.05, max = 0.3)

# Final price calculation based on the relationships
# Base price influenced by modal and demand
base_price <- modal * (1 + demand_rate / 100)

# Adjust for stock: inverse relationship
stock_adjustment <- 1 + (mean(stock) - stock) / mean(stock)

# Adjust for competitor price: small proportional increase
competitor_adjustment <- competitor_price / mean(competitor_price)

# Combine all influences into final price
final_price <- base_price * stock_adjustment * competitor_adjustment

# Apply profit margin
final_price <- final_price * (1 + profit_margin)

# Ensure final price is below competitor price
final_price <- round(pmin(final_price, competitor_price * 0.99), digits = 0) # 1% below competitor price

# Create a data frame
data <- data.frame(
  Modal = modal,
  Stock = stock,
  DemandRate = demand_rate,
  CompetitorPrice = competitor_price,
  ProfitMargin = profit_margin,
  FinalPrice = final_price
)

# View the first few rows
head(data)

# Summary statistics
summary(data)

# Save data to a CSV file (optional)
write.csv(data, "synthetic_data.csv", row.names = FALSE)
