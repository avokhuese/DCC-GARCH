# Load data from Excdel sheet

library(readxl)
prices <- read_excel("Downloads/1_reit_dax_price.xlsx", 
                     sheet = "Blad1")
View(prices)

# Extract daily prices for REIT and DAX
reit_prices <- prices$REIT
dax_prices <- prices$DAX

## Plot daily prices for both assets: REIT and DAX
plot(reit_prices)
plot(dax_prices)

# Convert daily prices to daily returns

reit_returns <- diff(log(reit_prices))
dax_returns <- diff(log(dax_prices))

## Plot daily returns for both assets
plot(reit_returns)
plot(dax_returns)

# Create data frame with daily returns for both assets

returns <- data.frame(Date=prices$Date[-1], REIT=reit_returns, DAX=dax_returns)


# Convert daily asset returns to monthly aggregated returns
returns$YearMonth <- format(as.Date(returns$Date), "%Y-%m")

monthly_returns <- aggregate(cbind(REIT, DAX) ~ YearMonth, data=returns, sum)

print(monthly_returns)
View(monthly_returns)

# Plot monthly_returns
plot(monthly_returns)

# To Perform the DCC-GARCH Analysis
# Install RMGARCH Library

install.packages("rmgarch")
library(rmgarch)

# Convert the monthly returns to time series data
reit_ts <- ts(monthly_returns$REIT, start = 1, frequency = 252)
dax_ts <- ts(monthly_returns$DAX, start = 1, frequency = 252)


# Create DCC-GARCH model
spec <- dccspec(uspec=multispec(replicate(2, ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0), include.mean=TRUE), distribution.model = "norm"))), dccOrder = c(1,1), distribution = "mvnorm")


# Fit model
fit <- dccfit(spec, data=cbind(reit_ts, dax_ts), fit.control = list(eval.se=TRUE))

#Print the summary
summary(fit)

# Create correlation matrix
cor_matrix <- cor(monthly_returns[, c("REIT", "DAX")])
print(cor_matrix)

# Plot correlation and volatility
plot(fit) # Enter 1 to 5 to view the different plots


