# Load required libraries
library(tidyverse)
library(readxl)
library(xts)
library(rmgarch)

# Load the data
rent_nominal_prices <- read_excel("Downloads/Rent_prices_OECD-3.xlsx", sheet = "Werkblad 1 - HOUSE_PRICES_28032")
stock_prices <- read_excel("Downloads/1_reit_dax_price.xlsx", sheet = "Blad1")

# Extract quarterly prices for RENT and NOMINAL and daily price for DAX
dax_prices <- stock_prices$DAX
reit_prices <- stock_prices$REIT

rent_prices <- rent_nominal_prices$Rent_prices

nominal_prices <- rent_nominal_prices$Nominal_prices

## Plot daily prices for both assets: RENT, Nominal and DAX
plot(dax_prices, type="l", ylab="Stock Prices")
plot(rent_prices, type="l", ylab="Rent Prices")
plot(nominal_prices, type="l", ylab="Nominal House Price")


# Convert daily stock prices to daily returns
dax_returns <- diff(log(dax_prices))
reit_prices <- diff(log(reit_prices))

# Convert quarterly prices to quarterly returns
rent_returns <- diff(log(rent_prices))
nominal_returns <- diff(log(nominal_prices))

## Plot daily and quarterly returns for all assets

plot(dax_returns, type="l", main="Daily Stock Price Returns", ylab="Stock Price Returns", xlab="")
plot(rent_returns, type="l", main="Total Rent Price Returns", ylab="Rent Returns", xlab="")
plot(nominal_returns, type="l", main="Total Nominal House Price Returns", ylab="Nominal Returns", xlab="")


# Convert daily asset returns to quarterly aggregated returns

returns <- data.frame(Date=stock_prices$Date[-2], DAX=dax_returns)

quarterly_returns = to.quarterly(returns, indexAt = "Lastof", OHLC=FALSE)

dax_quarterly_returns <- quarterly_returns[-132,]

# Calculate returns

price_index <- dax_quarterly_returns
real_estate_returns <- rent_returns + (rent_returns * price_index[-1])/price_index[-length(price_index)]

# Plot the real estate returns
plot(real_estate_returns, type="l", main="Total Real Estate Returns", ylab="Real Estate Returns", xlab="")

#Print the summary of the total returns
summary(real_estate_returns)
#real_estate_returns <- returns$RENT + (returns$RENT*returns$DAX)/returns$DAX[-length(returns$DAX)]

# Combine the quarterly returns into a single data frame
#returns <- data.frame(Date = index(rent_returns), DAX = dax_quarterly_returns[-1], RENT = rent_returns, NOMINAL = nominal_returns)
df_returns <- data.frame(Date = index(rent_returns), DAX = dax_quarterly_returns, RERET = real_estate_returns)

view(df_returns)
# Create the DCC-GARCH model specification
spec <- dccspec(uspec = multispec(replicate(2, ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE), distribution.model = "norm"))), dccOrder = c(1,1), distribution = "mvnorm")

# Fit the DCC-GARCH model
fit <- dccfit(spec, data = df_returns[, c("DAX", "RERET")], fit.control = list(eval.se = TRUE))

summary(fit)

# Compute the DCC
dcc <- dccforecast(fit, n.ahead = 1, sigma = volatility)

# Get the DCC value
print(dcc)

# Plot correlation and volatility
plot(fit)


# Create correlation matrix
cor_matrix <- cor(df_returns[, c("DAX", "RERET")])
print(cor_matrix)

