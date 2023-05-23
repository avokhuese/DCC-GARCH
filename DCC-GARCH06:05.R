# Load data from Excel sheet
library(tidyverse)
library(readxl)
rent_nominal_prices <- read_excel("Downloads/Rent_prices_OECD-3.xlsx", 
                          sheet = "Werkblad 1 - HOUSE_PRICES_28032")


stock_prices <- read_excel("Downloads/1_reit_dax_price.xlsx", 
                        sheet = "Blad1")

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

library(xts)
install.packages("datefixR")
# Convert daily asset returns to quarterly aggregated returns

returns <- data.frame(Date=stock_prices$Date[-2], REIT=reit_prices, DAX=dax_returns)

quarterly_returns = to.quarterly(returns, indexAt = "Lastof", OHLC=FALSE)


# Create data frame with quarterly returns for both assets
rent_returns <- data.frame(Date=rent_nominal_prices$Date[-1], RENT=rent_returns, NOMINAL=nominal_returns)

view(rent_returns)
# Create the summary of the estimated quarterly returns
summary(quarterly_returns)
summary(rent_returns)

# To Perform the DCC-GARCH Analysis
# Install RMGARCH Library

install.packages("rmgarch")
library(rmgarch)

# Convert the monthly returns to time series data
rent_ts <- ts(rent_returns$RENT.RENT, start = 1, frequency = 252)
nominal_ts <- ts(rent_returns$NOMINAL, start = 1, frequency = 252)

dax_ts <- ts(quarterly_returns$DAX, start = 1, frequency = 252)
reit_ts <- ts(quarterly_returns$REIT, start=1, frequency = 252)

rent_returns_ts <- data.frame(Date=rent_returns$Date[1], RENT=rent_ts, NOMINAL=nominal_ts)

dax_returns_ts <- data.frame(Date=quarterly_returns$DAX[-1], REIT=rent_ts, DAX=dax_ts[-1])


# Create DCC-GARCH model
spec <- dccspec(uspec=multispec(replicate(2, ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0), include.mean=TRUE), distribution.model = "norm"))), dccOrder = c(1,1), distribution = "mvnorm")


# Fit model for the rent returns
fit <- dccfit(spec, data=cbind(rent_ts, nominal_ts), fit.control = list(eval.se=TRUE))

#Print the summary
summary(fit)


# Plot correlation and volatility
plot(fit) # Enter 1 to 5 to view the different plots


# Fit model
fit2 <- dccfit(spec, data=cbind(dax_ts, reit_ts), fit.control = list(eval.se=TRUE))

#Print the summary
summary(fit2)


# Plot correlation and volatility
plot(fit2) # Enter 1 to 5 to view the different plots



# Create correlation matrix
cor_matrix <- cor(rent_returns[, c("RENT.RENT", "RENT.NOMINAL")])
print(cor_matrix)

# Calculate returns
price_index <- dax_returns_ts$DAX
real_estate_returns <- rent_returns$RENT.RENT + (rent_returns$RENT.NOMINAL * price_index[-1])/price_index[-length(price_index)]

#Print the summary of the total returns
summary(real_estate_returns)

# Plot the real estate returns
plot(real_estate_returns, type="l", main="Total Real Estate Returns", ylab="Real Estate Returns", xlab="")
