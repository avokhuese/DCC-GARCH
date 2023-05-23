# Load the data and Libray files
library(readxl)
library(dplyr)

independent_vars <- read_excel("Downloads/determinants_final-5.xlsx")

dependent_vars <- read_excel("Downloads/corr.xlsx", 
                             sheet = "Sheet1")

# Convert CPI to rate
independent_vars$CPI <- (independent_vars$CPI)/100


# specify columns to interpolate missing values
cols_to_interp <- c("3M bond", "30YR bond", "VIX")

# interpolate missing values in specified columns
independent_vars[cols_to_interp] <- apply(independent_vars[cols_to_interp], 2, function(x) approx(x = x, n = length(x), method = "linear", rule = 2)$y)

# Create new determinant variables
independent_vars$Term_spread <- independent_vars$`10YR bond` - independent_vars$`3M bond`

independent_vars$Mortgage_spread <- independent_vars$`mortgage bond` - independent_vars$`10YR bond`

# Convert date column to date format for both datasets
independent_vars$date <- as.Date(independent_vars$date, format = "%Y-%m-%d")
dependent_vars$date <- as.Date(dependent_vars$date, format = "%Y-%m-%d")


# Extract year and month from date column
independent_vars$Year_Month <- format(independent_vars$date, format = "%Y-%m")
dependent_vars$Year_Month <- format(dependent_vars$date, format = "%Y-%m")

# Group data by date
merged_data <- inner_join(independent_vars[-1], dependent_vars[-1], by = "Year_Month") %>%
  group_by(Year_Month)


# Compute regression analysis
# the selected independent variables as predictors
model <- lm(final_cor ~ `Unemployment rate` + VIX + CPI + Term_spread + Mortgage_spread, data = merged_data)

#model <- lm(final_cor ~  + Mortgage_spread + CPI + `Unemployment rate` + VIX, data=merged_data)
summary(model)

#Plot the corresponding charts
plot(model)
