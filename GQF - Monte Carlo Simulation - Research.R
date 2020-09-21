#Monte Carlo Simulation

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(tidyverse)

# Get the data
getSymbols('GOOG', src = 'yahoo', from = "2016-01-01", to = '2020-01-01')
getSymbols('AAPL', src = 'yahoo', from = "2016-01-01", to = '2020-01-01')
getSymbols('MSFT', src = 'yahoo', from = "2016-01-01", to = '2020-01-01')

data <- data.frame(AAPL$AAPL.Close, GOOG$GOOG.Close, MSFT$MSFT.Close)
prices <- as.matrix(data[, 1:3])

mc_rep = 1000 # Number of Monte Carlo Simulations
training_days = 30


# This function returns the first differences of a t x q matrix of data
returns = function(Y){
  len = nrow(Y)
  yDif = Y[2:len, ] / Y[1:len-1, ] - 1
}

# Get the Stock Returns
stock_Returns = returns(prices)

# Suppose we invest our money evenly among all three assets 
# We use today's Price to find the number of shares of each stock 
# that we buy
portfolio_Weights = t(as.matrix(rep(1/ncol(stock_Returns), ncol(stock_Returns))))
print(portfolio_Weights)

# Get the Variance Covariance Matrix of Stock Returns
coVarMat = cov(stock_Returns)
miu = colMeans(stock_Returns)
# Extend the vector to a matrix
Miu = matrix(rep(miu, training_days), nrow = 3)


# Use Monte-Carlo to simulate the 30-day Portfolio Returns
# Initializing simulated 30 day portfolio returns
portfolio_Returns_30_m = matrix(0, training_days, mc_rep)

set.seed(200)
for (i in 1:mc_rep) {
  Z = matrix ( rnorm( dim(stock_Returns)[2] * training_days ), ncol = training_days )
  # Lower Triangular Matrix from our Choleski Factorization
  L = t( chol(coVarMat) )
  # Calculate stock returns for each day
  daily_Returns = Miu + L %*% Z  
  # Calculate portfolio returns for 30 days
  portfolio_Returns_30 = cumprod( portfolio_Weights %*% daily_Returns + 1 )
  # Add it to the monte-carlo matrix
  portfolio_Returns_30_m[,i] = portfolio_Returns_30;
}

# Visualising result
x_axis = rep(1:training_days, mc_rep)
y_axis = as.vector(portfolio_Returns_30_m-1)
plot_data = data.frame(x_axis, y_axis)
ggplot(data = plot_data, aes(x = x_axis, y = y_axis)) + geom_path(col = 'red', size = 0.1) +
  xlab('Days') + ylab('Portfolio Returns') + 
  ggtitle('Simulated Portfolio Returns in 30 days')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# Get stats of result
# Porfolio Returns statistics on the 30th day.
Avg_Portfolio_Returns = mean(portfolio_Returns_30_m[30,]-1)
SD_Portfolio_Returns = sd(portfolio_Returns_30_m[30,]-1)
Median_Portfolio_Returns = median(portfolio_Returns_30_m[30,]-1)
print(c(Avg_Portfolio_Returns,SD_Portfolio_Returns,Median_Portfolio_Returns))


# Construct a 95% Confidential Interval for average returns
Avg_CI = quantile(portfolio_Returns_30_m[30,]-1, c(0.025, 0.975))
print(Avg_CI)
