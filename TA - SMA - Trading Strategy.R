#SMA - Trading Strategy

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)

#Get the data
getSymbols('GOOG', src = 'yahoo', from = "2018-01-01", to = '2019-04-01')

#Plot and add indicator utilised
chartSeries(GOOG)
addSMA(n= 20, col= 'red')
addSMA(n= 50, col= 'green')
GOOG$sma20 <- SMA(Cl(GOOG), 20)
GOOG$sma50 <- SMA(Cl(GOOG), 50)

#Create strategy
#Long when fast sma crosses slow sma upwards
#Short when fast sma crosses slow sma downwards
GOOG$signal <- ifelse((GOOG$sma20) > GOOG$sma50 , 1 , -1)

#Flat on first day and last day
GOOG$sig[1] <- 0 
GOOG$sig[nrow(GOOG)] <- 0

#Fill in the signal for other times
#Wherever signal is NA, copy previous value to next row
GOOG$signal <- na.locf(GOOG$signal)

#Lag signal so that you don't trade on the same bar that your signal fires
GOOG$signal <- Lag(GOOG$signal)
#Replace NA with zero position on first row
GOOG$signal[1] <- 0 

#Plot your positions
plot(GOOG$GOOG.Close)
addEventLines(events = GOOG$signal, lwd = 1, col = "red")

#Create a table with your returns
Returns <- na.omit(GOOG$signal) * dailyReturn(Cl(GOOG))

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(Cl(GOOG)),Returns))

#Create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(Returns, top=10), caption = "Table of Drawdowns")


