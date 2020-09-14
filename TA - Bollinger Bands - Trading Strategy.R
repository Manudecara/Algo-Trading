#Bollinger Bands - Trading Strategy

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
addBBands()
GOOG <- na.omit(merge(GOOG, BBands(Cl(GOOG))))

#Create strategy
#Flat when Close crosses the Moving Average
GOOG$sig[c(FALSE, diff(sign(Cl(GOOG) - GOOG$mavg), na.pad=FALSE) != 0)] <- 0
#Short when Close is above Upper Bollinger Band
GOOG$sig[Cl(GOOG) > GOOG$up] <- -1 
#Long when Close is under Lower Bollinger Band
GOOG$sig[Cl(GOOG) < GOOG$dn] <- 1 

#Flat on first day and last day
GOOG$sig[1] <- 0 
GOOG$sig[nrow(GOOG)] <- 0

#Fill in the signal for other times
#Wherever signal is NA, copy previous value to next row
GOOG$sig <- na.locf(GOOG$sig) 

#Lag signal so that you don't trade on the same bar that your signal fires
GOOG$sig <- Lag(GOOG$sig)
#Replace NA with zero position on first row
GOOG$sig[1] <- 0 

#Plot your positions
plot(GOOG$GOOG.Close)
addEventLines(events = GOOG$sig, lwd = 1, col = "red")

#Create a table with your returns
Returns <- na.omit(GOOG$sig) * dailyReturn(Cl(GOOG))

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(Cl(GOOG)),Returns))

#Create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(Returns, top=10), caption = "Table of Drawdowns")


