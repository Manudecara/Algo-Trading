#Bollinger Bands & RSI - Trading Strategy

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(dplyr)
library(knitr)

#Get the data
getSymbols('GOOG', src = 'yahoo', from = "2018-01-01", to = '2019-04-01')

#Plot and add indicator utilised
chartSeries(GOOG)
addRSI()
addBBands()
GOOG <- na.omit(merge(GOOG, RSI(Cl(GOOG))))
GOOG <- na.omit(merge(GOOG, BBands(Cl(GOOG))))

rsi_hold <- (GOOG$rsi < 70 & GOOG$rsi > 30) 
rsi_buy <- (GOOG$rsi < 30)
rsi_sell <- (GOOG$rsi > 70)

bbands_hold <- (GOOG$Close < GOOG$up & GOOG$Close > GOOG$dn) 
bbands_buy <- (GOOG$Close < GOOG$dn)
bbands_sell <- (GOOG$Close > GOOG$up)

GOOG$sig <- case_when(rsi_buy & bbands_buy ~ 1,
                      rsi_sell & bbands_sell ~ -1,
                      rsi_hold & bbands_hold ~ 0)

GOOG$sig <- na.fill(GOOG$sig, 0)

GOOG$sig <- Lag(GOOG$sig)

table(GOOG$sig)


#create a table with your returns
Returns <- na.omit(GOOG$sig) * dailyReturn(Cl(GOOG))

#plot the benchmark of the stock with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(Cl(GOOG)),Returns))

#create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")

#create a table of drawdowns 
kable(table.Drawdowns(Returns, top=10), caption = "Table of Drawdowns")





