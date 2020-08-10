#High Frequency Trading - Tutorial

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(highfrequency)
library(lubridate)

#Step 1: Exploring data
#quote data
data(sample_qdataraw)
data(sample_qdataraw_microseconds)
data(sample_qdata)
data(sample_qdata_microseconds)

#trade data
data(sample_tdataraw)
data(sample_tdataraw_microseconds)
data(sample_tdata)
data(sample_tdata_microseconds)


#Step 2: Cleaning your data
#for minute data
tdata_clean <- tradesCleanup(tdataraw=sample_tdataraw,exchanges="N")
tdata_clean$report

qdata_clean <- quotesCleanup(qdataraw=sample_qdataraw,exchanges="N")
qdata_clean$report


#Step 3: Agreggate your data
#Many types of aggregation 

#1. Claendar time based sampling: Prices are observed as regularly spaced time intervals by 
#taking the last price of the specified interval. 
#For example: previous tick aggregation to the 5-minute sampling frequency:
tdata_clean_5minagg <- aggregatets(tdata_clean$tdata, on="minutes", k=5)
head(tdata_clean_5minagg)

#2. Refresh time based sampling: next observation is when there has been
#a new observation for all series
#For example: We assume that stock1 and stock2 contain price data on imaginary stocks:
stock1 <- sample_tdata$PRICE
stock2 <- sample_tdataraw$PRICE
#Previous-tick aggregation to one minute:
agg_p1min = cbind(aggregatePrice(stock1, on= 'minutes', k=1),
                  aggregatePrice(stock2, on= 'minutes', k=1))
#Refresh time aggregation:
synch_p1min = refreshTime(list(stock1,stock2))


#Step 4: Liquidity
data("sample_tdata")
data("sample_qdata")
#Match the trade and quote data
tqdata = matchTradesQuotes(sample_tdata,sample_qdata)

#Get 23 liquidity measures
liquidity_tqdata <- getLiquidityMeasures(tqdata)

#Step 5: Measure Volatility

#Examples:
#1. Calculate a jump robust volatility measure 
#based on synchronized data:
rbpcov1 = rBPCov(synch_p1min, makeReturns=TRUE)

#2. Calculate a jump and microstructure noise robust volatility measure
#based on nonsynchronous data:
rtscov2 = rTSCov(list(stock1,stock2))

#plot volatility
est_vol = spotvol(sample_real5minprices,P1=6,P2=4,
                  periodicvol="TML",k=5, dummies=FALSE)
plot(est_vol)


#Step 6: Forcast Volatility 

rv_sp500 <- as.xts(realized_library$rv5, order.by = realized_library$date)

x <- harModel(data = rv_sp500)
class(x)
summary(x)
plot(x)
predict(x)






