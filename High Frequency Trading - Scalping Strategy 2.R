#High Frequency Trading - Scalping Strategy 2

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
library(rtsdata)
library(lubridate)
library(tidyr)

min_gbpchf18 <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2018.csv", 
                         head = TRUE, sep=";")
min_gbpchf18 <- min_gbpchf18[,-6]
names(min_gbpchf18) <- c('date', 'open', "high", "low", "close")
min_gbpchf18$date <- ymd_hms(min_gbpchf18$date)
str(min_gbpchf18)

min_gbpchf19 <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2019.csv", 
                         head = TRUE, sep=";")
min_gbpchf19 <- min_gbpchf19[,-6]
names(min_gbpchf19) <- c('date', 'open', "high", "low", "close")
min_gbpchf19$date <- ymd_hms(min_gbpchf19$date)
str(min_gbpchf19)

min_gbpchf <- rbind(min_gbpchf18, min_gbpchf19)

ohlc <- data.frame(min_gbpchf$open, min_gbpchf$high, min_gbpchf$low, min_gbpchf$close)
time_index <- as.POSIXct(min_gbpchf$date)
min_gbpchf <- as.xts(ohlc, time_index)
names(min_gbpchf) <- c("open", 'high', 'low', 'close')
summary(min_gbpchf)
str(min_gbpchf)

min_gbpchf <- na.omit(merge(min_gbpchf, BBands(min_gbpchf$close)))
min_gbpchf <- na.omit(merge(min_gbpchf, stoch(HLC(min_gbpchf), nFastK = 5, nFastD = 3, nSlowD = 3)))
min_gbpchf <- na.omit(merge(min_gbpchf, EMA(min_gbpchf$close, n= 50)))
min_gbpchf <- na.omit(merge(min_gbpchf, EMA(min_gbpchf$close, n= 100)))
names(min_gbpchf) <- c("Open", "High", "Low", "Close", 'dn' , 'mavg', 'up', 'pctB', 'fastK', "fastD", "slowD", 'ema50', "ema100")


#Create strategy

#Add Bollinger Band and RSI signals
min_gbpchf$sig <- NA
#Short when Close is above Upper Bollinger Band and RSI threshold
min_gbpchf$sig[min_gbpchf$ema50 > min_gbpchf$ema100 & 
                 (diff(sign(min_gbpchf$slowD - 0.80)) != 0) &
                 (diff(sign(min_gbpchf$slowD - 0.80)) != 2)] <- -1

#Long when Close is under Lower Bollinger Band and RSI threshold
min_gbpchf$sig[min_gbpchf$ema50 < min_gbpchf$ema100 & 
                 (diff(sign(min_gbpchf$slowD - 0.20)) != 0) &
                 (diff(sign(min_gbpchf$slowD - 0.20)) != -2)] <- 1

min_gbpchf$sig[(diff(sign(min_gbpchf$Close - min_gbpchf$mavg)) != 0)] <- 0

#Flat on first day and last day
min_gbpchf$sig[1] <- 0 
min_gbpchf$sig[nrow(min_gbpchf)] <- 0

#Fill in the signal for other times
#Wherever signal is NA, copy previous value to next row
min_gbpchf$sig <- na.locf(min_gbpchf$sig) 
table(min_gbpchf$sig)

#Lag signal so that you don't trade on the same bar that your signal fires
min_gbpchf$sig <- Lag(min_gbpchf$sig)
#Replace NA with zero position on first row
min_gbpchf$sig[1] <- 0 

#Plot your positions
plot(min_gbpchf$min_gbpchf.Close)
addEventLines(events = min_gbpchf$sig, lwd = 1, col = "red")

#Create a table with your returns
Returns <- na.omit(min_gbpchf$sig) * dailyReturn(Cl(min_gbpchf))

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(Cl(min_gbpchf)),Returns))

#Create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(Returns, top=10), caption = "Table of Drawdowns")