#High Frequency Scalping Strategy

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

min_gbpchf18 <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2018.csv", 
                         head = TRUE, sep=";")
min_gbpchf18 <- min_gbpchf18[,-6]
names(min_gbpchf18) <- c('date', 'open', "high", "low", "close")
min_gbpchf18 <- min_gbpchf18[,-c(2:4)]
min_gbpchf18$date <- ymd_hms(min_gbpchf18$date)
str(min_gbpchf18)

min_gbpchf19 <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2019.csv", 
                       head = TRUE, sep=";")
min_gbpchf19 <- min_gbpchf19[,-6]
names(min_gbpchf19) <- c('date', 'open', "high", "low", "close")
min_gbpchf19 <- min_gbpchf19[,-c(2:4)]
min_gbpchf19$date <- ymd_hms(min_gbpchf19$date)
str(min_gbpchf19)

min_gbpchf <- rbind(min_gbpchf18, min_gbpchf19)

ohlc <- data.frame(min_gbpchf$close)
time_index <- as.POSIXct(min_gbpchf$date)
min_gbpchf <- as.xts(ohlc, time_index)
names(min_gbpchf) <- c("close")
str(min_gbpchf)
plot(min_gbpchf)

min_gbpchf <- na.omit(merge(min_gbpchf, BBands(min_gbpchf$close)))
min_gbpchf <- na.omit(merge(min_gbpchf, EMA(min_gbpchf$close, n = 10)))
min_gbpchf <- na.omit(merge(min_gbpchf, EMA(min_gbpchf$close, n = 50)))
min_gbpchf <- na.omit(merge(min_gbpchf, RSI(min_gbpchf$close, n = 14)))

#Create strategy

#Create strategy
min_gbpchf$sig <- NA

#Enter Short
min_gbpchf$sig[(min_gbpchf$EMA > min_gbpchf$EMA.1) &
                 (min_gbpchf$rsi > 70) &
                 (diff(sign(min_gbpchf$close - min_gbpchf$up)) != 0) &
                 (diff(sign(min_gbpchf$close - min_gbpchf$up)) != -2)] <- -1

#Enter Long
min_gbpchf$sig[(min_gbpchf$EMA < min_gbpchf$EMA.1) &
                 (min_gbpchf$rsi < 30) &
                 (diff(sign(min_gbpchf$close - min_gbpchf$dn)) != 0) &
                 (diff(sign(min_gbpchf$close - min_gbpchf$dn)) != 2)] <- 1 

#Exit Long and Short
min_gbpchf$sig[(diff(sign(min_gbpchf$close - min_gbpchf$mavg)) != 0)] <- 0

#Flat on first day and last day
min_gbpchf$sig[1] <- 0 
min_gbpchf$sig[nrow(min_gbpchf)] <- 0

#Fill in the signal for other times
#Wherever signal is NA, copy previous value to next row
min_gbpchf$sig <- na.locf(min_gbpchf$sig) 

#Lag signal so that you don't trade on the same bar that your signal fires
min_gbpchf$sig <- Lag(min_gbpchf$sig)
#Replace NA with zero position on first row
min_gbpchf$sig[1] <- 0 

table(min_gbpchf$sig)

#Plot your positions
plot(min_gbpchf$close)
addEventLines(events = min_gbpchf$sig, lwd = 1, col = "red")

#Create a table with your returns
Returns <- na.omit(min_gbpchf$sig) * dailyReturn(Cl(min_gbpchf))

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(Cl(min_gbpchf)),Returns))

#Create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(Returns, top=10), caption = "Table of Drawdowns")
