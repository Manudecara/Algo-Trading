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


#Tick Data Download
bid_gbpchf <- read.csv("~/Downloads/GBPCHF_TICKDATA_2020_06/GBPCHF_TICKBID_2020_06.csv",
                       head = TRUE, sep=";")
bid_gbpchf <- bid_gbpchf[,-3]
names(bid_gbpchf) <- c('date', 'bid')
bid_gbpchf$date <- ymd_hms(bid_gbpchf$date)
str(bid_gbpchf)



ask_gbpchf <- read.csv("~/Downloads/GBPCHF_TICKDATA_2020_06/GBPCHF_TICKASK_2020_06.csv",
                       head = TRUE, sep=";")
ask_gbpchf <- ask_gbpchf[,-3]
names(ask_gbpchf) <- c('date', 'ask')
ask_gbpchf$date <- ymd_hms(ask_gbpchf$date)
str(ask_gbpchf)

bidask <- data.frame(bid_gbpchf$bid, ask_gbpchf$ask)
str(bidask)
time_index <- as.POSIXct(bid_gbpchf$date)
tick_gbpchf <- as.xts(bidask, time_index)
names(tick_gbpchf) <- c('bid', 'ask')
str(tick_gbpchf)

min_gbpchf <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2018.csv", 
                         head = TRUE, sep=";")
min_gbpchf <- min_gbpchf[,-6]
names(min_gbpchf) <- c('date', 'open', "high", "low", "close")
min_gbpchf <- min_gbpchf[,-c(2:4)]
min_gbpchf$date <- ymd_hms(min_gbpchf$date)
str(min_gbpchf)


#change timezones
tzone(min_gbpchf) <- 'GMT'
tzone(min_gbpchf)
tzone(tick_gbpchf) <- 'GMT'
tzone(tick_gbpchf)

#clean up functions
#for trade 
salesCondition(min_gbpchf)
mergeTradesSameTimestamp(min_gbpchf)
rmTradeOutliers(min_gbpchf)

#for quote
noZeroQuotes(tick_gbpchf)
mergeQuotesSameTimestamp(tick_gbpchf)
rmOutliersQuotes(tick_gbpchf)
rmNegativeSpread(tick_gbpchf)


#lets work with tick data
# Extract the time index. 
quote_times <- index(tick_gbpchf)
# Compute the consecutive time differences 
time_differences <- as.numeric(diff(quote_times))
summary(time_differences)

plot(tick_gbpchf$bid_price, type = 'l', main = "GBP/CHF bid price")

# Calculate the bid-ask spread. 
tick_gbpchf$bid_ask_spread <- tick_gbpchf$ask_price - tick_gbpchf$bid_price

# Plot the spread. 
plot(bid_ask_spread, type = "l",
main = "Bid ask spread")