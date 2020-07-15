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
bid_gbpchf <- read.csv("~/Downloads/GBPCHF_TICKDATA_2020_06/GBPCHF_TICKBID_2020_06.csv")
bid_gbpchf <- separate(data = bid_gbpchf, col = X20200601.000000.1.189940.0, 
                       into = c("date", "time", "bidoffer1", "bidoffer2"))
bid_gbpchf <- unite(bid_gbpchf, date, date, time, sep = '') 
bid_gbpchf <- unite(bid_gbpchf, bidprice, bidoffer1, bidoffer2, sep = '.')
bid_gbpchf$date <- ymd_hms(bid_gbpchf$date)
str(bid_gbpchf)
bid_gbpchf$bidprice <- as.numeric(bid_gbpchf$bidprice)
str(bid_gbpchf)


ask_gbpchf <- read.csv("~/Downloads/GBPCHF_TICKDATA_2020_06/GBPCHF_TICKASK_2020_06.csv")
ask_gbpchf <- separate(data = ask_gbpchf, col = X20200601.000000.1.190120.0, 
                       into = c("date", "time", "askoffer1", "askoffer2"))
ask_gbpchf <- unite(ask_gbpchf, date, date, time, sep = '') 
ask_gbpchf <- unite(ask_gbpchf, askprice, askoffer1, askoffer2, sep = '.')
ask_gbpchf$date <- ymd_hms(ask_gbpchf$date)
str(ask_gbpchf)
ask_gbpchf$askprice <- as.numeric(ask_gbpchf$askprice)
str(ask_gbpchf)

bidask <- data.frame(bid_gbpchf$bidprice, ask_gbpchf$askprice)
str(bidask)
time_index <- as.POSIXct(bid_gbpchf$date)
tick_gbpchf <- as.xts(bidask, time_index)
names(tick_gbpchf) <- c('bid_price', 'ask_price')
str(tick_gbpchf)

#Minute data download
min_gbpchf <- read.csv("~/Downloads/GBPCHF_MINDATA_2020_06/GBPCHF_MINDATA_2020_06.csv")
min_gbpchf <- separate(data = min_gbpchf, 
                       col = X20200601.000000.1.189940.1.189950.1.189620.1.189770.0, 
                       into = c("date", "time", "open1", "open2", "high1", "high2", 
                                "low1", "low2", "close1", "close2"))
min_gbpchf <- unite(min_gbpchf, date, date, time, sep = '') 
min_gbpchf <- unite(min_gbpchf, open, open1, open2, sep = '.')
min_gbpchf <- unite(min_gbpchf, high, high1, high2, sep = '.')
min_gbpchf <- unite(min_gbpchf, low, low1, low2, sep = '.')
min_gbpchf <- unite(min_gbpchf, close, close1, close2, sep = '.')
min_gbpchf$date <- ymd_hms(min_gbpchf$date)
str(min_gbpchf)
min_gbpchf$open <- as.numeric(min_gbpchf$open)
min_gbpchf$high <- as.numeric(min_gbpchf$high)
min_gbpchf$low <- as.numeric(min_gbpchf$low)
min_gbpchf$close <- as.numeric(min_gbpchf$close)

ohlc <- data.frame(min_gbpchf$open, min_gbpchf$high, min_gbpchf$low, min_gbpchf$close)
time_index <- as.POSIXct(min_gbpchf$date)
min_gbpchf <- as.xts(ohlc, time_index)
names(min_gbpchf) <- c("open", "high", "low", "PRICE")
str(min_gbpchf)



#change timezones
tzone(min_gbpchf) <- 'GMT'
tzone(min_gbpchf)
tzone(tick_gbpchf) <- 'GMT'
tzone(tick_gbpchf)


#Now you can save those files that have been cleaned and manipulated, 
#and load them after in a much simpler way.

ds.storage.file.csv.save(min_gbpchf, 'min_gbpchf_clean', 
                         date.format = '%Y-%m-%d %H:%M:%S')
ds.storage.file.csv.save(tick_gbpchf, 'tick_gbpchf_clean', 
                         date.format = '%Y-%m-%d %H:%M:%S')


min_gbpchf <- read.csv("~/Downloads/GBPCHF_CLEANDATA_2020_06/min_gbpchf_clean.csv")
min_gbpchf$Date <- ymd_hms(min_gbpchf$Date)
time_index <- as.POSIXct(min_gbpchf$Date)
min_gbpchf <- as.xts(min_gbpchf, time_index)
min_gbpchf <- min_gbpchf[,-1]

tick_gbpchf <- read.csv("~/Downloads/GBPCHF_CLEANDATA_2020_06/tick_gbpchf_clean.csv")
tick_gbpchf$Date <- ymd_hms(tick_gbpchf$Date)
time_index <- as.POSIXct(tick_gbpchf$Date, tz= 'GMT')
tick_gbpchf <- as.xts(tick_gbpchf, time_index)
tick_gbpchf <- tick_gbpchf[,-1]


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

tick_gbpchf <- tick_gbpchf[1:150000]

plot(tick_gbpchf$bid_price, type = 'l', main = "GBP/CHF bid price")

# Calculate the bid-ask spread. 
tick_gbpchf$bid_ask_spread <- tick_gbpchf$ask_price - tick_gbpchf$bid_price

# Plot the spread. 
plot(bid_ask_spread, type = "l",
main = "Bid ask spread")

