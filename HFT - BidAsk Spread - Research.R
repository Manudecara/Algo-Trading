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
time_index <- as.POSIXct(bid_gbpchf$date)
tick_gbpchf <- as.xts(bidask, time_index)
names(tick_gbpchf) <- c('bid', 'ask')
str(tick_gbpchf)

tick_gbpchf$spread <- tick_gbpchf$bid - tick_gbpchf$ask

plot(tick_gbpchf$spread, type = 'l')
