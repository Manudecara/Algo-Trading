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

#Download Free Data from 'TrueFX'. Registration needed.
eurchf <- read.csv('~/Downloads/EURCHF-2020-07.csv', header=FALSE)
datetime <- strptime(eurchf[,2], format="%Y%m%d %H:%M:%OS")
#Create xts object
eurchf <- as.xts(eurchf[,3:4], order.by=datetime)
colnames(eurchf) <- c("bid","ask")
head(eurchf)

#Calculate the midrate
midrate <- (eurchf[,1] + eurchf[,2])/2
names(midrate) <- "midpoint"

#Aggregate the midrate to various frequencies
#10 seconds aggregation
midrate10s <- aggregatets(midrate, on="seconds", k=10)
head(midrate10s)
plot(midrate10s)
#Remove weekends
index <- .indexwday(midrate10s)
unique(index)
midrate10s <- midrate10s[index %in% 1:5]
plot(midrate10s, main="", type="p", pch=1, cex=0.05)
#Calculate returns
ret10s <- 100 * diff(log(midrate10s))
summary(ret10s)

#1 minute aggregation
midrate1m <- aggregatets(midrate, on="minutes", k=1)
head(midrate1m)
plot(midrate1m)
#Remove weekends
index2 <- .indexwday(midrate1m)
unique(index2)
midrate1m <- midrate1m[index2 %in% 1:5]
plot(midrate1m, main="", type="p", pch=1, cex=0.05)
#Calculate returns
ret1m <- 100 * diff(log(midrate1m))
summary(ret1m)

#10 minutes aggregation
midrate10m <- aggregatets(midrate, on="minutes", k=10)
head(midrate10m)
plot(midrate10m)
#Remove weekends
index3 <- .indexwday(midrate10m)
unique(index3)
midrate10m <- midrate10m[index3 %in% 1:5]
plot(midrate10m, main="", type="p", pch=1, cex=0.05)
#Clculate returns
ret10m <- 100 * diff(log(midrate10m))
summary(ret10m)

#Calculate realized volatility for the returns of each frequency
rv10s <- rCov(ret10s, align.by="seconds", align.period=10)
rv1m   <- rCov(ret1m, align.by="minutes", align.period=1)
rv10m <- rCov(ret10m, align.by="minutes", align.period=10)
plot(rv10s^0.5,  col=1, lty=1)         # black continuous
lines(rv1m^0.5,  col=2, lty=2, lwd=2)  # red dashed
lines(rv10m^0.5, col=3, lty=3, lwd=2)  # green dots

#The realized volatility measure  RVt  is criticized because 
#it is not robust to microstructure noise and to outliers of jumps, 
#as it is shown in the previous graph. An alternative to the  
#RVt  estimator is the median RV estimator which consists of the taking 
#the median of the intra-day returns
medrv10m  <- medRV(ret10m, align.by="minutes", align.period=10)
plot(medrv10m^0.5, col=1, lty=1)
lines(rv10m^0.5, col=2, lty=2, lwd=2)