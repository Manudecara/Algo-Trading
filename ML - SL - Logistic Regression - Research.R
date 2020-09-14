#Logistic Regression - Research

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
getSymbols('GOOG', src = 'yahoo', from = "2016-01-01", to = '2020-01-01')
GOOG <- GOOG[,'GOOG.Close']

#Get indicators
bbands <- BBands(Cl(GOOG))
rsi14 <- RSI(Cl(GOOG))
rsi5 <- RSI(Cl(GOOG))
mean10 <- SMA(GOOG, n = 10)
mean20 <- SMA(GOOG, n = 20)
macd7205 <- MACD(GOOG, 7, 20, 5, 'SMA')
macd12269 <- MACD(GOOG, 12, 26, 9, 'SMA')

#create direction of price (up or down) depending on whether current price 
#is greater or lower than the previous 20 days price.

direction <- NA
direction[GOOG > Lag(GOOG, 20)] <- 1
direction[GOOG < Lag(GOOG, 20)] <- 0

#bind everything into one dataset

GOOG <- cbind(GOOG$GOOG.Close, mean10, mean20, bbands, 
              rsi14, rsi5, macd7205, macd12269, direction)

GOOG <- na.omit(GOOG)

#normalize data
GOOG <- scale(GOOG, 1:13)

#split into training and testing sets
train <- GOOG[1:700]
test <- GOOG[701:973]

logistic <- glm(train$direction ~ train$SMA + train$rsi + train$macd,
                family = binomial)

summary(logistic)

logistic.probs <- predict(logistic,
                     newdata = test,
                     type = "response")

summary(logistic.probs)

#logistic.pred is going to be a vector of trues and falses. 
#So if logistic.probs is bigger than 0.5, logistic.pred calls "Up" (1)
#Otherwise, it calls "Down"(0).
logistic.pred <- ifelse(logistic.probs > 0.5, 1, 0)
table(logistic.pred)
table(logistic.pred, train$direction)

#86% accuracy