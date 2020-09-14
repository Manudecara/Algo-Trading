#Neural Network - Research

setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(nnet)
library(caret)
library(lubridate)

#Get the data
getSymbols('GOOG', src = 'yahoo', from = "2010-01-01", to = '2020-01-01')
chartSeries(GOOG)

GOOG <- read.csv("~/Desktop/HISTDATA/GBPCHF_MIN_2019.csv", 
                         head = TRUE, sep=";")
GOOG <- GOOG[,-6]
names(GOOG) <- c('date', 'open', "high", "low", "close")
GOOG <- GOOG[,-c(2:4)]
GOOG$date <- ymd_hms(GOOG$date)
ohlc <- data.frame(GOOG$close)
time_index <- as.POSIXct(GOOG$date)
GOOG <- as.xts(ohlc, time_index)
names(GOOG) <- c("close")
str(GOOG)

#Get indicators
rsi14 <- RSI(Cl(GOOG))
rsi5 <- RSI(Cl(GOOG))
sma10 <- SMA(Cl(GOOG), n = 10)
sma20 <- SMA(Cl(GOOG), n = 20)
ema15 <- EMA(Cl(GOOG), n = 15)
macd7205 <- MACD(Cl(GOOG), 7, 20, 5, 'SMA')
macd12269 <- MACD(Cl(GOOG), 12, 26, 9, 'SMA')

GOOG <- cbind(Cl(GOOG), sma10, sma20, ema15, 
              rsi14, rsi5, macd7205, macd12269)

GOOG <- na.omit(GOOG)

#create direction of price (up or down) depending on whether current price 
#is greater or lower than the previous 20 days price.

direction <- data.frame(matrix(NA, dim(GOOG)[1], 1))
lag_ret <- (GOOG$close - Lag(GOOG$close, 20)) / Lag(GOOG$close, 20)
summary(lag_ret)
direction[lag_ret > 0.0001] <- 'Up'
direction[lag_ret < -0.0001] <- 'Down'
direction[lag_ret < 0.0001 & lag_ret > -0.0001] <- "Nowhere"
table(direction)
direction <- na.omit(direction)


#create train, validation and testing sets
GOOG <- GOOG[-c(1:20)]
train <- scale(GOOG[1:250000, ])
validation <- scale(GOOG[250001:300000, ])
testing <- scale(GOOG[300001:371644, ])

train_direction <- direction[1:250000, ]
validation_direction <- direction[250001:300000, ]
testing_direction <- direction[300001:371644,  ]

model <- nnet(train, class.ind(train_direction), size = 4)
model

table(is.na(GOOG))
table(is.na(train))
table(is.na(validation))
table(is.na(testing))
table(is.na(train_direction))
table(is.na(validation_direction))
table(is.na(testing_direction))


validation_prediction <- predict(model, validation)
head(validation_prediction)
summary(validation_prediction)

valid_pred_class <- data.frame(matrix(NA, dim(validation_prediction)[1], 1))
valid_pred_class[validation_prediction[,'Down'] > 0.3, 1] <- 'Down'
valid_pred_class[validation_prediction[,'Nowhere'] > 0.3, 1] <- 'Nowhere'
valid_pred_class[validation_prediction[,'Up'] > 0.4, 1] <- 'Up'
table(valid_pred_class)
table(is.na(valid_pred_class))
table(is.na(validation_direction))
valid_pred_class <- na.locf(valid_pred_class)

valid_pred_class <- as.factor(valid_pred_class$matrix.NA..dim.validation_prediction..1...1.)
validation_direction <- as.factor(validation_direction)
valid_matrix <- confusionMatrix(valid_pred_class, validation_direction)
valid_matrix

test_prediction <- predict(model, testing)
head(test_prediction)
summary(test_prediction)

test_pred_class <- data.frame(matrix(NA, dim(test_prediction)[1], 1))
test_pred_class[test_prediction[,'Down'] > 0.3, 1] <- 'Down'
test_pred_class[test_prediction[,'Nowhere'] > 0.3, 1] <- 'Nowhere'
test_pred_class[test_prediction[,'Up'] > 0.4, 1] <- 'Up'
table(test_pred_class)
table(is.na(test_pred_class))
table(is.na(test_prediction))
valid_pred_class <- na.locf(test_pred_class)

test_pred_class <- as.factor(test_pred_class$matrix.NA..dim.test_prediction..1...1.)
testing_direction <- as.factor(testing_direction)
test_matrix <- confusionMatrix(test_pred_class, testing_direction)
test_matrix

signal <- ifelse(test_pred_class == 'Up', 1, ifelse(test_pred_class == 'Down', -1,0))
signal <- data.frame(signal)
table(signal)

cost <- 0
trade_ret <- testing$close * Lag(signal) - cost

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(trade_ret)

#Create a calendar table of returns
kable(table.CalendarReturns(trade_ret), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(trade_ret, top=10), caption = "Table of Drawdowns")   


