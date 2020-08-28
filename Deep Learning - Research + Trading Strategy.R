#Deep Learning - Research + Trading Strategy

setwd("~/Quant Finance")
install.packages('deepnet')

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
library(deepnet)

#Get the data
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

#Get indicators
rsi14 <- RSI(Cl(min_gbpchf))
rsi5 <- RSI(Cl(min_gbpchf))
sma10 <- SMA(Cl(min_gbpchf), n = 10)
sma20 <- SMA(Cl(min_gbpchf), n = 20)
ema15 <- EMA(Cl(min_gbpchf), n = 15)
macd7205 <- MACD(Cl(min_gbpchf), 7, 20, 5, 'SMA')
macd12269 <- MACD(Cl(min_gbpchf), 12, 26, 9, 'SMA')

min_gbpchf <- cbind(min_gbpchf$close, sma10, sma20, ema15, 
              rsi14, rsi5, macd7205, macd12269)

min_gbpchf <- na.omit(min_gbpchf)

#create direction of price (up or down) depending on whether current price 
#is greater or lower than the previous 20 days price.

direction <- data.frame(matrix(NA, dim(min_gbpchf)[1], 1))
lag_ret <- (min_gbpchf$close - Lag(min_gbpchf$close, 20)) / Lag(min_gbpchf$close, 20)
summary(lag_ret)
direction[lag_ret > 0.0001] <- 'Up'
direction[lag_ret < -0.0001] <- 'Down'
direction[lag_ret < 0.0001 & lag_ret > -0.0001] <- "Nowhere"
table(direction)
direction <- na.omit(direction)


#create train, validation and testing sets
min_gbpchf <- min_gbpchf[-c(1:20)]
train <- scale(min_gbpchf[1:400000, ])
validation <- scale(min_gbpchf[400001:500000, ])
testing <- scale(min_gbpchf[500001:743870, ])

train_direction <- direction[1:400000, ]
validation_direction <- direction[400001:500000, ]
testing_direction <- direction[500001:743870,  ]

model <- dbn.dnn.train(train, class.ind(train_direction), hidden = c(3,4,6))
model

validation_prediction <- nn.predict(model, validation)
summary(validation_prediction)
nn.test(model, validation, class.ind(validation_direction), t= 0.4)

valid_pred_class <- data.frame(matrix(NA, dim(validation_prediction)[1], 1))
valid_pred_class[validation_prediction[,'Down'] > 0.3754, 1] <- 'Down'
valid_pred_class[validation_prediction[,'Nowhere'] > 0.233, 1] <- 'Nowhere'
valid_pred_class[validation_prediction[,'Up'] > 0.3771, 1] <- 'Up'
table(valid_pred_class)
table(is.na(valid_pred_class))
table(is.na(validation_direction))
valid_pred_class <- na.locf(valid_pred_class)

valid_pred_class <- as.factor(valid_pred_class$matrix.NA..dim.validation_prediction..1...1.)
validation_direction <- as.factor(validation_direction)
valid_matrix <- confusionMatrix(valid_pred_class, validation_direction)
valid_matrix

test_prediction <- nn.predict(model, testing)
head(test_prediction)
summary(test_prediction)

test_pred_class <- data.frame(matrix(NA, dim(test_prediction)[1], 1))
test_pred_class[validation_prediction[,'Down'] > 0.3754, 1] <- 'Down'
test_pred_class[validation_prediction[,'Nowhere'] > 0.233, 1] <- 'Nowhere'
test_pred_class[validation_prediction[,'Up'] > 0.3771, 1] <- 'Up'
table(test_pred_class)
table(is.na(test_pred_class))
table(is.na(test_prediction))
valid_pred_class <- na.locf(test_pred_class)

test_pred_class <- as.factor(test_pred_class$matrix.NA..dim.test_prediction..1...1.)
testing_direction <- as.factor(testing_direction)
test_matrix <- confusionMatrix(test_pred_class, testing_direction)
test_matrix

signal <- ifelse(test_pred_class == 'Up', 1, ifelse(test_pred_class == 'Down', -1,0))
table(signal)
table(is.na(signal))

cost <- 0
trade_ret <- testing$close * Lag(signal) - cost
summary(trade_ret)

#Plot the benchmark of the derivative with the performance of your strategy
charts.PerformanceSummary(trade_ret)

#Create a calendar table of returns
kable(table.CalendarReturns(trade_ret), caption = "Calendar Returns")

#Create a table of drawdowns 
kable(table.Drawdowns(trade_ret, top=10), caption = "Table of Drawdowns") 