setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(lubridate)
library(class) 
library(e1071)
library(C50)

start_date = as.Date("2015-01-01")
end_date = as.Date("2020-01-01")
getSymbols("GOOG", src = "yahoo", from = start_date, to = end_date)

GOOG <- na.omit(merge(GOOG, RSI(Cl(GOOG))))
GOOG <- na.omit(merge(GOOG, SMA(Cl(GOOG))))
GOOG <- na.omit(merge(GOOG, SAR(GOOG)))
GOOG <- na.omit(merge(GOOG, WPR(HLC(GOOG))))
GOOG <- na.omit(merge(GOOG, CCI(HLC(GOOG))))
GOOG <- na.omit(merge(GOOG, ADX(HLC(GOOG))))
GOOG$return <- dailyReturn(Cl(GOOG))
GOOG <- GOOG[,-6]
GOOG <- GOOG[,-c(12:14)]

GOOG <- as.data.frame(GOOG)
setDT(GOOG, keep.rownames = TRUE)
names(GOOG) <- c('date', 'open', 'high', 'low', 'close','volume', 'rsi', 
                 'sma', 'sar','wpr', 'cci', 'adx', 'return')
GOOG$date <- as.Date(as.character(GOOG$date, format = '%y/%m/%d'))
GOOG <- as.data.frame(GOOG)

GOOG$class <- ifelse(GOOG$return >= 0, 'UP', 'DOWN')
GOOG <- GOOG[ ,-c(1:6)]
GOOG <- GOOG[ ,-7]
str(GOOG)
GOOG$class <- as.factor(GOOG$class)
str(GOOG)

train_GOOG <- GOOG[1:800, ]
test_GOOG <- GOOG[801:1176, ]

prop.table(table(train_GOOG$class))
prop.table(table(test_GOOG$class))

dtree_model <- C5.0(x= train_GOOG[-7], y= train_GOOG$class)
dtree_model
summary(dtree_model)           

dtree_pred <- predict(dtree_model, test_GOOG)
CrossTable(test_GOOG$class, dtree_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))


dtree_model10 <- C5.0(x= train_GOOG[-7], y= train_GOOG$class, trials = 10)
dtree_model10
summary(dtree_model10)
dtree_pred10 <- predict(dtree_model10, test_GOOG)

CrossTable(test_GOOG$class, dtree_pred10, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))


error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost
dtree_cost_model <- C5.0(x= train_GOOG[-7], y= train_GOOG$class, costs = error_cost)
dtree_cost_model
summary(dtree_model10)
dtree_cost_pred <- predict(dtree_cost_model, test_GOOG)

CrossTable(test_GOOG$class, dtree_cost_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))
