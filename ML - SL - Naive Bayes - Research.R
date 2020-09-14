#Naive Bayes - Research

setwd("~/Quant Finance")
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(dplyr)
library(psych)
library(xts)
library(zoo)
library(knitr)
library(gmodels)
library(lubridate)
library(class)
library(e1071)

start_date = as.Date("2016-01-01")
end_date = as.Date("2019-01-01")
getSymbols("GOOG", src = "yahoo", from = start_date, to = end_date)

day_of_week <- wday(GOOG, label = TRUE)
price_change <- Cl(GOOG) - Op(GOOG)
label <- ifelse(price_change > 0,"UP","DOWN")

df <- data.frame(day_of_week, label)
names(df) <- c('day_of_week', 'label')

NB_pred <- naiveBayes(df$day_of_week, df$label)

NB_pred

sma20 <- SMA(GOOG$GOOG.Close, n=20)
sma50 <- SMA(GOOG$GOOG.Close, n=50)
sma_cross <- sma50-sma20

df2 <- data.frame(day_of_week, sma_cross, label)
df2 <- df2[-c(1:53), ]
names(df2) <- c('day_of_week', 'sma_cross', 'label')

train_df2 <- df2[1:500, 1:2]
train_label_df2 <- df2[1:500, 3]
test_df2 <- df2[500:701, 1:2]
test_label_df2 <- df2[500:701, 3]

nb_classifier <- naiveBayes(train_df2, train_label_df2)
nb_test_predictions <- predict(nb_classifier, test_df2)

CrossTable(nb_test_predictions, test_label_df2,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#48% accuracy
