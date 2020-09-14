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
library(rpart)
library(rpart.plot)

start_date = as.Date("2015-01-01")
end_date = as.Date("2020-01-01")
getSymbols("GOOG", src = "yahoo", from = start_date, to = end_date)

#I will make a decision tree for predicting stock movement through classification
#using tehnical idicators
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

#create the binary variable to be predicted
GOOG$class <- ifelse(GOOG$return >= 0, 'UP', 'DOWN')
GOOG <- GOOG[ ,-c(1:6)]
GOOG <- GOOG[ ,-7]
str(GOOG)
GOOG$class <- as.factor(GOOG$class)
str(GOOG)

train <- GOOG[1:800, ]
test <- GOOG[801:1176, ]

prop.table(table(train$class))
prop.table(table(test$class))

#create normal decision tree model
dtree_model <- rpart(GOOG$class ~ GOOG$rsi+GOOG$sma+GOOG$sar+GOOG$wpr+GOOG$cci+GOOG$adx, 
                     data = train, method= 'class')
rpart.plot(dtree_model)
dtree_model$variable.importance
#create prediction model
dtree_predict_class <- rpart.predict(dtree_model, type = "class")
#check results
CrossTable(GOOG$class, dtree_predict_class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))


#create new model but with variables classified by 'information split'
dtree_model2 <- rpart(GOOG$class ~ GOOG$rsi+GOOG$sma+GOOG$sar+GOOG$wpr+GOOG$cci+GOOG$adx, 
                     data = train, method= 'class', parms = list(split = 'information'))
rpart.plot(dtree_model2)
dtree_model2$variable.importance
#create prediction model
dtree_predict_class2 <- rpart.predict(dtree_model2, type = "class")
#check results
CrossTable(GOOG$class, dtree_predict_class2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))


#sometimes some errors are more costly than others 
#therefore error matrixes are necessary
error_cost <- matrix(c(0,1,3,0), byrow = TRUE, nrow = 2)
error_cost

#create new model and introducte error matrix
dtree_model3 <- rpart(GOOG$class ~ GOOG$rsi+GOOG$sma+GOOG$sar+GOOG$wpr+GOOG$cci+GOOG$adx, 
                      data = train, method= 'class', parms = list(loss = error_cost))
rpart.plot(dtree_model3)
dtree_model3$variable.importance
#create prediction model
dtree_predict_class3 <- rpart.predict(dtree_model3, type = "class")
#check results
CrossTable(GOOG$class, dtree_predict_class3,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))



