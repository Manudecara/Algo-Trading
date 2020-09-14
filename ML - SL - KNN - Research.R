setwd("~/Quant Finance")

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
library(class)
library(gmodels)


from ="2016-1-01"
to ="2020-01-01"
symbols = c('GOOG')
getSymbols(symbols, from=from, to=to, adjust=TRUE)
GOOG$close.1 <- Lag(Cl(GOOG), k=1)
GOOG <- GOOG[-1, ]
GOOG <- as.data.frame(GOOG)
setDT(GOOG, keep.rownames = TRUE)
names(GOOG) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', 
                 'Close1')
GOOG$Date <- as.Date(as.character(GOOG$Date, format = '%y/%m/%d'))
GOOG <- as.data.frame(GOOG)


GOOG <- subset(GOOG, select = -c(Open, High, Low, Volume, Adjusted))
higher <- (GOOG$Close > GOOG$Close1)
lower <- (GOOG$Close < GOOG$Close1)

GOOG$labels[higher] <- 'HIGH'
GOOG$labels[lower] <- 'LOW'

GOOG_train <- GOOG[1:700, 2:3]
GOOG_test <- GOOG[701:1005, 2:3]
GOOG_train_labels <- GOOG[1:700, 4]
GOOG_test_labels <- GOOG[701:1005, 4]

#run knn
GOOG_test_pred <- knn(train = GOOG_train, 
                      test = GOOG_test, 
                         cl = GOOG_train_labels, k= 1)
#show results
CrossTable(x = GOOG_test_labels, y = GOOG_test_pred)

accuracy <- rep(0, 30)
k <- 1:30
for(x in k){
  GOOG_test_pred <- knn(train = GOOG_train, test = GOOG_test, 
                        cl= GOOG_train_labels, k= x)
  accuracy[x] <- mean(GOOG_test_pred == GOOG_test_labels)
}

plot(k, accuracy, type = 'b')

#it would seem that at k=17 the algorithm is able to predict the results the best.
#it should be noted that 17 is the square root of the testing sample
#one of the many ways of figuring out how many "k's" to use

#run knn
GOOG_test_pred <- knn(train = GOOG_train, 
                      test = GOOG_test, 
                      cl = GOOG_train_labels, k= 17)
#show results
CrossTable(x = GOOG_test_labels, y = GOOG_test_pred)

#90% accuracy in prediction




