#Cointegration - Pair trading

setwd("~/Quant Finance")

library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(dplyr)
library(knitr)
library(ggplot2)
library(data.table)


start_date = "2012-01-01" 
end_date = "2020-01-01" 

title <- c("Coca-Cola vs Pepsi")
stock_list <- c("KO","PEP")
getSymbols(stock_list, src = "yahoo", from = start_date, to=end_date)

#when we have more than one stock we work with a list
stock_pair <- list(a = (Cl(KO)), b = (Cl(PEP)), name= title)


#Pass in the pair of stocks and do the necessary checks to see if they are cointegrated
  
#Step 1: Plot the pairs
dev.new() #creates multiple graphs
par(mfrow=c(2,2))
plot(merge(stock_pair$a, stock_pair$b), main=stock_pair$name, ylab="Price", type="l")


#Step 2: Calculate the daily returns
returns_a <- dailyReturn(stock_pair$a)
returns_b <- dailyReturn(stock_pair$b)
returns_a <- as.numeric(returns_a) 
returns_b <- as.numeric(returns_b)
  
#Step 3: Regress the daily returns onto each other
#Regression finds BETA and C in the linear regression retA = BETA * retB + C
regression <- lm(returns_a ~ returns_b + 0)
regression
beta <- coef(regression)
beta
print(paste("The beta/Hedge Ratio is: ",beta,sep=""))
#Plot the daily returns
plot(x=returns_b,y=returns_a,type="p",main="Regression of returns for Stock A & B") 
#Plot linear regression line
lines(x=returns_b,y=(returns_b*beta),col="blue")
  
#Step 4: Use the regression co-efficients to generate the spread
spread <- stock_pair$a - beta*stock_pair$b 
spreadRet <- dailyReturn(spread)
plot((spreadRet), type="l",main="Spread Returns") 
plot(spread, type="l",main="Actual Spread") 

#For a cointegrated spread the cumsum should not deviate very far from 0
#For a non-cointegrated spread the cumsum will likely show some trending characteristics
  
#Step 5: Use the ADF to test if the spread is stationary
#use tSeries library
adfResults <- adf.test(spread,k=0,alternative="stationary")
  
print(adfResults)
if(adfResults$p.value <= 0.05){
print(paste("The spread is likely Cointegrated with a pvalue of ",adfResults$p.value,sep=""))
} else {
print(paste("The spread is likely NOT Cointegrated with a pvalue of ",adfResults$p.value,sep=""))
}



#Pairs Trading
#Backtesting 

start_date = "2012-01-01" 
end_date = "2020-01-01" 

title <- c("Coca-Cola vs Pepsi")
stock_list <- c("KO","PEP")
getSymbols(stock_list, src = "yahoo", from = start_date, to=end_date)

#update the list of stocks
stock_pair <- list(a = (Cl(KO)), b = (Cl(PEP)), name= title, hedge_ratio= 0.677)

#Strategy is if the spread is greater/shorter than the upper/lower band, 
#then go short/long accordingly

#Step 1: Generate the spread
spread <- stock_pair$a - stock_pair$hedge_ratio*stock_pair$b
spread <- na.omit(merge(spread, BBands(spread)))
names(spread) <- c('close', 'dn', 'mavg', 'up', 'pctb')

# short when spread close is above up
spread$sig[spread$close > spread$up] <- -1 
# long when spread close is below dn
spread$sig[spread$close < spread$dn] <- 1 

spread$close - spread$mavg
sign(spread$close - spread$mavg)
(diff(sign(spread$close - spread$mavg)))
!= 0

# Flat where spread close crossed the mavg
spread$sig[(diff(sign(spread$close - spread$mavg)) != 0)] <- 0

spread$sig[1] <- 0 # flat on the first day
spread$sig[nrow(spread)] <- 0 # flat on the last day
spread$sig <- na.locf(spread$sig) # wherever sig is NA, copy previous value to next row

spread$sig <- Lag(spread$sig)
spread$sig[1] <- 0 # flat on the first day

table(spread$sig)

dev.new()
par(mfrow=c(1,1))
plot(spread$mavg, main= "KO vs PEP spread with bollinger bands", 
     col="red", ylab="Spread", type='l')
lines(spread$up, col="purple")
lines(spread$dn, col="purple")
lines(spread$close, col="blue")

plot(spread$close)
addEventLines(events = spread$sig, lwd = 0.5, col = "red")


#Calculate spread daily ret
a_returns <- dailyReturn(stock_pair$a)
b_returns <- dailyReturn(stock_pair$b)
ab_daily_returns <- a_returns - stock_pair$hedge_ratio*b_returns

#create a table with your returns
Returns <- na.omit(spread$sig) * ab_daily_returns

#plot the benchmark of the stock with the performance of your strategy
charts.PerformanceSummary(cbind(dailyReturn(spread$close),Returns))

#create a calendar table of returns
kable(table.CalendarReturns(Returns), caption = "Calendar Returns")








