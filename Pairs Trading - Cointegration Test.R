#Pairs Trading - Cointegration Test

setwd("~/Quant Finance")

library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(dplyr)
library(knitr)

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

