#RSI (Quantstrat) - Trading Strategy

setwd("~/Quant Finance")

# Install from github
install_github("braverock/blotter")
install_github("braverock/quanstrat")
library(devtools)
library(blotter)
library(quantstrat)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(xts)
library(dplyr)
library(knitr)
update.packages('blotter', repos="http://R-Forge.R-project.org")


options("getSymbols.warning4.0"=FALSE)
from ="2018-01-01"
to ="2019-01-01"
symbols = c("GOOG","IBM","MSFT","AAPL")
getSymbols(symbols, from=from, to=to, adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
currency("USD")
Sys.setenv(TZ="UTC") #setting up the timezone


initEq=1000000
strategy.st <- portfolio.st <- account.st <- "RSI"
rm.strat(strategy.st)

initPortf(portfolio.st, symbols)
initAcct(account.st, portfolios=portfolio.st, initEq = initEq)
initOrders(portfolio.st)

strategy(strategy.st, store=TRUE)


add.indicator(
  strategy.st, name="RSI",
  arguments=list(price= quote(Cl(mktdata)), n=14),
  label="rsi")

#In quantstrat, there are three ways one can use a signal. It is refer to as 'name':
  
#sigThreshold: more or less than a fixed value
#sigCrossover: when two signals cross over
#sigComparsion: compare two signals

#The 'column' refers to the data for calculation of signal. There are five possible 'relationships':
  
#gt = greater than
#gte = greater than or equal to
#lt = less than
#lte = less than or equal to
#eq = equal to


# Bull market if Cl < RSI 30
add.signal(
  strategy.st, 
  name= 'sigThreshold',
  arguments = list(threshold=30,   
                   column="rsi",
                   relationship="lt"),
                   label="rsi.buy")

# Sell market if CL > RSI 70
add.signal(
  strategy.st, 
  name= 'sigThreshold',
  arguments = list(threshold=70,   
                   column="rsi",
                   relationship="gt"),
                   label="rsi.sell")
  
#While trading signals tell us buy or sell, but it does not specify the execution details.
#Trading rules will specify the following seven elements:
    
#SigCol: Name of Signal
#SigVal: implement when there is signal (or reverse)
#Ordertype: market, stoplimit
#Orderside: long, short
#Pricemethod: market
#Replace: whether to replace other others
#Type: enter or exit the order
  
  
add.rule(strategy.st, 
           name='ruleSignal', 
           arguments = list(sigcol="rsi.buy", 
                            sigval=TRUE,  
                            orderqty=1000,
                            ordertype='market', 
                            orderside='long',
                            pricemethod='market',
                            replace=FALSE), 
         type='enter', path.dep=TRUE)


add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol= "rsi.sell", 
                          sigval=TRUE,  
                          orderqty=1000,
                          ordertype='market', 
                          orderside='long',
                          pricemethod='market',
                          replace=FALSE), 
         type='enter', path.dep=TRUE)


add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "rsi.buy", 
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "short",
                          pricemethod='market',
                          replace = TRUE), 
         type = "exit", path.dep=TRUE)

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "rsi.sell", 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          pricemethod='market', 
                          replace = TRUE), 
         type = "exit", path.dep=TRUE)



out <- applyStrategy(strategy.st, portfolios=portfolio.st)


updatePortf(Portfolio = portfolio.st)
updateAcct(name = portfolio.st)
updateEndEq(Account = account.st)

chart.Posn(Portfolio=portfolio.st, Symbol = "GOOG")
chart.Posn(Portfolio=portfolio.st, Symbol = "IBM")
chart.Posn(Portfolio=portfolio.st, Symbol = "MSFT")
chart.Posn(Portfolio=portfolio.st, Symbol = "AAPL")

