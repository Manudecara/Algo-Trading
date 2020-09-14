setwd("~/Quant Finance")
install.packages("quantmod")
install.packages("FinancialInstrument")
install.packages("PerformanceAnalytics")
install.packages("foreach")
install.packages("devtools")
# Install from github directly
install_github("braverock/blotter")
install_github("braverock/quanstrat")
library(devtools)
library(blotter)
library(quantstrat)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(foreach)
update.packages('blotter', repos="http://R-Forge.R-project.org")



options("getSymbols.warning4.0"=FALSE)
from ="2017-1-01"
to ="2019-01-01"
symbols = c("GOOG","IBM")
getSymbols(symbols, from=from, to=to, adjust=TRUE)
currency("USD")
stock(symbols, currency="USD", multiplier=1)
Sys.setenv(TZ="UTC") #setting up the timezone


GOOG <- merge(GOOG, SAR(GOOG))
GOOG <- na.omit(merge(GOOG, MACD(Cl(GOOG))))
IBM <- merge(IBM, SAR(IBM))
IBM <- na.omit(merge(IBM, MACD(Cl(IBM))))


initEq=1000000
strategy.st <- portfolio.st <- account.st <- "BBands"
rm.strat(strategy.st)

initPortf(portfolio.st, symbols)
initAcct(account.st, portfolios=portfolio.st, initEq = initEq)
initOrders(portfolio.st)

strategy(strategy.st, store=TRUE)

add.indicator(
  strategy.st, name="BBands",
  arguments=list(HLC = quote(Cl(mktdata)), n=20, sd=2),
  label="bbands")

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

# Bull market if Cl < BBands(dn) 
add.signal(
  strategy.st, 
  name= 'sigCrossover',
  arguments = list(column=c("mavg","dn"),
                   relationship="lt"),
  label="long")

# Bear market if Cl > BBands(up) 
add.signal(
  strategy.st, 
  name= 'sigCrossover',
  arguments = list(column=c("mavg.bbands","up.bbands"),
                   relationship="gt"),
  label="short")


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
         name ='ruleSignal', 
         arguments = list(sigcol = 'long',
                          sigval = TRUE,  
                          orderqty = 1000,
                          ordertype = 'market', 
                          orderside = 'long',
                          pricemethod = 'market',
                          osFUN = osMaxPos,
                          orderset = "ocolong",
                          replace = FALSE), 
         type='enter', 
         path.dep = TRUE,
         label= 'EnterLONG')



add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = 'short', 
                          sigval = TRUE,  
                          orderqty = -1000,
                          ordertype = 'market', 
                          orderside = 'short',
                          pricemethod ='market',
                          osFUN = osMaxPos,
                          orderset = "ocolong",
                          replace = FALSE), 
         type='enter', 
         path.dep = TRUE,
         label= 'EnterSHORT')


add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = 'mavg.bbands',
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          pricemethod ='market',
                          replace = TRUE), 
         type = "exit", 
         path.dep =TRUE,
         label = "Exit2SHORT")



add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = 'Cross.mid', 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "short",
                          pricemethod ='market', 
                          replace = TRUE), 
         type = "exit", 
         path.dep =TRUE,
         label = "Exit2LONG")




for(symbols in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = symbols,
              timestamp = from,
              maxpos = 1000)
}




out <- applyStrategy(strategy.st, portfolios=portfolio.st)


updatePortf(Portfolio = portfolio.st)
updateAcct(name = portfolio.st)
updateEndEq(Account = account.st)


chart.Posn(Portfolio=portfolio.st, Symbol = "GOOG", 
           TA = 'add_BBands(n=20, sd=2)
           add_RSI(n=14)')
chart.Posn(Portfolio=portfolio.st, Symbol = "IBM", 
           TA = 'add_BBands(n=20, sd=2)
           add_RSI(n=14)')