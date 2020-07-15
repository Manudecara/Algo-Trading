#Pairs Trading - Strategy

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
