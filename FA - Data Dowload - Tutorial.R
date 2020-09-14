Income <- read.csv(file = '~/Desktop/Fundamental Data/us-income-annual.csv', 
                   head = TRUE, sep=";")
Balance <- read.csv(file = '~/Desktop/Fundamental Data/us-balance-annual.csv', 
                    head = TRUE, sep=";")
CashFlow <- read.csv(file = '~/Desktop/Fundamental Data/us-cashflow-annual.csv', 
                     head = TRUE, sep=";")

regression <- lm(Income$Revenue ~ Income$Research...Development)
a <- regression$coefficients[1]
b <- regression$coefficients[2]
plot(Income$Research...Development, Income$Revenue)
abline(a,b, col = "blue")

