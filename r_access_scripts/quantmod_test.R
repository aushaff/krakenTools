library(quantmod)
library(PerformanceAnalytics)
getSymbols("IBM", src= "yahoo", from = "2000-01-01", to ="2015-09-25") #download IBM, from the stipulated range of dates

size1 <- function (price, day) {
  
  rsi <- RSI(price, day)
  rsi <- lag(rsi, 1)
  rsi[is.na(rsi)] <- 0
  
  holdings1 <- integer(nrow(price))
  
  # assume start buying from the 15th day onwards,
  # since we are using RSI, n=14
  for (i in (day+1):nrow(price)) {
  
    if (rsi[i] < 30) {
      # if RSI<30, we buy one share of IBM
      holdings1[i] <- holdings1[i - 1] + 1
    }
    else if (rsi[i] < 50) {
      # if 30<RSI<50, we don't buy or sell,
      # so that the holdings does not change
      holdings1[i] <- holdings1[i - 1]
    } else {
      # sell all if RSI>50
      holdings1[i] <- 0
    }
  }
  reclass(holdings1, price)
}

ret1<-dailyReturn(Cl(IBM))*size1(Cl(IBM),14)
charts.PerformanceSummary(ret1)
