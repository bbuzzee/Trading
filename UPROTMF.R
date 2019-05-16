
setwd("~/Finance/trading")
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)

# Formula: Daily Return = [Underlying Index Adjusted Daily Return (including dividend) x Leverage] - (12 month LIBOR)/250 - (Expense Ratio)/250

uproSim <- xts(read.zoo("UPROSIM.csv", header=TRUE, sep=",", format="%m/%d/%Y", index.column = 1))
uproSim <- uproSim[,1]

uproSim$price <- 1
uproSim$x[1,1] <- 0

# Recursive function to find factorial
for (i in 2:length(uproSim$price)){
  uproSim$price[i+1] <- uproSim$x[i]*uproSim$price[i] + uproSim$price[i]
}



tmfSim <- xts(read.zoo("TMFSIM.csv", header=TRUE, sep=",", format="%m/%d/%Y"))
tmfSim <- tmfSim[,1]


stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] <- stats[1,]/stats[4,]
  stats[6,] <- stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] <- "Worst Drawdown"
  rownames(stats)[5] <- "Calmar Ratio"
  rownames(stats)[6] <- "Ulcer Performance Index"
  return(stats)
}


getSymbols("UPRO")
getSymbols("VFINX", from = "2000-12-01")
getSymbols("TMF")
getSymbols("SSO")  


spyRets <- Return.calculate(Ad(VFINX))
uproRets <- Return.calculate(Cl(UPRO))
tmfRets <- Return.calculate(Cl(TMF))
ssoRets <- Return.calculate(Cl(SSO))


# Comapare simulated and observed returns
portRetssim <- .5*uproSim$x + .5*tmfSim$x
portRets <- .5*uproRets + .5*tmfRets

compare0 <- cbind( spyRets, portRetssim, portRets)
names(compare0) <- c("VFINX", "PORTSIM", "PORTOBS")
#compare0 <- tail(compare0, 2000)
charts.PerformanceSummary(compare0)

stratStats(compare0)


# Strategy

smaSPY <- SMA(Cl(VFINX), 600)
sigSPY <- Cl(VFINX) > smaSPY



uproRetsSMA <-  lag(sigSPY,2 ) * uproSim$x
BH <- .5*uproSim$x + .5*tmfSim$x
upro <- lag(sigSPY,2)*BH

comp0  <- cbind(uproRetsSMA, BH, upro)
names(comp0) <- c("SMA", "BH", "UPRO")
# comp0 <- tail(comp0, 300)
charts.PerformanceSummary(comp0)
points(sigSPY, col = "pink")
stratStats(comp0)

