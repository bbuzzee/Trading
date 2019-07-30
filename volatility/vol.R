require(downloader)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
require(data.table)

download("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vix3mdailyprices.csv", 
         destfile="vxvData.csv")
download("http://www.cboe.com/publish/ScheduledTask/MktData/datahouse/vix6mdailyprices.csv", 
         destfile="vxmtData.csv")




vxv <- xts(read.zoo("vxvData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))
vxmt <- xts(read.zoo("vxmtData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))


symbs <- c("SVXY",
           "SPY",
           "XIVH",
           "UPRO",
           "TMF",
           "ZIV",
           "VXX")

getSymbols(symbs)


spyRets <- Return.calculate(Cl(SPY))
svxyRets <- Return.calculate(Cl(SVXY))
xivhRets <- Return.calculate(Cl(XIVH))
uproRets <- Return.calculate(Cl(UPRO))
tmfRets <- Return.calculate(Cl(TMF))
zivRets <- Return.calculate(Cl(ZIV))
vxxRets <- Return.calculate(Cl(VXZ))

vix3mVxmt <- Cl(vxv)/Cl(vxmt)




stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] <- stats[1,]/stats[4,]
  stats[6,] <- stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] <- "Worst Drawdown"
  rownames(stats)[5] <- "Calmar Ratio"
  rownames(stats)[6] <- "Ulcer Performance Index"
  return(stats)
}

# QUANT R TRADER
ma30 <- EMA(vix3mVxmt, 30)
ma60 <- EMA(vix3mVxmt, 60)
ma200 <- EMA(vix3mVxmt, 90)

plot(tail(vix3mVxmt, 252))
lines(ma60, col = "blue", lwd = 2)
lines(ma200, col = "green", lwd = 2)
points(svxyQR*1.05, col = "green", pch = 15)



svxyQR <- (vix3mVxmt < 1) & (vix3mVxmt < ma60)
svxyQR2 <- (vix3mVxmt < 1) & (vix3mVxmt < ma60) & (ma60 > .9)
zivQR <-  (vix3mVxmt < 1) & (vix3mVxmt < ma60) & (ma60 < .9)


retsSvxy <- lag(svxyQR, 2) * svxyRets
retsZiv <- lag(svxyQR, 2) * zivRets
retsVXX <- lag(!zivQR, 2) * vxxRets
retsSvZv <-  lag(svxyQR2, 2) * svxyRets + lag(zivQR, 2) * zivRets
retsUptmf <- .5*uproRets + .5 * tmfRets
retsCombo <- .5*retsSvZv + .5 * retsUptmf

compare1 <- na.omit(cbind(retsSvxy, retsZiv, retsVXX, retsSvZv, retsUptmf, spyRets, retsCombo))
names(compare1) <- c("SVXY", "ZIV", "VXX", "SVZIV", "UPROTMF", "SPY", "Combo")

yrs <- as.character(seq(from = 2011, to = 2017))

#compare1 <- compare1["2016"]
charts.PerformanceSummary(tail(compare1, 252*2))
stratStats(tail(compare1, 252*2))

tail(svxyQR)

# LONG VOL STRATEGIES


vxxQR1 <- (vix3mVxmt < 1) & (vix3mVxmt > ma60) & (ma60 < .9)
vxxQR2 <- (vix3mVxmt > 1) & (vix3mVxmt > ma60)

retsVol1 <- lag(vxxQR1, 2) * vxxRets
retsVol2 <- lag(vxxQR2, 2) * vxxRets


compare2 <- na.omit(cbind(retsVol1, retsVol2))
stratStats(compare2)

