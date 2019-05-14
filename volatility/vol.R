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

getSymbols("SVXY", src = "yahoo")
getSymbols("SPY")
getSymbols("XIVH")
getSymbols("UPRO")
getSymbols("TMF")
getSymbols("VXX")

spyRets <- Return.calculate(Cl(SPY))
svxyRets <- Return.calculate(Cl(SVXY))
xivhRets <- Return.calculate(Cl(XIVH))
uproRets <- Return.calculate(Cl(UPRO))
tmfRets <- Return.calculate(Cl(TMF))
vxxRets <- Return.calculate(Cl(VXX))

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
maLong <- SMA(vix3mVxmt, 60)
ma125 <- SMA(vix3mVxmt, 125)
ma150 <- SMA(vix3mVxmt, 150)

ma_avg <- mean(c(maLong, ma125, ma150))

svxyQR <- vix3mVxmt < 1 & vix3mVxmt < maLong 
vxxQR <- vix3mVxmt > 1 & ma_avg > 1

retsSvxy <- lag(svxyQR, 2) * svxyRets 
retsXivh <- lag(svxyQR, 2) * xivhRets
retsLev <- .5*uproRets + .5*tmfRets
retsPort <- .5*retsTTO+ .5*retsLev

compare1 <- na.omit(cbind(retsTTO, retsLev, retsPort, spyRets))
names(compare1) <- c("SVXY", "UPRO", "Vol-UPRO-TMF", "SPY")

# compare1 <- tail(compare1, 300)
charts.PerformanceSummary(compare1)
stratStats(compare1)

tail(svxyQR)

# TTO

svxyOp <- CalculateReturns(Op(SVXY))
vxxOp <- CalculateReturns(Op(VXX))

vol2 <- rollapply(Cl(SPY), FUN = sd.annualized, width = 2)

df <- merge(vxv, vol2, join = "inner")
colnames(df) <- c("", "", "", "vix3m", "vol2day")

sigSvxyTTO <- EMA(df$vix3m - df$vol2day, n = 2) > 1 
sigVxxTTO <- EMA(df$vix3m - df$vol2day, n = 2) < 1

retsTTO <- lag(sigSvxyTTO, 2) * svxyOp   + lag(sigVxxTTO, 2) * vxxOp
# retsTTO <- tail(retsTTO, 200)
compare <- na.omit(cbind(spyRets, retsTTO))
stratStats(compare)
charts.PerformanceSummary(compare)

tail(sigSvxyTTO)

# add rolling sd smaller than something as an indicator
chart.RollingPerformance(retsTTO, width = 22*6,  FUN = 'Return.annualized')



sum(lag(sigSvxyTTO) != sigSvxyTTO, na.rm = T)/2607*360




# MOving Average SPY/UPRO Strategy

getSymbols("SSO")
maSpy30 <- SMA(Cl(SPY), 30)
maSpy60 <- SMA(Cl(SPY), 60)
maSpy200 <- SMA(Cl(SPY), 200)
maSso200 <- SMA(Cl(SPY), 200)

uproSig <- Cl(SPY) > maSpy200
ssoSig <- Cl(SSO) > maSso200

retsUpro200 <-  uproSig*uproRets
retsSpy200 <- lag(uproSig,1)*uproRets 

compare3 <- na.omit(cbind(retsUpro200, uproRets, spyRets, retsSpy200))

stratStats(compare3)
charts.PerformanceSummary(compare3)



uprosig18 <- uproSig["2018"]
uprorets18 <- uproRets["2017"]

uprosig18*uprorets18
