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
getSymbols("VXX")

spyRets <- Return.calculate(Cl(SPY))
svxyRets <- Return.calculate(Cl(SVXY))
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

retsLong <- lag(svxyQR, 2) * svxyRets

# retsLong <- tail(retsLong, 200)

charts.PerformanceSummary(retsLong)
stratStats(retsLong)
tail(svxyQR)

# TTO

vol2 <- rollapply(Cl(SPY), FUN = sd.annualized, width = 2)

df <- merge(vxv, vol2, join = "inner")
colnames(df) <- c("", "", "", "vix3m", "vol2day")

sigSvxyTTO <- EMA(df$vix3m - df$vol2day, n = 5) > 1 
sigVxxTTO <- EMA(df$vix3m - df$vol2day, n = 5) < 1

retsTTO <- lag(sigSvxyTTO, 1) * svxyRets   # + lag(!sigSvxyTTO, 1) * vxxRets
# retsTTO <- tail(retsTTO, 200)
compare <- na.omit(cbind(spyRets, retsTTO))
stratStats(compare)
charts.PerformanceSummary(compare)

tail(sigSvxyTTO)

# add rolling sd smaller than something as an indicator
chart.RollingPerformance(retsTTO, width = 22*6,  FUN = 'Return.annualized')


tail(sigSvxyTTO)

sum(lag(sigSvxyTTO) != sigSvxyTTO, na.rm = T)/2607*360
