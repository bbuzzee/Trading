require(downloader)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
require(data.table)

download("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vix3mdailyprices.csv", 
         destfile="vxvData.csv")

# changes vxmt to vix6m to get current data
download("http://www.cboe.com/publish/ScheduledTask/MktData/datahouse/vix6mdailyprices.csv", 
         destfile="vxmtData.csv")

VIX <- fread("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixcurrent.csv", skip = 1)
VIXdates <- VIX$Date
VIX$Date <- NULL; VIX <- xts(VIX, order.by=as.Date(VIXdates, format = '%m/%d/%Y'))


vxv <- xts(read.zoo("vxvData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))
vxmt <- xts(read.zoo("vxmtData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))


getSymbols("ZIV", src = "yahoo")
getSymbols("SVXY", src = "yahoo")

# use ZIV instead of xiv
xivRets <- CalculateReturns(SVXY$SVXY.Adjusted)
#xivRets <- CalculateReturns(ZIV$ZIV.Adjusted)


#============================================================

vixVix3m <- Cl(VIX)/Cl(vxv)
vixVxmt <- Cl(VIX)/Cl(vxmt)
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

# changed from 60 day to 90 date MA
maShort <- SMA(vixVix3m, 90)
maMed <- SMA(vixVxmt, 90)
maLong <- SMA(vix3mVxmt, 90)

sigShort <- vixVix3m < 1 & vixVix3m < maShort
sigMed <- vixVxmt < 1 & vixVxmt < maMed 
sigLong <- vix3mVxmt < 1 & vix3mVxmt < maLong 

retsShort <- lag(sigShort, 2) * xivRets 
retsMed <- lag(sigMed, 2) * xivRets 
retsLong <- lag(sigLong, 2) * xivRets

compare <- na.omit(cbind(retsShort, retsMed, retsLong))
colnames(compare) <- c("Short", "Medium", "Long")
charts.PerformanceSummary(compare)
stratStats(compare)
