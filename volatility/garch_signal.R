require(rugarch)
require(quantmod)
require(TTR)
require(PerformanceAnalytics)

#=========== FIT GARCH MODEL =======================
# get SPY data from Yahoo 
getSymbols("SPY", from = '1990-01-01')

spyRets = na.omit(Return.calculate(Ad(SPY)))

# clearly see clusters
plot(spyRets)


# GJR garch with AR1 innovations under a skewed student T distribution for returns
gjrSpec = ugarchspec(mean.model = list(armaOrder = c(1,0)),
                     variance.model = list(model = "gjrGARCH",
                                           variance.targeting = TRUE),
                     distribution.model = "sstd")


# Use rolling window of 504 days, refitting the model every 22 trading days
t1 = Sys.time()

garchroll = ugarchroll(gjrSpec, data = spyRets, 
                       n.start = 504, refit.window = "moving", refit.every = 22)
t2 = Sys.time()
print(t2-t1)

# convert predictions to data frame
garchroll = as.data.frame(garchroll)

#============================================================================================


#============== COMPARE SD AND PRED GARCH SD TO VIX FOR SIGNAL ==============================
getSymbols('^VIX', from = '1990-01-01')


# convert GARCH sigma predictions to same scale as the VIX by annualizing, multiplying by 100
garchPreds = xts(garchroll$Sigma * sqrt(252) * 100, order.by=as.Date(rownames(garchroll)))
diff = garchPreds - Ad(VIX)



getSymbols("SVXY", src = "yahoo")
getSymbols("ZIV", src = "yahoo")

zivRets = na.omit(Return.calculate(Cl(ZIV)))
svxyRets = na.omit(Return.calculate(Cl(SVXY)))


# Signal compare future looking (vix) to current
zivSig = diff < 0 


garchZiv = lag(zivSig, 2) * zivRets  # + lag(vxzSig, 2) * vxzRets
garchSvxy = lag(zivSig, 2) * svxyRets 

histSpy = runSD(spyRets, n = 21, sample = FALSE) * sqrt(252) * 100
spyDiff = histSpy - Ad(VIX)

# same thing with different signal
zivSig = spyDiff < 0 


spyZiv = lag(zivSig, 2) * zivRets # + lag(vxzSig, 2) * vxzRets
spySvxy = lag(zivSig, 2) * svxyRets



compare = na.omit(cbind(garchZiv, garchSvxy, spyZiv, spySvxy))
colnames(compare) = c("gjrGARCH", "garchSvxy", "spyZiv", "spySvxy")




stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] = stats[1,]/stats[4,]
  stats[6,] = stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] = "Worst Drawdown"
  rownames(stats)[5] = "Calmar Ratio"
  rownames(stats)[6] = "Ulcer Performance Index"
  return(stats)
}



charts.PerformanceSummary(compare)
stratStats(compare)
