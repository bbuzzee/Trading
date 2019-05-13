require(quantmod)
require(PerformanceAnalytics)
library(tidyverse)
library(FredR)

getFedData <- function(tag = "DFF"){
  
  #=== pull data ===#
  api.key <- "59f051c54cc49a42ef1f3ba3426792b8"
  fred <- FredR::FredR(api.key)
  
  # REQUIRES global fred object to be loaded
  dt <- fred$series.observations(tag)
  dt <- dt %>% as.tibble()
  
  print("removing NAs")
  # remove NA's
  dt$value <- as.numeric(dt$value)
  dt <- dt[!is.na(dt$value),]
  
  #=== clean dataframe up ===#
  print("cleaning data")
  # remove unneeded columns
  dt <-  dt %>% dplyr::select(date, value) %>% dplyr::mutate(date = as.Date(date))
  
  # round all dates to nearest month
  dt$date <- dt$date %>% lubridate::round_date(unit = "month")
  
  # for each month only retain median value
  dt <- dt %>% dplyr::group_by(date) %>% dplyr::summarize(value = median(value))
  
  return(dt)
  
}

stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] <- stats[1,]/stats[4,]
  stats[6,] <- stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] <- "Worst Drawdown"
  rownames(stats)[5] <- "Calmar Ratio"
  rownames(stats)[6] <- "Ulcer Performance Index"
  return(stats)
}


cue <- getFedData(tag = "UNRATE")
cue <- xts(cue$value, order.by = cue$date)

getSymbols("SPY")
getSymbols("UPRO")
uproRets <- Return.calculate(Cl(UPRO))
spyRets <- Return.calculate(Cl(SPY))

df <- merge(Cl(SPY), cue, fill = NA )
df <- na.locf(df)

df <- df[index(df) > min(index(Cl(SPY))),]
spySma10 <- SMA(df$SPY.Close, n = 10)
unrate12 <- SMA(df$cue, n = 12)

# Buy when unemployment is below its 12 mo moving average
# 
signal <- !((df$cue> unrate12) & (df$SPY.Close < spySma10))

stratRets <- lag(signal, 1) * spyRets

stratStats(stratRets)
stratStats(spyRets)
charts.PerformanceSummary(stratRets)
monyear(index(df))
yearmon(df)
