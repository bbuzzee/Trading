# This script combines a long-only UPRO/TMF strategy with a growth-trend timing strategy outlined on the
# philosophical economics blog.
# For more information see: http://www.philosophicaleconomics.com/2016/02/uetrend/
# And https://www.bogleheads.org/forum/viewtopic.php?f=10&t=272007


# make sure to install all these packages before attempting to run this code.
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(fredr)

getFedData <- function(tag = "DFF"){
  
  #=== pull data ===#
  api.key <- "bea3df08931786951992bd042f7b2254"
  fredr_set_key(api.key)
  
  # REQUIRES global fred object to be loaded
  dt <- fredr(tag)
  dt <- dt %>% as_tibble()
  
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

plot(cue,grid.ticks.on = "years")

#getSymbols("VFINX", from = "2000-12-01")
#getSymbols("UPRO")


spy <- read.csv("spyprice.csv", skip = 510, nrows = 7681)
names(spy) <- c("date", "price")
spy$date <- as.Date(spy$date, format = "%d-%b-%Y")
spy <- xts(spy$price, order.by = spy$date)

uproRets <- xts(read.zoo("UPROSIM.csv", header=TRUE, sep=",", format="%m/%d/%Y", index.column = 1))
uproRets <- uproRets[,1]


tmfRets <- xts(read.zoo("TMFSIM.csv", header=TRUE, sep=",", format="%m/%d/%Y"))
tmfRets<- tmfRets [,1]


#uproRets <- Return.calculate(Cl(UPRO))
spyRets <- Return.calculate(spy)



df <- merge(spy, cue, fill = NA )
df <- na.locf(df)

df <- df[index(df) > min(index(spy)),]
spySma10 <- SMA(df$spy, n = 22*12)
unrate12 <- SMA(df$cue, n = 22*10)

# Buy when unemployment is below its 12 mo moving average
# 
signal <- !((df$cue > unrate12) & (df$spy < spySma10))


sp <- lag(signal, 1) *spyRets
up <- lag(signal, 1) *uproRets
uptmfSig <- lag(signal, 2) *.5 * uproRets + lag(signal, 2) *.5 * tmfRets 
bhuptmf <- .5 * uproRets + .5 * tmfRets 


rets <- cbind(spyRets, uproRets, sp, up,uptmfSig, bhuptmf)
names(rets) <- c("BHspy", "BHupro", "spySig", "uproSig", "uptmfSig", "BHuptmf")

stratStats(rets)
charts.PerformanceSummary(rets)

knitr::kable(stratStats(rets))
                         