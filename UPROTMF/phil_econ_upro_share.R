# This script combines leverage with a timing model first described on the philosophical economics blog.
# For more information on the timing model see: http://www.philosophicaleconomics.com/2016/02/uetrend/
# Hedgefundies thread on a upro/tmf buy and hold strategy is available
# at https://www.bogleheads.org/forum/viewtopic.php?f=10&t=272007
# Finally, willthrill is implementing a similar unleveraged strategy and has a lively discussion
# thread at https://www.bogleheads.org/forum/viewtopic.php?f=10&t=270035


# make sure to install all these packages before attempting to run this code.
# also make sure your working directory is the same location you downloaded the csv files to

# install.packages("quantmod")
# install.packages("PerformanceAnalytics")
# install.packages("tidyverse")

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)



#=====================================================
# DEFINE FUNCTIONS
#=====================================================



# This function aggregates several metrics we can use to judge performance/risk.
stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  rownames(stats)[4] <- "Worst Drawdown"
  return(stats)
}



#=====================================================
# READ IN DATA AND CONVERT TO XTS OBJECTS
#=====================================================

# We'll have four sets of data--simulated upro/tmf returns,
# the unemployment rate, and daily spy prices
# you should have them all downloaded in your working directory
# with the same names I used (including same capitalization)



# CIVILIAN UNEMPLOYMENT #
# Downloaded from federal reserve at https://fred.stlouisfed.org/series/UNRATE/

cue <- read.csv("UNRATE.csv", header=TRUE)
# date isn't read in as a Date datatype, so we need to convert it.
cue$DATE <- as.Date(cue$DATE, format = "%Y-%m-%d")
# xts objects make time series manipulations/analysis easier
# so we'll convert all our data to xts objects
cue <- xts(cue$UNRATE, order.by = cue$DATE)

# plot(cue, grid.ticks.on = "years")


# SPY PRICES #
# Downloaded from http://www.cboe.com/micro/buywrite/dailypricehistory.xls
# I copied spy price and date to a new sheet and saved as a csv file
# CBOE changes the format of dates after the first 500ish rows, so i opt to skip them rather
# than deal with it

spy <- read.csv("spyprice.csv", skip = 510, nrows = 7681)
names(spy) <- c("date", "price")
spy$date <- as.Date(spy$date, format = "%d-%b-%Y")
spy <- xts(spy$price, order.by = spy$date)

spyRets <- Return.calculate(spy)

# UPRO/TMF #
# Simulated upro/tmf data comes from hedgefundie

uproRets <- read.csv("UPROSIM.csv", header=TRUE)
names(uproRets) <- c("date", "return", "")
# get rid of percent signs and convert to numeric datatype
uproRets$return <- as.numeric(sub("%", "", uproRets$return))
# 3% is read in as 3 instead of .03, so we need to divide rets by 100
uproRets$return <- uproRets$return/100
uproRets$date <- as.Date(uproRets$date, format = "%m/%d/%y")
uproRets <- xts(uproRets$return, order.by = uproRets$date)


# tmf gets the same treatment
tmfRets <- read.csv("TMFSIM.csv", header=TRUE)
names(tmfRets) <- c("date", "return", "")
tmfRets$return <- as.numeric(sub("%", "", tmfRets$return))
tmfRets$return <- tmfRets$return/100
tmfRets$date <- as.Date(tmfRets$date, format = "%m/%d/%y")
tmfRets <- xts(tmfRets$return, order.by = tmfRets$date)



# we will use this dataframe to compute our signal, merges by date by default
df <- merge(spy, cue, fill = NA )

# unemployment numbers are monthly, so we need to "fill in" the unemployment rate to the rest of the days
# the na.locf function does this
df <- na.locf(df)

# make sure there are no missing values
df <- df[index(df) > min(index(spy)),]


#=====================================================
# cOMPUTE SIGNAL
#===================================================== 

# compute simple moving averages, 22 trading days per month
spySma10 <- SMA(df$spy, n = 22*10)
unrate12 <- SMA(df$cue, n = 22*12)

# Our signal is going to be a dated vector
# Since it is an xts object, when we multiply by returns it defaults to same date comparisons

# SELL when unemployment is above its moving average AND spy is below its respective MA
# Otherwise be long
signal <- !((df$cue > unrate12) & (df$spy < spySma10))


#=====================================================
# Calculate returns of various strategies
#=====================================================

# If our returns are daily closing values, which I believe they are,
# a lag of one requires we get the signal AND buy at the same close, which isn't possible.
# A lag of two assumes we buy at the close the day after our signal fires.

# long 100% spy at signal
spSig <- lag(signal, 2) * spyRets

# long 100% upro at signal
upSig <- lag(signal, 2) * uproRets

# go 50/50 upro/tmf at signal
uptmfSig <- lag(signal, 2)*.5*uproRets + lag(signal, 2)*.5*tmfRets 

# buy and hold 50/50 upro/tmf
bhuptmf <- .5*uproRets + .5*tmfRets 

# spyRets and uproRets represent straight buy and hold
# I don't account for trading costs, so this analysis is only useful
# for low-frequency strategies
rets <- cbind(spyRets, uproRets, bhuptmf, spSig, upSig, uptmfSig)
names(rets) <- c("BHspy", "BHupro","BHuptmf", "spySig", "uproSig", "uptmfSig")

print(stratStats(rets))
charts.PerformanceSummary(rets, main='Comparison of Cumulative Performance')

# Going long 50/50 upro/tmf at the signal appears to be the clear winner in terms
# of risk adjusted returns and max drawdown.

                         