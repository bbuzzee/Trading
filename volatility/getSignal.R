require(downloader)
require(quantmod)


getSignal <- function(){

  # RETRIEVE THE DATA
  download("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vix3mdailyprices.csv", 
           destfile="vxvData.csv")
  download("http://www.cboe.com/publish/ScheduledTask/MktData/datahouse/vix6mdailyprices.csv", 
           destfile="vxmtData.csv")
  
  
  vxv <- xts(read.zoo("vxvData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))
  vxmt <- xts(read.zoo("vxmtData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))
  
  

  # Our signal is that the there is more volatility expected long term than short term
  # and the ratio is above its 60 day moving average.
  
  vix3mVxmt <- Cl(vxv)/Cl(vxmt)
  maLong <- SMA(vix3mVxmt, 60)
  
  
  svxyQR <- vix3mVxmt < 1 & vix3mVxmt < maLong
  
  last_sig <- tail(svxyQR, 1)
  
  action <- ifelse(coredata(last_sig), "buy", "sell")
  
  
  message <- paste0("The signal for ", index(last_sig), " is ", action,". ",  "You should ", action,  " by the end of the day ", index(last_sig) + 1)
  return(message)
}

getSignal()
