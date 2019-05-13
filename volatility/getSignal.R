require(downloader)
require(quantmod)
require(mailR)
library(rJava)

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
  
  last_sig <- tail(svxyQR, 5)
  
  action <- ifelse(coredata(last_sig), "BUY", "SELL")
  
  
  message <- paste0("The signal for ", index(last_sig), " is ", action,". ",  "You should ", action,  " by the end of the day ", index(last_sig) + 1)
  
  table <- data.frame(Date = index(last_sig), "Action" = action)
  return(table)
}

getSignal()


sendSignal <- function(address = "benbuzzee@gmail.com"){
  
  send.mail(from = address,
            to = address,
            subject = "Daily Volatility Signal",
            body = "message",
            smtp = list(host.name = "aspmx.l.google.com", port = 25),
            authenticate = FALSE,
            send = TRUE)
}


sendSignal()
