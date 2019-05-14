require(downloader)
require(quantmod)
require(mailR)
require(htmlTable)
# mailR requires 32 bit R and java be installed

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
  
  
  table <- data.frame(index(last_sig), action)
  names(table) <- c("Date", "Action")
  body <-htmlTable(table, rnames = FALSE)
  return(body)
}

# getSignal()


sendSignal <- function(address = "benbuzzee@gmail.com", body = "message"){
  
  send.mail(from = address,
            to = address,
            subject = "Daily Volatility Signal",
            body = body,
            smtp = list(host.name = "aspmx.l.google.com", port = 25),
            authenticate = FALSE,
            html = TRUE,
            send = TRUE)
}


# sendSignal(body = getSignal())
