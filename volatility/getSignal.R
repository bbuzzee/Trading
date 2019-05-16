require(downloader)
require(quantmod)
require(mailR)

require(htmlTable)
# mailR requires 32 bit R and java be installed

library(rJava)


getSignal <- function(){

  
  
  # ====================================== QUANTSTRATTRADER STRAT ======================================================
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
  
  
  
  
  
  #================================ TTO SIGNAL ===================================
  
  svxyOp <- CalculateReturns(Op(SVXY))
  vxxOp <- CalculateReturns(Op(VXX))
  
  vol2 <- rollapply(Cl(SPY), FUN = sd.annualized, width = 2)
  
  df <- merge(vxv, vol2, join = "inner")
  colnames(df) <- c("", "", "", "vix3m", "vol2day")
  
  sigSvxyTTO <- EMA(df$vix3m - df$vol2day, n = 2) > 1 

  retsTTO <- lag(sigSvxyTTO, 2) * svxyOp #   +  lag(sigVxxTTO, 2) * vxxOp
  
  last_sig_tto <- tail(retsTTO, 5)
  
  action_tto  <- ifelse(coredata(last_sig_tto), "BUY", "SELL")
  
  table <- cbind(data.frame(index(last_sig_tto), action_tto), table)
  
  names(table) <- c("DateTTO", "ActionTTO", "DateQR", "ActionQR")
  
  body <- htmlTable(table, rnames = FALSE)
  
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


sendSignal(body = getSignal())
