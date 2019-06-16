require(downloader)
require(quantmod)
require(mailR)
require(htmlTable)
require(PerformanceAnalytics)
require(fredr)
library(tidyverse)
# mailR requires 32 bit R and java be installed

library(rJava)


getSignal <- function(){

  
  # ====================================== PHIL ECON UPRO/TMF SIGNAL ======================================================
  
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
  
  # ======== UNEMPLOYMENT ==========
  
  cue <- getFedData(tag = "UNRATE")
  cue <- xts(cue$value, order.by = cue$date)
  
  # ======== RETAIL ========== 
  # ========= SPY ===========
  
  getSymbols("VFINX", from = "2000-12-01")
  spy <- Cl(VFINX)
  names(spy) <- "spy"
  
  
  # ========= SIGNAL ===========
  
  df <- merge(spy, cue, fill = NA)
  df <- na.locf(df)
  
  df <- df[index(df) > min(index(spy)),]
  spySma10 <- EMA(df$spy, n = 22*12)
  unrate12 <- EMA(df$cue, n = 22*12)
  
  # Buy when unemployment is below its 12 mo moving average
  signal_phil <- !((df$cue > unrate12) & (df$spy < spySma10))
  
  last_sig_phil <- tail(signal_phil, 5)

  action <- ifelse(coredata(last_sig_phil), "BUY", "SELL")
  
  table <- data.frame(index(last_sig_phil), action)

  
  
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

  table <- cbind(index(last_sig), action, table)
  
  
  
  
  
  #================================ TTO SIGNAL ===================================
  
  getSymbols("SPY")

  vol2 <- rollapply(Cl(SPY), FUN = sd.annualized, width = 2)
  
  df <- merge(vxv, vol2, join = "inner")
  colnames(df) <- c("", "", "", "vix3m", "vol2day")
  
  sigSvxyTTO <- EMA(df$vix3m - df$vol2day, n = 2) > 1 
  
  last_sig_tto <- tail(sigSvxyTTO, 5)
  
  action_tto  <- ifelse(coredata(last_sig_tto), "BUY", "SELL")
  
  table <- cbind(data.frame(index(last_sig_tto), action_tto), table)
  
  names(table) <- c("DateTTO", "ActionTTO", "DateQR", "ActionQR", "DateUpro", "ActionUpro")
  
  body <- htmlTable(table, rnames = FALSE)
  
  return(body)

}

# d <-  getSignal()


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
