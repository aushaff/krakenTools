# should return the index as 59:59 (m:s) for the required period
create_index <- function(xts_in, period_in, sub_period) {

  if(period_in =="minutes") {
    
    # fill the missing days with NAs
    index <- seq(lubridate::floor_date(start(xts_in), "mins")+59,
                 lubridate::floor_date(end(xts_in), "mins")+59, by = "mins")
    
  } else if(period_in == "hour") {
    
    index <- seq(lubridate::floor_date(start(xts_in), "hour")+3599,
                 lubridate::floor_date(end(xts_in), "hour")+3599, by = "hour")
    
  } else if(period_in == "day"){
    
    index <- seq(lubridate::floor_date(start(xts_in), "day")+(3600*24)-1,
                 lubridate::floor_date(end(xts_in), "day")+(3600*24)-1, by = "day")
  }
}

to_period <- function(xts_in, period_in, out_dir, sub_period = 1) {
  
  curr_ohlc <-xts::to.period(xts_in, 
                            period = period_in,
                            k = sub_period)
  names(curr_ohlc) <- c("Open", "High", "Low", "Close", "Volume")
  
  index <- create_index(curr_ohlc, period_in, sub_period)

  # ensure that we don't duplicate the index with existing time
  ind_diff <- as.character(index) %in% as.character(index(curr_ohlc))
  new_index <- index[!ind_diff]
  curr_ohlc_nas <- merge(curr_ohlc, zoo(, new_index))
  
  
  # fill the NAs with the previous non-NA value
  curr_ohlc_nas_last <- na.locf(curr_ohlc_nas)
  
  if(period_in == "minutes") {
    
    curr_ohlc_nas_last <- curr_ohlc_nas_last[floor(.indexsec(curr_ohlc_nas_last))==59,]
    
  } else if(period_in == "hour") {  
    
    curr_ohlc_nas_last <- curr_ohlc_nas_last[(floor(.indexmin(curr_ohlc_nas_last))==59&
                                              floor(.indexsec(curr_ohlc_nas_last))==59),]

  } else if(period_in =="day") {
    
    curr_ohlc_nas_last <- curr_ohlc_nas_last[(.indexhour(curr_ohlc_nas_last)==23&
                                                floor(.indexmin(curr_ohlc_nas_last))==59&
                                                floor(.indexsec(curr_ohlc_nas_last))==59),]
    
  }
    
  return(curr_ohlc_nas_last)
    
}