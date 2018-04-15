to_period <- function(xts_in, period_in, out_dir, sub_period = 1) {
  
  curr_xts <-xts::to.period(xts_in, 
                            period = period_in,
                            k = sub_period)
  
  names(curr_xts) <- c("Open", "High", "Low", "Close", "Volume")

  if(period_in == "hour") {
    
    # fill the missing days with NAs
    index <- seq(round(start(curr_xts), "hour")-1, 
                 round(end(curr_xts), "hour")-1, by = "hour")
    
    curr_xts_nas <- merge(curr_xts, zoo(, index))
    
    # fill the NAs with the previous non-NA value
    curr_xts_nas_last <- na.locf(curr_xts_nas)
    
    curr_xts_nas_last_59 <- lf_hourly_last[(.indexmin(curr_xts_nas_last)==59&
                                              .indexsec(lf_hourly_last)==59),]
    
  } else if(period_in =="day") {
    
    # fill the missing days with NAs
    index <- seq(start(curr_xts), end(curr_xts), by = "day")
    return(index)
    curr_xts_nas <- merge(curr_xts, zoo(, index))
    
    # fill the NAs with the previous non-NA value
    curr_xts_nas_last <- na.locf(curr_xts_nas)
    
  }
    
}