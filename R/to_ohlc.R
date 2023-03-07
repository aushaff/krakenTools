#'@title to ohlc
#'@description convert the long_df from to_long to ohlc
#'@export
to_ohlc <- function(long_df, 
                    dp_dir_in,
                    periods = c("1 seconds",
                                "30 seconds",
                                "1 minutes", 
                                "30 minutes",
                                "1 hours",
                                "4 hours",
                                "12 hours",
                                "24 hours")) {
  
  header <- c("price", "volume", "unix_time", 
              "buy_sell", "mark_lim", "misc")
  
  colnames(long_df) <- header
 
  long_df$dt <- as.POSIXct(long_df$unix_time, 
                           origin="1970-01-01",
                           tz = "UTC")
  
  long_xts <- xts::xts(long_df[, c("price", "volume")],
                       order.by = long_df[, "dt"])
  
  sapply(periods, function(curr_period) {
    
    split <- strsplit(curr_period, " ")[[1]]
    period <- split[2]
    k <- split[1]
    
    ohlc <- xts::to.period(long_xts, 
                           period = period,
                           k = k,
                           drop.time = TRUE, 
                           OHLC = TRUE)

    colnames(ohlc) <- c("open", "high", "low", "close", "volume")
    
    saveRDS(ohlc, file.path(dp_dir_in, paste0(period,"_",k, "_ohlc.rds")))
    
  })
  
  
  return(1)
  
}