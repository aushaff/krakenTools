#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)
library(plyr)

# to do. 
# write directly to file?
# pause and retry if there are errors
# check to see if the data exists before saving to file? 

# currently only one at a time
pair <- "ETHEUR"

get_all_historical_trades <- function(pair_in) {
  
  since <- 0
  more_data <- TRUE
  
  out_dat <- data.frame(price = numeric(), 
                        volume = numeric(),
                        time = numeric(),
                        buy_sell = character(), 
                        market_limit = character(), 
                        misc = character())
  
  while(more_data) {
  
    curr_trades <- get_recent_trades(pair_in,
                                     since)
      
    err <- curr_trades$error
    if(length(err)>0) {
      cat("Error received retrieving data")
      retries <- 0
      max_retries <- 10
      sleep_time <- 10
      
      for(i in max_retries) {
        Sys.sleep(sleep_time)
        
        curr_trades <- get_recent_trades(pair_in,
                                         since)
        err <- curr_trades$error
        if(length(err)==0) {
          break
        }
      }
      
      more_data <- FALSE
      print(err)
      return(out_dat)
    } else {
      
      curr_dat <- curr_trades[[1]]
      
      out_dat <- rbind(out_dat, curr_dat)
      since <- curr_trades$last
    } # end if
  } # end while
  return(out_dat)
}


# get trades from time 0
eth_eur <- get_all_historical_trades(pair)
