#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)
library(plyr)
library(anytime)

# to do. 
# write directly to file?
# pause and retry if there are errors
# check to see if the data exists before saving to file? 
# handle end of file

get_all_historical_trades <- function(pair_in) {
  
  #========================================================
  # Setup 
  
  since <- 0 # time to collect data from (0 is now)
  more_data <- TRUE # more data to be downloaded
  is_err <- FALSE # error flag
  max_retries <- 10 # maximum number of re-down attempts
  sleep_time <- 30 # pause inbetween retries
  
  # dataframe for output
  out_dat <- data.frame(price = numeric(), 
                        volume = numeric(),
                        time = numeric(),
                        buy_sell = character(), 
                        market_limit = character(), 
                        misc = character())

  #========================================================
  # Data downloading
  
  # continue downloading for as long as there is data
  # how does this fail?
  while(more_data) {
    
    # get the data from current since
    curr_trades <- get_recent_trades(pair_in,
                                     since)
    retries <- 0 # outside of the error loop so retries is 0
    
    #======================================================
    # Error checking - eventually this should handle end of data
    
    # get the error list from returned data
    err <- curr_trades$error
    
    # if there is an error
    if(length(err)>0) {
      
      # code here to handle end of data
      # if error message ...
      # more_data <- FALSE
      
      
      cat("Error received retrieving data")
      print(err)
      is_err <- TRUE
      
      # while error flag is set retry for max_retries
      while(is_err) {
        
        cat("Retrying. Retry #", retries, "\n")
        
        # pause for sleep_time
        Sys.sleep(sleep_time)
        
        curr_trades <- get_recent_trades(pair_in,
                                         since)
        err <- curr_trades$error
        
        # if there is an error 
        if(length(err)>0) {
          
          # if max_retries return the df and the error
          if(max_retries==retries) {
            
            print("Maximum retries reached. Error persists")
            print("err")
            return(out_dat)
          }
          
        } else {
          # if there isn't an error
          is_err <- FALSE
        }
        
        # increment number of retries
        retries <- retries + 1
      
      } # end while(is_err)
      
    } # end if error
    
    #======================================================
    # if no error can add data to df and update since
    curr_dat <- curr_trades[[1]]
    out_dat <- rbind(out_dat, curr_dat)
    since <- curr_trades$last
    
    
    cat("Current since: ", since, "\n")
 
  } # end of while(more_data)
} # end of function
  
  
    
# currently only one at a time
pair <- "ETHEUR"

# get trades from time 0
eth_eur <- get_all_historical_trades(pair)
