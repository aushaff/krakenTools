#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)
library(plyr)
library(anytime)
library(R.utils)
library(dplyr)

# to do:
# move the get data section to a function that can be passed
#   the since from the beginning or end of a file along with
#   a flag to append to start or finish. 
# needs to be some filter to check if date from start overlaps
#   and then only add the rows that are before the last date in 
#   the file.
# should this write lines directly to file? if downloading for the first
#   time it holds a lot of data in memory. 

#==============================================================================
# pair in e.g.: ETHEUR
# file_in e.g.: path/file.csv
get_historical_trades <- function(pair_in, # pair to be read
                                  file_in, # data file.csv
                                  curr_since = 0 # time to collect data
                                                      # from; 0 = now
                                  ) { 

  #========================================================
  # Setup
  more_data <- TRUE # more data to be downloaded

  #========================================================
  # check output file exists. If it doesn't create it
  file_setup_out <- output_file_setup(file_in)
  
  # get first and last times (NA if new file)
  first_time <- file_setup_out[[1]]
  last_time <- file_setup_out[[2]]

  # effectively these need to be handled seperately so the while will
  # need to be a function - maybe all from here is a function
  #========================================================
  # dataframe for output
  # out_dat <- data.frame(price = numeric(),
  #                       volume = numeric(),
  #                       time = numeric(),
  #                       buy_sell = character(),
  #                       market_limit = character(),
  #                       misc = character())

  #============================================================================
  # Data downloading
  
  # continue downloading for as long as there is data
    while(more_data) {

     #retries <- 0 # outside of the error loop so retries is 0
      
      # get the data from current since
      tryCatch({
        
        curr_trades <- krakenR::get_recent_trades(pair_in,
                                                  curr_since)  
      }, warning = function(warn) {
        print(paste0("Warning ", warn, " received"))
      }, error = function(err) {
        print(paste0("Error ", err, " received and caught (tryCatch)"))
        
        handle_error(err,
                     pair_in,
                     curr_since)
      })
      
      #========================================================================
      # Error checking and handling
      
      # get the error list from returned data
      err <- curr_trades$error
      
      # if there is an error. This will retry the data for max_retries and 
      # return curr_trades if possible; stopping if not
      if(length(err)>0) {
        
        err_out <- handle_error(err,
                                pair_in,
                                curr_since)
        
        # after return from handle_error - check return
        if(err_out[1]=="MAX_ERR") {
          
          more_data <- FALSE
          
        } else if (is.list(err_out)) {
          
          curr_trades <- err_out
          
        } else {
          print("Unhandled return from error function")
          print(class(err_out))
          print(err_out)
          more_data <- FALSE
          
        }
        
      } # end if error
      
      #========================================================================
      # process curr_trades (current tick data)
      
      # extract the data portion
      curr_dat <- data.frame(curr_trades[[1]])
      header <- c("price", "volume", "unix_time", "buy_sell", "mark_lim", "misc")
      colnames(curr_dat) <- header
      
      # convert the time to CET and then add it to the df
      curr_dat$time <- anytime(as.numeric(as.character(curr_dat$unix_time)))
      
      # add the current tick data to the dataframe
      #out_dat <- rbind(out_dat, curr_dat)

      # update the since
      curr_since <- curr_trades$last
      
      # check timestamp for save
      last_unix_time <- curr_dat[nrow(curr_dat),]$unix_time      
      
      if(as.numeric(as.character(last_unix_time)) >= first_time) {
        
        print("Writing end of file")
        more_data <- FALSE
        write_quote_df(curr_dat, 
                       file_in,
                       eof = TRUE)  
        
      } else {
        
        print("Writing temp file")
        write_quote_df(curr_dat, 
                       file_in)  
        
      }
      
      
    } # end while 
  return(curr_since)
}

# currently only one at a time
pair_in <- pair <- "XLMXBT"
file_in <- xlmxbt_file <- "/home/deckard/Desktop/XLMXBT_tick.csv"
temp_in <- xlmxbt_file <- "/home/deckard/Desktop/XLMXBT_tick_temp.csv"
# get trades - check to see if file exists and update

# currently this just gets all the data from the current time
get_historical_trades(pair, xlmxbt_file)


temp <- read.csv(temp_in, header = FALSE)
head(temp)
tail(temp)



