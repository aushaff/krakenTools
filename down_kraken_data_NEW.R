#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)
library(plyr)
library(anytime)
library(R.utils)

# to do.
# write directly to file?
# check to see if the data exists before saving to file?
# handle end of file
# it appears to be trying to get data with the same since multiple times
# needs restructuring to find and solve problem. Looks as though some
# part of the data collection isn't working but this should cause a
# retry if the data isn't available. Otherwise the curr_since should
# increment. The sinces should only be the same if there is an http error
# but an error message should appear ...

#============================================================================
# check to see if input file exists and if not create it
output_file_setup <- function(file_in){
  
  # if there is a file_in
  if(!is.na(file_in)) {

    # if file exists ...
    if(file.exists(file_in)) {

      cat("File exists and will be written to", "\n")

      # check file can be read and get first and last times
      tryCatch({

        # get first time
        first_row <- read.table(file = file_in,
                              nrows = 1,
                              header = TRUE,
                              sep =",")
        # get last time
        line_no <- countLines(file_in)

        last_row <- read.csv(file = file_in,
                             skip = line_no -1,
                             header = FALSE)
        if(nrow(first_row)==1&&nrow(last_row)==1) {
          
          # get first date of file and save as first_date
          first_time <- first_row[,3]
          last_time <- last_row[,3]  
        
        } else{
          
          first_time <- NA
          last_time <- NA
        }
      }, error = function(err) {

        stop("File exists but cannot be read")
      })

    } else { # if file doesnt exist ...

      cat("File input: ", "\n", file_in, " doesn't exist. Creating ...")

      # call function to create file
      tryCatch({
        create_kraken_tick_file(file_in)

        first_time <- NA
        last_time <- NA

      }, warning = function(war) {
        print(paste0("Warning recieved creating tick file: ", war))
      }, error = function (err){

        stop(paste0("File ", file_in, " cannot be created. ", err))
      })

    } # end if(file.exists)
  } else {
    cat("No output file and path specified. Function call requires:
          get_all_historical_trades(pair, path_and_file)", "\n")
  }# end if(!is.na(file_in))

  return(list(first_time, last_time))
}

#==============================================================================
#  If it eventually gets data it returns it. Otherwise it stops the program
handle_error <- function(err_in,
                         pair_in,
                         curr_since) {

  # setup
  is_err <- TRUE # error flag
  max_retries <- 10 # maximum number of re-down attempts
  sleep_time <- 30 # pause inbetween retries
  retries <- 0 # current number of retries

  cat("Error received retrieving data: ", "\n")
  print(err_in)

  # while error flag is set retry for max_retries
  while(is_err & retries<max_retries) {

    cat("Retrying. ===============", "\n", "Retry #", retries, "\n")

    # pause for sleep_time
    Sys.sleep(sleep_time)

    curr_trades <- get_recent_trades(pair_in,
                                     curr_since)
    err_in <- curr_trades$error

    # if there is an error
    if(length(err_in)>0) {

        

    } else {
      
      # if there isn't an error
      is_err <- FALSE
      # return curr_trades

    }

    # increment number of retries
    retries <- retries + 1
  }
} # end while(is_err)

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
  out_dat <- data.frame(price = numeric(),
                        volume = numeric(),
                        time = numeric(),
                        buy_sell = character(),
                        market_limit = character(),
                        misc = character())

  #============================================================================
  # Data downloading
  
  # continue downloading for as long as there is data
    while(more_data) {

      retries <- 0 # outside of the error loop so retries is 0
      
      # get the data from current since
      curr_trades <- get_recent_trades(pair_in,
                                       curr_since)
      #========================================================================
      # Error checking and handling
      
      # get the error list from returned data
      err <- curr_trades$error
      
      # if there is an error
      if(length(err)>0) {
        err_out <- handle_error(err,
                                pair_in,
                                since)
      } # end if error
      
      # here we update curr_trades if data is returned or 
      # we stop the functioning
      
      if(err_out[1]==0){
        stop(paste0("Persistent error: ", err_out[2]))
      } else {
        
        #curr_dat <- curr_trades[[1]]
      }
      
       
      curr_dat <- curr_trades[[1]]
      
      
      
  #========================================================
}

# currently only one at a time
pair_in <- pair <- "XLMXBT"
file_in <- xlmxbt_file <- "/home/deckard/Desktop/XLMXBT_tick.csv"

# get trades - check to see if file exists and update
get_historical_trades(pair, xlmxbt_file)




#     #======================================================
#     # if no error get the data 

# <<<<<<< HEAD
# =======
#     
#     # add the time in CET
#     curr_dat$cet <- anytime(unlist(curr_dat$unix_time))
# >>>>>>> 5c4930a2d235915f641f733842cb0f06bed15276
#     # out_dat <- rbind(out_dat, curr_dat)
# 
#     # update the sinces
#     old_since <- curr_since
#     curr_since <- curr_trades$last
# 
#     cat("Old since: ", old_since, "; New since: ", curr_since, "\n")
# 
# <<<<<<< HEAD
#     if(old_since != curr_since && !is.na(curr_dat)) {
# =======
#     # if(old_since != curr_since && !is.na(curr_dat)) {
# >>>>>>> 5c4930a2d235915f641f733842cb0f06bed15276
# 
#       cat("Appending to file", "\n")
#       # append to the end of the csv file
#       write.table(curr_dat, file = file_in,
#                 row.names = FALSE,
#                 col.names = FALSE,
#                 sep = ",",
#                 append = TRUE)
# <<<<<<< HEAD
#     } else {
#       cat("Two sinces are the same", "\n")
#     }
# =======
#     # } else {
#     #   cat("Two sinces are the same", "\n")
#     # }
# >>>>>>> 5c4930a2d235915f641f733842cb0f06bed15276
# 
#     curr_dat <- NA # just to ensure same data isn't written twice
# 
#   } # end of while(more_data)
# } # end of function




