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
# check to see if the data exists before saving to file?
# handle end of file

handle_dl_err <- function(pair_in, 
                          since,
                          curr_retry = 0,
                          max_retries = 10, # maximum number of re-down attempts
                          sleep_time = 30) { # pause inbetween retries
  
  cat("Error received retrieving data: ", "\n")
  print(err)
  
  # code here to handle end of data
  # if error message equals end of data error ...
  
  # else ... pause and retry
    
  # while current retry is less than max_retries
  while(curr_retry < max_retries+1) {
    
    # pause for sleep_time
    Sys.sleep(sleep_time)
    cat("Retrying. ===============", "\n", "Retry #", retries, "\n")
    
    curr_trades <- get_recent_trades(pair_in,
                                     since)
    err <- curr_trades$error
    
    if(length(err)>0) {
     
      # call itself 
    } else {
      return(curr_trades)
    }
    
  } # end while is error and < max_retries

# if gets   
}
  



  
  
  # if there is an error
  
    
    # if max_retries return the df and the error
    if(max_retries==retries) {
      
      print("Maximum retries reached. Error persists")
      print("err")
      return(out_dat)
    }

# pair in as: ETHEUR
# file_in as: path/file.csv
get_all_historical_trades <- function(pair_in, file_in = NA) {

  # Setup
  curr_since <- 0 # time to collect data from (0 is now)
  more_data <- TRUE # more data to be downloaded
  is_err <- FALSE # error flag

  #============================================================================
  # check to see if input file exists and if not create it

  if(!is.na(file_in)) {

    # if file exists ...
    # if(file.exists(file_in)) {
    # 
    #   cat("File exists. Will be written to", "\n")
    # 
    #   # check to see if file can be read
    # 
    #   # get first date of file and save as first_date
    #   first_date <- NA
    #   # get last date of file and save as last_date
    #   last_date <- NA
    # 
    # } else { # if file doesnt exist ...

      print(paste0("File input: ", "/n", file_in, " doesn't exist. Creating ..."))

      # call function to create file
      tryCatch({
        create_kraken_tick_file(file_in)
      }, warning = function(war) {
        print(paste0("Warning recieved creating tick file: ", war))
      }, error = function (err){

        stop(paste0("File ", file_in, " cannot be created. ", err))
      })
    #} # end if(file.exists)
  } else {
    cat("No output file and path specified. Function call requires:
        get_all_historical_trades(pair, path_and_file)", "\n")
    return(0)
  }# end if(!is.na(file_in))

  #============================================================================
  # dataframe for output
  # out_dat <- data.frame(price = numeric(),
  #                       volume = numeric(),
  #                       time = numeric(),
  #                       buy_sell = character(),
  #                       market_limit = character(),
  #                       misc = character())

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
    # Error checking
    
    # get the error list from returned data
    err <- curr_trades$error

    # if there is an error
    if(length(err)>0) {
    
        err_status <- handle_dl_err(pair_in, 
                                    since,
                                    curr_retry = 0)
    }
      
    #======================================================
    # if no error get the data 
    curr_dat <- curr_trades[[1]]
    
    # add the time in CET
    curr_dat$cet <- anytime(unlist(curr_dat$unix_time))
    # out_dat <- rbind(out_dat, curr_dat)

    # update the sinces
    old_since <- curr_since
    curr_since <- curr_trades$last

    cat("Old since: ", old_since, "; New since: ", curr_since, "\n")

    # if(old_since != curr_since && !is.na(curr_dat)) {

      cat("Appending to file", "\n")
      # append to the end of the csv file
      write.table(curr_dat, file = file_in,
                row.names = FALSE,
                col.names = FALSE,
                sep = ",",
                append = TRUE)
    # } else {
    #   cat("Two sinces are the same", "\n")
    # }

    curr_dat <- NA # just to ensure same data isn't written twice

  } # end of while(more_data)
} # end of function



# currently only one at a time
pair <- "ETHEUR"
#eth_eur_file <- "/media/External/data/kraken/etheur.csv"
eth_eur_file <- "/home/austin/partage/austin.haffenden/Private/personal/kraken_data/etheur.csv"

# get trades - check to see if file exists and update
get_all_historical_trades(pair, eth_eur_file)
#get_all_historical_trades(pair)
