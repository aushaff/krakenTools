#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#'
#==============================================================================
# pair in e.g.: ETHEUR
# file_in e.g.: path/file.csv
get_historical_trades <- function(pair_in, # pair to be read
                                  folder_in, # data folder
                                  curr_since # time to collect data
                                                      # from; 0 = now
                                  ) {

  #========================================================
  # Setup
  more_data <- TRUE # more data to be downloaded
  retries <- 0 # outside of the error loop so retries is 0
  max_retries <- 10
  earliest <- 0

  #============================================================================
  # Data downloading

  # continue downloading for as long as there is data
  while(more_data) {

    # get the data from current since
    tryCatch({

      print("===================================================")
      print(paste0("Current asset is: ", pair_in))
      print(paste0("Current since is: ", curr_since))
      curr_trades <- krakenR::get_recent_trades(pair_in,
                                                  curr_since)
      print("curr_trades returned")
      #print(curr_trades)
      #============================================
      # extract the data and provide column headers
      curr_dat <- data.frame(curr_trades[[1]])
      header <- c("price", "volume", "unix_time", "buy_sell", "mark_lim", "misc")
      colnames(curr_dat) <- header

      #============================================
      # convert the time to CET and then add it to the df
      #curr_dat$time <- anytime(as.numeric(as.character(curr_dat$unix_time)))

      #============================================
      # update the since
      #curr_since <- curr_trades$last
      #cat("Since updated to: ", curr_since, "\n")

      #============================================
      print("Writing file")
      tryCatch({
        write_quote_df(curr_dat,
                       file_in,
                       pair_in,
                       folder_in, 
                       last_since = curr_since)
        print("File written OK")
      }, warning = function(warn) {
        
        print(paste0("Warning ", warn, " received saving file"))
        stop()
        
      }, error = function(err) {
        
        print(paste0("Error ", err, " received saving file"))
        stop()
        
      })
      # reset the error retries
      retries <- 0
      
      # get the earlist date from the current data
      earliest <- max(curr_dat$time)
      #print(earliest)
      
      }, warning = function(warn) {
      print(paste0("Warning ", warn, " received"))


      }, error = function(err) {
        print(paste0("Error ", err, " received and caught (tryCatch)"))
        
        
        retries <- retries + 1
        cat("Number of retries is: ", retries, "\n")
        
        Sys.sleep(10)
        
        if(retries == max_retries) {
          more_data <- FALSE
        }

      })
    

    # check the time and stop if less than 30 mins old
    time_stop <- Sys.time() - (60*30)
    cat("time_stop is: ", time_stop, "\n")
    cat("earliest is: ", earliest, "\n")
    print(time_stop >= earliest)
    
    more_data <- ifelse(time_stop >= earliest,
                       TRUE,
                       FALSE)
  }
}

# get the since from the most recent file or set to 0
get_since <- function(folder_path) {
  
  # check for files in folder
  folder_files <- list.files(folder_path, 
                             pattern = ".csv" )
  #print(folder_files)
  
  if(length(folder_files) == 0) {
    
    return(curr_since <- 0)
    
  } else {
    
    # get the most recent date
    max_date <- max(folder_files)
    
    new_since <- strsplit(max_date, "_")[[1]][1]
    cat(" New since is: ", new_since, "\n")
    
    return(new_since)
    
  }
}

# take in the asset name, get the since and then
# download the data
process_asset <- function(asset_in,
                          folder_root) {
  
  print("=====================================")
  cat("Processing asset: ", asset_in, "\n")
  
  # set the data folder
  folder_path <- file.path(folder_root, 
                           asset_in, 
                           "raw_data")
  
  # get the since
  curr_since <- get_since(folder_path)
  cat("Since is: ", curr_since, "\n")

 
  # download the data
  get_historical_trades(asset_in,
                        folder_path,
                        curr_since)
  
}

