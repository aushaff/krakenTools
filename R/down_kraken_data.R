#'@title script to download kraken tick data
#'@description uses the krakenR package to download all historical tick
#'data for the specified pair
#' pair in e.g.: ETHEUR
#' file_in e.g.: path/file.csv
#'@export
get_historical_trades <- function(pair_in, # pair to be read
                                  folder_in, # data folder
                                  curr_since # time to collect data
                                  ) {

  #========================================================
  # Setup
  more_data <- TRUE # more data to be downloaded
  retries <- 0 
  max_retries <- 3
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
      #print("curr_trades returned")
      #print(curr_trades)
      
      #============================================
      # extract the data and provide column headers
      curr_dat <- data.frame(curr_trades$result[[1]],
                             stringsAsFactors = FALSE)
      
      header <- c("price", "volume", "unix_time", 
                   "buy_sell", "mark_lim", "misc")
       
      colnames(curr_dat) <- header
      
      #===========================================
      # update the since
      curr_since <- curr_trades$result[[2]]
      cat("Since updated to: ", curr_since, "\n")
      
      #============================================
      # reset the error retries
      retries <- 0
      
      #============================================
      # add the datetime on the end of the curr_dat for the database
      #print("****")
      #print(curr_dat[, 3])
      
      #print(class(curr_dat$unix_time))
      curr_dat$unix_time <- as.numeric(curr_dat$unix_time)
      #curr_dat$unix_time <- format(curr_dat$unix_time, digits = 15)
      
      #print(class(curr_dat$unix_time))
      
      curr_dat$dt <- as.POSIXct(curr_dat$unix_time,
                                origin="1970-01-01",
                                tz = "UTC")
      #head(csv)
      curr_dat$dt <- format(curr_dat$dt, "%Y-%m-%d %H:%M:%OS4")
      #head(csv)
      
      
      #============================================
      #print("Writing file")
        tryCatch({
          krakenTools::write_quote_df(curr_dat,
                                      file_in,
                                      pair_in,
                                      folder_in, 
                                      last_since = curr_since)
          print("File written OK")
        }, warning = function(warn) {
          print(paste0("Warning received from write_quote_df: ", warn))
          more_data <- FALSE
        }, error = function(err) {
          print(paste0("Error received from write_quote_df: ", err))
          more_data <- FALSE
        })
      #============================================
      # get the earlist date from the current data
      earliest <- max(as.numeric(as.character(curr_dat$unix_time)))

      # check the time and stop if less than 1 hour old
      time_stop <- Sys.time() - (60*60)
      cat("time_stop is: ", time_stop, "\n")
      cat("earliest is: ", earliest, "\n")
      print(time_stop >= earliest)
      
      more_data <- ifelse(time_stop >= earliest,
                          TRUE,
                          FALSE)
      
    }, warning = function(warn) {
      print(warn)
   
    }, error = function(err) {
      
      print(err)
      
      #============================================
      # extract the error if there is one
      tryCatch({
        if(curr_trades$error != 0) {
          print(paste0("Error received from Kraken: ", curr_trades$error))
        }
      }, error = function(err) {
        print(err)
      })
      
 
      # increase the number of retries 
      retries <<- retries + 1
      # cat("Number of retries is: ", retries, "\n")
      # cat("Max retries is: ", max_retries, "\n")
      # cat("More data is: ", more_data, "\n")
      
      # see if we are at the maximum
      if(retries > max_retries) {
        more_data <<- FALSE
      }
      
      # if not wait to try again
      Sys.sleep(10)
    })
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
    max_date <- max(gtools::mixedsort(folder_files))
    
    new_since <- strsplit(max_date, "_")[[1]][1]
    cat(" New since is: ", new_since, "\n")
    
    return(new_since)
    
  }
}

#'@title down_tick_data
#'@description access functio to take in the asset name, get the since and then
#' download the data
#'@export
down_tick_data <- function(asset_in,
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

#'@title download multiple assets
#'@description call down_tick_data for multiple assets and create the long
#'file if flagged
#'@export
down_multi_ass <- function(ass_l, 
                           folder_root,
                           to_long = TRUE) {

  sapply(ass_l, function(curr_asset) {
    
    krakenTools::down_tick_data(curr_asset, folder_root)  
    #to_long(curr_asset, folder_root)
  })
  
  if(to_long) {
    
    sapply(ass_l, function(curr_ass) {
      
      curr_raw_dir <- file.path(folder_root, curr_ass , "raw_data")
      curr_dat_dir <- file.path(folder_root, curr_ass , "data_products")
      
      # detach(package:krakenTools, unload = TRUE)
      # library(krakenTools)
      krakenTools::to_long(curr_ass,
                           curr_raw_dir,
                           curr_dat_dir)
    })
  }
}
