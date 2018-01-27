#' #'@title function to download kraken tick data for a specified day
#' #'@description uses the krakenR package to download all historical tick
#' #'data for the specified pair for a given date (midnight to midnight)
#' #'@param asset e.g.: ETHEUR
#' #'@param date as character in the form: 31-01-2017
#' get_day_asset_tick <- function(asset_in, date_in) {
#'   
#'   options(digits.secs=9)
#'   max_retries <- 10
#'   retries <- 0 
#'   date_start_str <- paste0(date_in, "T00:00:00:00.000001")
#'   date_start <- as.numeric(strptime(date_start_str, format = "%d-%m-%YT%H:%M:%S:%OS"))
#'   
#'   date_end_str <- paste0(date_in, "T23:59:59:00.999999")
#'   #as.numeric(strptime(date_end_str, format = "%d-%m-%YT%H:%M:%S:%OS"))
#'   date_end <- as.numeric(strptime(date_end_str, format = "%d-%m-%YT%H:%M:%S:%OS"))
#'   
#'   #mySeet <- 1000*(curr_date$hour*3600 + curr_date$min*60 + curr_date$sec)
#'   #curr_date_num <- as.numeric(curr_date)
#'   
#'   #curr_since <- format(1512946800, nsmall = 4)
#'   #ew_since <- gsub("\\.", "", curr_since)
#'   curr_since <- stri_pad_right(date_start, 
#'                               19,
#'                               0)
#'   
#'   more_data <- TRUE
#'   
#'   while(more_data) {
#'     
#'     # get the data from current since
#'     tryCatch({
#'       
#'       curr_trades <- krakenR::get_recent_trades(asset_in, curr_since)
#'       
#'       curr_dat <- data.frame(curr_trades[[1]])
#'       header <- c("price", "volume", "unix_time", "buy_sell", "mark_lim", "misc")
#'       colnames(curr_dat) <- header
#'       
#'       #============================================
#'       # convert the time to CET and then add it to the df
#'       curr_dat$time <- anytime(as.numeric(as.character(curr_dat$unix_time)))
#'       
#'       #============================================
#'       # update the since
#'       curr_since <- curr_trades$last
#'       
#'       if(curr_since > date_end) {
#'         more_data <- FALSE
#'       }
#'       
#'     }, warning = function(warn) {
#' 
#'     }, error = function(err) {
#' 
#'       print(err)
#'       retries <- retries + 1
#'       
#'       Sys.sleep(10)
#'       
#'       if(retries == max_retries) {
#'         cat("Maximum retries reached: ABORTED", "\n")
#'         more_data <- FALSE
#'       }
#'       
#'     }) # end tryCatch
#'      
#'   } # end while
#'   
#' } # end function
#' 
#' library(krakenR)
#' library(anytime)
#' date_in <- "11-12-2017"
#' asset_in <- "XBTEUR"
#' 
#' as.Date(date, format = "%d-%m-%Y")
#' 
#' num_tn <- as.numeric(as.POSIXlt(Sys.time()))
#'  
#' num_tn
#' since_m <- num_tn$hour*3600 + num_tn$min*60 + num_tn$sec
#' since_m
#' as.POSIXlt(1513035169.2036, origin = '1970-01-01')
#' 
#' 
#' # to do:
#' # now needs to be modified to get time from last file date
#' # this will need to adjust the unix time and check for duplicate rows:
#' # for new since remove decimal point and add five zeroes to the end
#' #now have all the data need to modify to et most recent
#' # batch run on pi?
#' 
#' #==============================================================================
#' # pair in e.g.
#' # file_in e.g.: path/file.csv
#' get_historical_trades <- function(pair_in, # pair to be read
#'                                   folder_in, # data folder
#'                                   curr_since # time to collect data
#'                                                       # from; 0 = now
#'                                   ) {
#' 
#'   #========================================================
#'   # Setup
#'   more_data <- TRUE # more data to be downloaded
#'   retries <- 0 # outside of the error loop so retries is 0
#'   max_retries <- 10
#'   earliest <- 0
#' 
#'   #============================================================================
#'   # Data downloading
#' 
#'   # continue downloading for as long as there is data
#'   while(more_data) {
#' 
#'     # get the data from current since
#'     tryCatch({
#' 
#'       print("===================================================")
#'       print(paste0("Current asset is: ", pair_in))
#'       print(paste0("Current since is: ", curr_since))
#'       curr_trades <- krakenR::get_recent_trades(pair_in,
#'                                                   curr_since)
#'       print("curr_trades returned")
#'       #print(curr_trades)
#'       #============================================
#'       # extract the data and provide column headers
#'       curr_dat <- data.frame(curr_trades[[1]])
#'       header <- c("price", "volume", "unix_time", "buy_sell", "mark_lim", "misc")
#'       colnames(curr_dat) <- header
#' 
#'       #============================================
#'       # convert the time to CET and then add it to the df
#'       curr_dat$time <- anytime(as.numeric(as.character(curr_dat$unix_time)))
#' 
#'       #============================================
#'       # update the since
#'       curr_since <- curr_trades$last
#'       #cat("Since updated to: ", curr_since, "\n")
#' 
#'       #============================================
#'       print("Writing file")
#'       write_quote_df(curr_dat,
#'                      file_in,
#'                      pair_in,
#'                      folder_in)
#'       print("File written OK")
#'       # reset the error retries
#'       retries <- 0
#'       
#'       # get the earlist date from the current data
#'       earliest <- max(curr_dat$time)
#'       #print(earliest)
#'       
#'       }, warning = function(warn) {
#'       print(paste0("Warning ", warn, " received"))
#' 
#' 
#'       }, error = function(err) {
#'         print(paste0("Error ", err, " received and caught (tryCatch)"))
#'         
#'         print(curr_trades$err)
#'         print(curr_trades)
#'         
#'         retries <- retries + 1
#'         
#'         Sys.sleep(10)
#'         
#'         if(retries == max_retries) {
#'           more_data <- FALSE
#'         }
#' 
#'       })
#'     
#' 
#'     # check the time and stop if less than 30 mins old
#' 
#'     time_stop <- Sys.time() - (60*30)
#'     cat("time_stop is: ", time_stop, "\n")
#'     cat("earliest is: ", earliest, "\n")
#'     print(time_stop >= earliest)
#'     
#'     more_data <- ifelse(time_stop >= earliest,
#'                        TRUE,
#'                        FALSE)
#'   }
#' }
#' 
#' # get the since from the most recent file or set to 0
#' get_since <- function(folder_path) {
#'   
#'   # check for files in folder
#'   folder_files <- list.files(folder_path)
#'   #print(folder_files)
#'   
#'   if(length(folder_files) == 0) {
#'     
#'     return(curr_since <- 0)
#'     
#'   } else {
#'     
#'     max_date <- max(folder_files)
#'     print(max_date)
#'     csv_in <- read.csv(file.path(folder_path, max_date),
#'                        header = TRUE,
#'                        dec = ".")
#'     # kraken needs 5 zeros after the reported since
#'     # nsmall = 9 returns digits that appear correct (at first look
#'     # but this really needs checking)
#'     curr_since <- format(max(csv_in$unix_time), nsmall = 4)
#'     new_since <- gsub("\\.", "", curr_since)
#'     new_since <- stri_pad_right(new_since, 
#'                                 19,
#'                                 0)
#'     #cat(" New since is: ", new_since, "\n")
#'     
#'     return(new_since)
#'     
#'   }
#' }
#' 
#' # take in the asset name, get the since and then
#' # download the data
#' process_asset <- function(asset_in,
#'                           folder_root) {
#'   
#'   print("=====================================")
#'   cat("Processing asset: ", asset_in, "\n")
#'   
#'   # set the data folder
#'   folder_path <- file.path(folder_root, asset_in)
#'   
#'   # get the since
#'   curr_since <- get_since(folder_path)
#'   cat("Since is: ", curr_since, "\n")
#' 
#'  
#'   # download the data
#'   get_historical_trades(asset_in,
#'                         folder_path,
#'                         curr_since)
#'   
#' }
#' 
#' #===================================================
#' # Get asset information
#' assets <- get_tradable_asset_pair()
#' 
#' length(assets)
#' # 58
#' 
#' # [1] "BCHEUR"   "BCHUSD"   "BCHXBT"   "DASHEUR"  "DASHUSD"  "DASHXBT" 
#' # [7] "EOSETH"   "EOSXBT"   "GNOETH"   "GNOXBT"   "USDTUSD"  "ETCETH"  
#' # [13] "ETCXBT"   "ETCEUR"   "ETCUSD"   "ETHXBT"   "ETHXBT.d" "ETHCAD"  
#' # [19] "ETHCAD.d" "ETHEUR"   "ETHEUR.d" "ETHGBP"   "ETHGBP.d" "ETHJPY"  
#' # [25] "ETHJPY.d" "ETHUSD"   "ETHUSD.d" "ICNETH"   "ICNXBT"   "LTCXBT"  
#' # [31] "LTCEUR"   "LTCUSD"   "MLNETH"   "MLNXBT"   "REPETH"   "REPXBT"  
#' # [37] "REPEUR"   "XBTCAD"   "XBTCAD.d" "XBTEUR"   "XBTEUR.d" "XBTGBP"  
#' # [43] "XBTGBP.d" "XBTJPY"   "XBTJPY.d" "XBTUSD"   "XBTUSD.d" "XDGXBT"  
#' # [49] "XLMXBT"   "XMRXBT"   "XMREUR"   "XMRUSD"   "XRPXBT"   "XRPEUR"  
#' # [55] "XRPUSD"   "ZECXBT"   "ZECEUR"   "ZECUSD"  
#' 
#' folder_root <- "/media/External/data/kraken"
#' 
#' ass_l <- c()
#' 
#' for(i in 1:length(assets)) {
#'   
#'   curr_asset <- assets[[i]]$altname
#'   ass_l <- c(ass_l, curr_asset)
#'   dir.create(file.path(folder_root, curr_asset))
#'   
#' }
#' 
#' # remove those assets with '.d' at the end. I need to find out what this
#' # relates to 
#' ass_l <- grep("[.d]", ass_l, value = TRUE, invert = TRUE) 
#' ass_l <- sort(ass_l)
#' ass_l
#' 
#' # [1] "BCHEUR"  "BCHUSD"  "BCHXBT"  "DASHEUR" "DASHUSD"
#' # [6] "DASHXBT" "EOSETH"  "EOSXBT"  "ETCETH"  "ETCEUR" 
#' # [11] "ETCUSD"  "ETCXBT"  "ETHCAD"  "ETHEUR"  "ETHGBP" 
#' # [16] "ETHJPY"  "ETHUSD"  "ETHXBT"  "GNOETH"  "GNOXBT" 
#' # [21] "ICNETH"  "ICNXBT"  "LTCEUR"  "LTCUSD"  "LTCXBT" 
#' # [26] "MLNETH"  "MLNXBT"  "REPETH"  "REPEUR"  "REPXBT" 
#' # [31] "USDTUSD" "XBTCAD"  "XBTEUR"  "XBTGBP"  "XBTJPY" 
#' # [36] "XBTUSD"  "XDGXBT"  "XLMXBT"  "XMREUR"  "XMRUSD" 
#' # [41] "XMRXBT"  "XRPEUR"  "XRPUSD"  "XRPXBT"  "ZECEUR" 
#' # [46] "ZECUSD"  "ZECXBT" 
#' 
#' #==============================================================================
#' # Process the assets
#' # process_asset(asset_in, folder_root)
#' 
#' sapply(ass_l, process_asset, folder_root)
#' 
#' # folder_path <- file.path(folder_root, "BCHEUR")
#' # 
#' # process_asset("BCHEUR", folder_root)
#' 
#' 
