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
library(krakenTools)

# to do:
# now needs to be modified to get time from last file date
# this will need to adjust the unix time and check for duplicate rows:
# for new since remove decimal point and add five zeroes to the end

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

  #============================================================================
  # Data downloading

  # continue downloading for as long as there is data
  while(more_data) {

    # get the data from current since
    tryCatch({

      print("===================================================")
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
      curr_dat$time <- anytime(as.numeric(as.character(curr_dat$unix_time)))
      print(max(curr_dat$time))
      #============================================
      # update the since
      curr_since <- curr_trades$last
      #cat("Since updated to: ", curr_since, "\n")

      #============================================
      #print("Writing file")
      write_quote_df(curr_dat,
                     file_in,
                     pair_in,
                     folder_in)

      # reset the error retries
      retries <- 0
      }, warning = function(warn) {
      print(paste0("Warning ", warn, " received"))


      }, error = function(err) {
        print(paste0("Error ", err, " received and caught (tryCatch)"))
        print(curr_trades$err)
        print(curr_trades)
        retries <- retries + 1
        Sys.sleep(10)

        if(retries == max_retries) {
          more_data <- FALSE
        }

      })
    }
}

# currently only one at a time
pair_in <- pair <- "XLMXBT"
folder_in <-  "/home/deckard/Desktop/kraken_data/XLMXBT"

# currently this just gets all the data from the beginning
get_historical_trades(pair, folder_in, 0)

1468968981.559
1469298336.7172

curr_since <- 1469298336717245137
curr_trades <- krakenR::get_recent_trades(pair_in,
                                          curr_since)

full_since <- curr_trades[[1]]
nrow(full_since)
head(full_since)
# [,1]          [,2]            [,3]              [,4] [,5] [,6]
# [1,] "0.000003500" "400.00000000"  "1469298342.3313" "b"  "m"  ""
# [2,] "0.000003500" "400.00000000"  "1469298351.7677" "b"  "m"  ""
# [3,] "0.000003500" "400.00000000"  "1469298355.3257" "b"  "m"  ""
# [4,] "0.000003500" "400.00000000"  "1469298367.1669" "b"  "m"  ""
# [5,] "0.000003500" "3711.42857142" "1469298367.8609" "b"  "m"  ""
# [6,] "0.000003500" "400.00000000"  "1469298373.7157" "b"  "m"  ""
tail(full_since)
# [995,]  "0.000003360" "2284.07000000" "1469392469.0252" "s"  "l"  ""
# [996,]  "0.000003360" "2247.45000000" "1469392478.0973" "s"  "l"  ""
# [997,]  "0.000003360" "1612.47000000" "1469392489.693"  "s"  "l"  ""
# [998,]  "0.000003360" "2287.35000000" "1469392502.0007" "s"  "l"  ""
# [999,]  "0.000003360" "1554.84000000" "1469392516.1667" "s"  "l"  ""
# [1000,] "0.000003360" "3010.15000000" "1469392525.472"  "s"  "l"  ""



#curr_since <- 1469298336717245137
curr_since <- 1469298336717200000
# for new since remove decimal point and add five zeroes to the end
curr_trades <- krakenR::get_recent_trades(pair_in,
                                          curr_since)

dec_since <- curr_trades[[1]]
nrow(dec_since)
head(dec_since)
tail(dec_since)
