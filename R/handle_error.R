#==============================================================================
#  If it eventually gets data it returns it. Otherwise it stops the program
handle_error <- function(err_in,
                         pair_in,
                         curr_since) {

  # setup
  is_err <- TRUE # error flag
  max_retries <- 10 # maximum number of re-down attempts
  sleep_time <- 10 # pause inbetween retries
  retries <- 0 # current number of retries



  # while error flag is set retry for max_retries
  while(is_err & retries<max_retries) {

    cat("Error received retrieving data. Retry No. ", retries, "\n")
    print(err_in)

    # pause for sleep_time
    Sys.sleep(sleep_time)
    tryCatch({

      curr_trades <- krakenR::get_recent_trades(pair_in,
                                                curr_since)

      err_in <- curr_trades$error
    }, warning = function(warn) {
      print(paste0("Warning ", warn, " received"))
    }, error = function(err) {

      print(paste0("Error ", err, " received and caught (tryCatch)"))
      err_in <- err
    })


    # if there is an error
    if(length(err_in)>0||length(curr_trades)!=2) {

      # increment number of retries
      retries <- retries + 1

    } else {

      # if there isn't an error
      is_err <- FALSE
      # return curr_trades
      return(curr_trades)

    }
  } # end while(is_err)

  # this should only be reached if maximum retries
  cat("Maximum number of retries reached ", retries, "\n")
  print(err_in)
  stop("MAX_ERR")

} # end function
