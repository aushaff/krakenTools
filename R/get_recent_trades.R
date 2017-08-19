#'@title gets recent trades from a 'since' date
#'@description raw data return in form of:
#'pair name plus:
#' <price>, <volume>, <time>, <buy/sell>, <market/limit>, <miscellaneous>
#' and 'last'. the id to be used as since when polling for new trade data
#'@param base_url https://api.kraken.com/0/public/Trades
# needs to catch http error 522
get_recent_trades <- function(pair = "XBTEUR",
                              since = 0) {

  base_url <- "https://api.kraken.com/0/public/Trades"
  url <- paste0(base_url, "?", "pair=", pair)
  trades_ret <- c() # empty structure for return (data or error)

  # catch errors when calling the url and return them in the
  # same format as usual output
  tryCatch({

    trades_ret <- jsonlite::fromJSON(url)

  }, error = function(err) {
    print(paste0("Error caught: ", err))
    trades_ret$error <- list(err)
    return(trades_ret)
  })

  # if an error message is returned handle it
  # and return all the data
  err <- trades_ret$error
  if(length(err)>0) {
    cat("Error received retrieving data")
    return(trades_ret)

    # otherwise return the data and the last time
  } else {

    trades_out <- trades_ret$result
  }

  return(trades_out)
}
