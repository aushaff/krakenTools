#'@title gets the server time.
#'@description aid to approximating the skew time between server and client.
#'returns unix timestamp and time in RFC1123 format
get_server_time <- function() {

  serv_time_results <- jsonlite::fromJSON("https://api.kraken.com/0/public/Time")

  # if an error message is returned handle it
  # and return all the data
  err <- serv_time_results$error
  if(length(err)>0) {
    cat("Error received retrieving data")
    return(serv_time_results)

  # otherwise return the two time formats
  } else {

    serv_time <- serv_time_results$result
  }

  return(serv_time)
}
