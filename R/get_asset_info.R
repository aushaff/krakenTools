#'@title get information on tradable assets from kraken.com
#'@description currently returns default information as dataframe
# https://api.kraken.com/0/public/Assets?info='currency'

get_asset_info <- function() {

  base_url <- "https://api.kraken.com/0/public/Assets"
  asset_results <- jsonlite::fromJSON(base_url)

  # if an error message is returned handle it
  # and return all the data
  err <- asset_results$error
  if(length(err)>0) {
    cat("Error received retrieving data")
    return(asset_results)

    # otherwise return the two time formats
  } else {

    asset_out <- do.call(rbind.data.frame, asset_results$result)
  }

  return(asset_out)
}
