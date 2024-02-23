#'@title download ohlc for an asset pair
#'@description OHLC data is only available for the last 720 candles of whatever
#' interval specified. This function down loads the trade data and returns it as
#' OHLC of the specified interval and period
#'@param asset_pair character of the asset pair as accepted by kraken api
#'@param min_dt the datetime to start from as a unix timestamp
#'@param max_dt the datetime to end at as a unix timestamp
#'@param interval the period in minutes for each candle
#'@return a dataframe
#'@export
down_ohlc <- function(asset_pair,
                      min_dt,
                      max_dt,
                      interval) {

  # download the tick data for the specified period (min_dt to max_dt)
  curr_tick_data <- down_tick_data <- function(pair_in = asset_pair,
                                               min_dt = min_dt,
                                               max_dt = max_dt)

  # construct the OHLC data of the specifed intervals




  return(ohlc_data)
}
