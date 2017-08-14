#'@title get information about tradable pairs from kraken.com
#'@description default returns all information on all pairs.
#' base_url: https://api.kraken.com/0/public/AssetPairs
#' Input:
#'   info = info to retrieve (optional):
#'   info = all info (default)
#'   leverage = leverage info
#'   fees = fees schedule
#'   margin = margin info
#'   pair = comma delimited list of asset pairs to get
#'          info on (optional. default = all)
#'
#' Raw result: array of pair names and their info:
#'    <pair_name> = pair name
#'    altname = alternate pair name
#'    aclass_base = asset class of base component
#'    base = asset id of base component
#'    aclass_quote = asset class of quote component
#'    quote = asset id of quote component
#'    lot = volume lot size
#'    pair_decimals = scaling decimal places for pair
#'    lot_decimals = scaling decimal places for volume
#'    lot_multiplier = amount to multiply lot volume by to get currency volume
#'    leverage_buy = array of leverage amounts available when buying
#'    leverage_sell = array of leverage amounts available when selling
#'    fees = fee schedule array in [volume, percent fee] tuples
#'    fees_maker = maker fee schedule array in [volume, percent fee] tuples
#'                 (if on maker/taker)
#'    fee_volume_currency = volume discount currency
#'    margin_call = margin call level
#'    margin_stop = stop-out/liquidation margin level
#'
#' Note: If an asset pair is on a maker/taker fee schedule, the taker side is
#' given in "fees" and maker side in "fees_maker". For pairs not on
#' maker/taker, they will only be given in "fees".


get_tradable_asset_pair <- function() {

  base_url <- "https://api.kraken.com/0/public/AssetPairs"
  tap_results <- jsonlite::fromJSON(base_url)

  # if an error message is returned handle it
  # and return all the data
  err <- tap_results$error
  if(length(err)>0) {
    cat("Error received retrieving data")
    return(tap_results)

    # otherwise return the two time formats
  } else {

    tap_out <- tap_results$result
  }

  return(tap_out)
}


