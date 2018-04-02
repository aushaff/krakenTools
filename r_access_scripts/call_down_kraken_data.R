library(krakenR)
#===================================================
# Get asset information
assets <- krakenR::get_tradable_asset_pair()

length(assets)
# 68

get_asset_names <- function(asset) {

  curr_asset <- asset$altname
  return(curr_asset)
  #dir.create(file.path(folder_root, curr_asset))

}


ass_l <- unlist(lapply(assets, get_asset_names))

# remove those assets with '.d' at the end. I need to find out what this
# relates to
ass_l <- grep("[.d]", ass_l, value = TRUE, invert = TRUE)
ass_l <- sort(ass_l)
#ass_l <- rev(ass_l)

# "BCHEUR"  "BCHUSD"  "BCHXBT" "DASHEUR" "DASHUSD" "DASHXBT"  "EOSETH" 
# "EOSEUR"  "EOSUSD"  "EOSXBT"  "ETCETH"  "ETCEUR"  "ETCUSD"  "ETCXBT" 
# "ETHCAD"  "ETHEUR"  "ETHGBP"  "ETHJPY"  "ETHUSD"  "ETHXBT"  "GNOETH" 
# "GNOEUR"  "GNOUSD"  "GNOXBT"  "ICNETH"  "ICNXBT"  "LTCEUR"  "LTCUSD" 
# "LTCXBT"  "MLNETH"  "MLNXBT"  "REPETH"  "REPEUR"  "REPUSD"  "REPXBT" 
# "USDTUSD"  "XBTCAD"  "XBTEUR"  "XBTGBP"  "XBTJPY"  "XBTUSD"  "XDGXBT" 
# "XLMEUR"  "XLMUSD"  "XLMXBT"  "XMREUR"  "XMRUSD"  "XMRXBT"  "XRPCAD" 
# "XRPEUR"  "XRPJPY"  "XRPUSD"  "XRPXBT"  "ZECEUR"  "ZECJPY"  "ZECUSD" 
# "ZECXBT" 

#==============================================================================
# Process the assets
# process_asset(asset_in, folder_root)

#sapply(ass_l, process_asset, folder_root)

# folder_path <- file.path(folder_root, "BCHEUR")
# 
folder_root <- "/media/deckard/External/data/kraken"
sapply(ass_l, function(curr_asset) {
  
  process_asset(curr_asset, folder_root)  
  #to_long(curr_asset, folder_root)
})

ass_l <- c("XLMEUR", "XLMUSD", "XLMXBT")

