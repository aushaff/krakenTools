library(krakenR)
assets <- get_asset_info()

assets <- get_tradable_asset_pair()

length(assets)
# 58

ass_l <- c()
folder_root <- "/media/External/data/kraken"

for(i in 1:length(assets)) {

  curr_asset <- assets[[i]]$altname
  ass_l <- c(ass_l, curr_asset)
  dir.create(file.path(folder_root, curr_asset))

}

# [1] "BCHEUR"   "BCHUSD"   "BCHXBT"   "DASHEUR"  "DASHUSD"  "DASHXBT"
# [7] "EOSETH"   "EOSXBT"   "GNOETH"   "GNOXBT"   "USDTUSD"  "ETCETH"
# [13] "ETCXBT"   "ETCEUR"   "ETCUSD"   "ETHXBT"   "ETHXBT.d" "ETHCAD"
# [19] "ETHCAD.d" "ETHEUR"   "ETHEUR.d" "ETHGBP"   "ETHGBP.d" "ETHJPY"
# [25] "ETHJPY.d" "ETHUSD"   "ETHUSD.d" "ICNETH"   "ICNXBT"   "LTCXBT"
# [31] "LTCEUR"   "LTCUSD"   "MLNETH"   "MLNXBT"   "REPETH"   "REPXBT"
# [37] "REPEUR"   "XBTCAD"   "XBTCAD.d" "XBTEUR"   "XBTEUR.d" "XBTGBP"
# [43] "XBTGBP.d" "XBTJPY"   "XBTJPY.d" "XBTUSD"   "XBTUSD.d" "XDGXBT"
# [49] "XLMXBT"   "XMRXBT"   "XMREUR"   "XMRUSD"   "XRPXBT"   "XRPEUR"
# [55] "XRPUSD"   "ZECXBT"   "ZECEUR"   "ZECUSD"
