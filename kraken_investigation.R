# Get information on the available asset pairs on Kraken
library(krakenR)

all_assets <- get_asset_info()

asset_pairs <- get_tradable_asset_pair()
str(asset_pairs)

asset_names <- names(asset_pairs)
# Bitcoin cash
# "BCHEUR"     "BCHUSD"     "BCHXBT"     

# Dash
# "DASHEUR"    "DASHUSD"    "DASHXBT"    

# EOS
# "EOSETH"     "EOSXBT"    

# Gnosis
# "GNOETH"     "GNOXBT"     

# Tether
# "USDTZUSD"   

# Ethereum classic
# "XETCXETH"   "XETCXXBT"   "XETCZEUR"   "XETCZUSD"   

# Ethereum
# "XETHXXBT"  "XETHXXBT.d" "XETHZCAD"   "XETHZCAD.d" "XETHZEUR"   "XETHZEUR.d" 
# "XETHZGBP"   "XETHZGBP.d" "XETHZJPY"  "XETHZJPY.d" "XETHZUSD"   "XETHZUSD.d" 

# Iconomi
# "XICNXETH"   "XICNXXBT"   

# Litecoin
# "XLTCXXBT"   "XLTCZEUR"   "XLTCZUSD"  

# Melon
# "XMLNXETH"   "XMLNXXBT"   

# Augur
# "XREPXETH"   "XREPXXBT"   "XREPZEUR"   

# Bitcoin
# "XXBTZCAD"   "XXBTZCAD.d" "XXBTZEUR"  "XXBTZEUR.d" "XXBTZGBP"   "XXBTZGBP.d" 
# "XXBTZJPY"   "XXBTZJPY.d" "XXBTZUSD"   "XXBTZUSD.d" 

# DogeCoin
# "XXDGXXBT"  

# Stellar Lumens
# "XXLMXXBT"   

# Monero
# "XXMRXXBT"  "XXMRZEUR"   "XXMRZUSD"

# Ripple
# "XXRPXXBT"  "XXRPZEUR"   "XXRPZUSD"   

# Zcash
# "XZECXXBT"  "XZECZEUR"   "XZECZUSD" 
