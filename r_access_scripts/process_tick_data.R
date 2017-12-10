#'@title script to process the tick data
#'@description processs data downloaded using the krakenR package
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)

# to do:
# whaterver processing is done needs the capability to add new data, potentially
# just a check for new data function
# check for gaps, duplicates, what other issues?
# process to long dataframe and variety of OHLC. Save as RDS.
# this script only for data processing ... set of functions for krakenTools

pair_in <- pair <- "XLMXBT"
folder_in <-  "/home/deckard/Desktop/kraken_data/XLMXBT"

