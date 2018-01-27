#'@title script to process the tick data
#'@description processs data downloaded using the krakenR package to a 
#' long dataframe (for each asset)
#'
detach("package:krakenR", unload=TRUE)
library(krakenR)
detach("package:krakenTools", unload=TRUE)
library(krakenTools)

root_folder <-  "/media/deckard/External/data/kraken"
curr_asset <- "XLMXBT"

# Aim of this script will be to process the downloaded data into a long data
# frame and carry out consistency checks e.g. 
#      find missing data
#      fill in data gaps
#      remove duplicates 
#      generally provide the data in a form that is easy to work with

# want to keep this purely as the data that comes from Kraken (plus the 
# datetime field added at download) any cleaning or processing can be done
# in a later script

# list the files in the current directory
curr_dir <- file.path(root_folder, curr_asset)
curr_files <- list.files(curr_dir)
tst <- read.csv(file.path(curr_dir, curr_files[1]))
long_df <- do.call(rbind, lapply(curr_files, 
                                 function(x) {
                                      read.csv(file.path(curr_dir, x), 
                                               header = TRUE)             
}))

dups <- duplicated(long_df)
summary(dups)

head(long_df)
tail(long_df)
nrow(long_df)
# 1019427