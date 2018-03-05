# to convert the tick data to long OHLC data.tables  # load in file and check for (and return existence of) duplicates
to_OHLC <- function(in_curr, folder_root, period) {
  

  
  
  return(long_file)
}

in_curr <- c("XBTEUR")
folder_root <- "/media/deckard/External/data/kraken"
dp_dir <- file.path(folder_root, in_curr, "data_products")  
long_filename <- paste0(in_curr, "_long_all_tick_data.csv")

system.time(long_file <- data.table::fread(file.path(dp_dir, long_filename),
                                           sep = ";"))
# Read 13489000 rows and 8 (of 8) columns from 1.036 GB file in 00:01:04
#   user  system elapsed 
# 59.930   3.770  83.678 

long_sub <- as.matrix(long_file[1:10,])

system.time(long_sub$t <- as.POSIXct(long_sub$time, "UTC", "%Y-%m-%d %H:%M:%S"))
# user  system elapsed 
# 0.005   0.000   0.048 
long_sub

library(xts)
help(xts)
myxts <- as.xts(long_sub[, c("price", "volume")], order.by = time)

long <- to_OHLC("XBTEUR", folder_root)
cp_long <- long
cp_long$time <- as.POSIXct(cp_long$time, "UTC", "%Y%m%dT%H%M%S")








