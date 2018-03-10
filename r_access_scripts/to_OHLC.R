# to convert the tick data to long OHLC data.tables  # load in file and check for (and return existence of) duplicates
to_OHLC <- function(in_curr, folder_root, period) {
  

  
  
  return(long_file)
}

in_curr <- c("XBTEUR")
folder_root <- "/media/deckard/External/data/kraken"
#folder_root <- getwd()
dp_dir <- file.path(folder_root, in_curr, "data_products")  
long_filename <- paste0(in_curr, "_long_all_tick_data.csv")

system.time(long_file <- data.table::fread(file.path(dp_dir, long_filename),
                                           sep = ";"))
# Read 13489000 rows and 8 (of 8) columns from 1.036 GB file in 00:01:04
#   user  system elapsed 
# 59.930   3.770  83.678 


# long_sub <- data.table::fread(file.path("data", "long_sub.csv"),
#                               stringsAsFactors = FALSE,
#                               dec = ".")
# long_sub <- long_file[1:6,]
# 
# myxts <- xts(long_sub[, c("price", "volume")],
#             order.by = anytime::anytime(long_sub$unix_time))
# 
# plot(myxts$price)

system.time(long_myxts <- xts(long_file[, c("price", "volume")],
             order.by = anytime::anytime(long_file$unix_time)))
# user  system elapsed 
# 1.921   4.085  77.792 

#saveRDS(long_sub, file.path(dp_dir, "delete_long_sub.rds"))
system.time(saveRDS(long_myxts, file.path(dp_dir, "XBTEUR_long_xts.rds")))
# user  system elapsed 
# 35.529   0.704  48.029 

# to
# xts_sub <- long_myxts[1:100,]

# to OHLC: 1, 5, 15, 30 min; 1, 4 and 24 hours
# one_min <- to.minutes(xts_sub, k = 1)
# min <- to.minutes(long_myxts[1000:1010,], k = 5)

# to 1 minute
one_min <-  to.minutes(long_myxts, k = 1)
system.time(saveRDS(one_min, file.path(dp_dir, "XBTEUR_OHLC_one_min_xts.rds")))

# to 5 minute
five_min <-  to.minutes5(long_myxts)
system.time(saveRDS(five_min, file.path(dp_dir, "XBTEUR_OHLC_five_min_xts.rds")))

# to 15 minute
fifteen_min <-  to.minutes15(long_myxts)
system.time(saveRDS(fifteen_min, file.path(dp_dir, "XBTEUR_OHLC_15_min_xts.rds")))

# to 30 minute
thirty_min <-  to.minutes30(long_myxts)
system.time(saveRDS(thirty_min, file.path(dp_dir, "XBTEUR_OHLC_30_min_xts.rds")))

# to 1 hour
one_hour <-  to.hourly(long_myxts)
system.time(saveRDS(one_hour, file.path(dp_dir, "XBTEUR_OHLC_one_hour_xts.rds")))

# to 4 hour
four_hour <-  to.period(long_myxts, period = "hours", k = 4)
system.time(saveRDS(four_hour, file.path(dp_dir, "XBTEUR_OHLC_four_hours_xts.rds")))

# to 24 hour
one_day <-  to.daily(long_myxts)
system.time(saveRDS(one_day, file.path(dp_dir, "XBTEUR_OHLC_one_day_xts.rds")))

plot(one_min)
plot()
plot(one_day)

library(xts)
# from 4 hour
system.time(four_hour <- readRDS(file.path(dp_dir, 
                                           "XBTEUR_OHLC_four_hour_xts.rds")))
names(four_hour) <- c("Open", "High", "Low", "Close", "Volume")
library(quantmod)
chartSeries(four_hour)
candleChart(four_hour, multi.col=TRUE, theme='white')
