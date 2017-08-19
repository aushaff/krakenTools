#'@title creates the text file with header for outuput of tick data
#'@description header in form of:
#' <price>, <volume>, <time>, <buy/sell>, <market/limit>, <miscellaneous>
#'@param
create_kraken_tick_file <- function(file_loc) {

  out_df <- data.frame(price = numeric(),
                       vol = numeric(),
                       time = numeric(),
                       buy_sell = character(),
                       mark_lim = character(),
                       misc = character())

  #header <- c("price", "volume", "time", "buy_sell", "mark_lim", "misc")
  cat("in create_kraken_tick_file", "\n")
  write.csv(out_df, file = file_loc,
            row.names = FALSE)
  return(1)
}
