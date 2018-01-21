#==============================================================================
write_quote_df <- function(curr_dat,
                           file_in,
                           pair_in,
                           folder_in) {

  start_date <- gsub(" ", "_", as.character(curr_dat$time[1]))
  cat("start_date is", start_date, "\n")
  end_date <- gsub(" ", "_", as.character(curr_dat$time[nrow(curr_dat)]))
  cat("end_date is", end_date, "\n")
  temp_file <- file.path(folder_in,
                         paste0(end_date, "__", start_date, "_", pair_in, ".csv"))

  # if(eof) {
  #
  #   print("End of data")
  #
  #   # add the last of the data to the temp file
  #   write.table(curr_dat, file = temp_file,
  #               row.names = FALSE,
  #               col.names = FALSE,
  #               sep = ",",
  #               append = TRUE)
  #
  #   # read both files
  #
  #   # merge them (excluding duplicates) on unix_time
  #   temp_in <- read.table(temp_file, sep = ",")
  #
  #   colnames(temp_in) <- c("price", "vol", "unix_time",
  #                          "buy_sell", "mark_lim", "misc", "time")
  #
  #   data_in <- read.table(file_in, sep = ",",
  #                         header = TRUE)
  #
  #   non_dups <- dplyr::setdiff(temp_in, data_in)
  #
  #   new_data <- rbind(data_in, non_dups)
  #
  #   write.table(new_data, file = file_in,
  #               row.names = FALSE,
  #               col.names = TRUE,
  #               sep = ",",
  #               append = FALSE)
  #
  #   unlink(temp_file)
  #
  #
  #
  # } else {
  tryCatch({
    
    write.table(curr_dat, file = temp_file,
                row.names = FALSE,
                col.names = TRUE,
                sep = ",",
                append = FALSE)

  }, warning = function(warn) {
    
    print(paste0("Warning ", warn, " received saving file"))
    stop()
    
  }, error = function(err) {
    
    print(paste0("Error ", err, " received saving file"))
    stop()
    
  })
     #}

}
