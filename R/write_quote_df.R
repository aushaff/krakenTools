#'@title write_quote_df
#'@description write the downloaded tick data to file 
#'@export
write_quote_df <- function(curr_dat,
                           file_in,
                           pair_in,
                           folder_in,
                           last_since) {

  # start_date <- gsub(" ", "_", as.character(curr_dat$time[1]))
  # cat("start_date is", start_date, "\n")
  # end_date <- gsub(" ", "_", as.character(curr_dat$time[nrow(curr_dat)]))
  # cat("end_date is", end_date, "\n")
  
  # ensure that the save location exists 
  out_folder <- file.path(folder_in)
  if(!dir.exists(out_folder)) {
    dir.create(out_folder,
               recursive = TRUE)
    
    cat("Folder created: ", "\n")
    print(out_folder)
  }
  
  # stop if it doesn't
  stopifnot(dir.exists(out_folder))
  
  # complete path and filename
  temp_file <- file.path(out_folder,
                         paste0(last_since, "____", 
                                pair_in, ".csv"))

  tryCatch({
    
    data.table::fwrite(curr_dat, file = temp_file,
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
