#'@title To long df
#'@description Convert downloaded data to long dataframe
#'
to_long <- function(in_curr, raw_dir, dp_dir) {
  
  #options(digits = 20)
  # get all filenames from the raw_data directory
  # order them by unix date

  all_files <- gtools::mixedsort(list.files(raw_dir))

  # check to see if long_df exists
  long_filename <- paste0(in_curr, "_long_all_tick_data.csv")
  
  file_exists <- file.exists(file.path(dp_dir, long_filename))
  
  # if doesnt exist:
  if(!file_exists) {
  
    #cat("Creating new long-file", long_filename, "\n")
    
    dir.create(dp_dir)
    # take the earliest file from all files
    #curr_file <- min(all_files)
    
    # read it in 
    # new_long_df <- data.table::fread(file.path(raw_dir, curr_file))
    # #new_long_df$last <- strsplit(curr_file, "_")[[1]][1]
    # 
    # # save it 
    # data.table::fwrite(new_long_df, file.path(dp_dir, long_filename),
    #                    append = FALSE,
    #                    sep = ";",
    #                    row.names = FALSE,
    #                    col.names = TRUE)
    #             
    # cat("New long file created", "\n")
  } else {
    file.remove(file.path(dp_dir, long_filename))
  }
  
  # read in the existing long_df and catch an error
  # tryCatch({
  #     
  #   old_long_df <- data.table::fread(file.path(dp_dir, long_filename))
  # }, warning = function(warn) {
  #   stop(warn)
  # }, error = function(err) {
  #   stop(paste0("error reading existing long_df ", long_filename))
  # })
  # 
  # # get latest_date in the existing long_df
  # rec_date <- as.numeric(max(old_long_df$last)  )
  # 
  # # split the 'lasts' from all the file names
  # lasts <- strsplit(all_files, "_")
  # return(lasts)
  # lasts_pt1_n <- as.numeric(as.character(unlist(lasts)[seq(1, length(lasts), 5)]))
  # return(lasts_pt1_n)
  # # make current only those filenames that are later than
  # # the latest date in the long dataframe
  # curr_files <- all_files[rec_date < lasts_pt1_n]
  
  if(length(all_files)==0) {
    
    cat("No files to process", "\n")  
    return(0)
    
  }
  
  # append the curr_files to the existing dataframe
  sapply(all_files, 
           function(curr_file) {
             in_file <- data.table::fread(file.path(raw_dir, curr_file))
             #in_file$last <- strsplit(curr_file, "_")[[1]][1]
             
             # check to ensure that we are adding data of the
             # same dimensions
             stopifnot(ncol(in_file)==6)
             
             # then write
             data.table::fwrite(in_file, file.path(dp_dir, long_filename),
                                append = TRUE,
                                sep = ";",
                                row.names = FALSE,
                                col.names = TRUE,
                                verbose = FALSE,
                                na = NA)
             })

  # load in file and check for (and return existence of) duplicates
  # long_file <- data.table::fread(file.path(dp_dir, long_filename),
  #                    sep = ";")
  # 
  # names(long_file) <- c("price", "volume", "unix_time", 
  #                       "buy_sell", "mark_lim", "misc")
  # 
  # data.table::fwrite(in_file, file.path(dp_dir, long_filename),
  #                    append = TRUE,
  #                    sep = ";",
  #                    row.names = FALSE,
  #                    col.names = FALSE,
  #                    verbose = FALSE,
  #                    na = NA)
  
}

