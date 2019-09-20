#'@title To long df
#'@description Convert data downloaded from kraken to long dataframe
#'@export
#'
to_long <- function(in_curr, 
                    raw_dir_in, 
                    dp_dir_in, 
                    duplicate_check = TRUE) {
  
  options(digits = 20)
  
  file_suffix <- paste0("__", in_curr, "_long_tick_data.csv")
  #print(file_suffix)
  
  # does the long dataframe exist?
  long_df_check <- list.files(dp_dir_in, pattern = paste0(in_curr,
                                                       "_long_tick_data.csv"))
  if(length(long_df_check)>0) {
    
    print("Long file exists")
    
    # curr_filename
    curr_filename <- long_df_check
    #print(curr_filename)
    
    #curr_last
    curr_last <- strsplit(curr_filename, "_")[[1]][1]
    #print(curr_last)
    
    # files_to_be_read
    all_files <- gtools::mixedsort(list.files(raw_dir_in))
    
    ind <- pmatch(curr_last, all_files)
    
    if(ind==length(all_files)) {
      return("No files to be read")
    } else {
      
      files_to_be_read <- all_files[(ind+1):length(all_files)]  
    }
  
  } else {
    
    # get all filenames from the raw_data directory and order them by unix date
    all_files <- gtools::mixedsort(list.files(raw_dir_in))
    
    # get first file and read it in
    curr_file <- all_files[1]
    new_long_df <- data.table::fread(file.path(raw_dir_in, curr_file))
    
    curr_last <- strsplit(curr_file, "_")[[1]][1]
  
    curr_filename <- paste0(curr_last,
                            file_suffix)
    # save it
    data.table::fwrite(new_long_df, file.path(dp_dir_in, curr_filename),
                       append = FALSE,
                       sep = ";",
                       row.names = FALSE,
                       col.names = TRUE)
    
    files_to_be_read <- all_files[-1]
  }
  
  # here we should have the curr_last, curr_filename and the files_to_be_read

  if(length(files_to_be_read)==0) {

    cat("No files to process", "\n")
    return(0)
  }
  
  
  # append the files_to_be_read to the existing csv file
  sapply(files_to_be_read, function(curr_file) {
    
    in_file <- data.table::fread(file.path(raw_dir_in, curr_file))
    
    curr_last <<- strsplit(curr_file, "_")[[1]][1]

    # check to ensure that we are adding data of the
    # same dimensions
    stopifnot(ncol(in_file)==6)

    # then write
    data.table::fwrite(in_file, file.path(dp_dir_in, curr_filename),
                       append = TRUE,
                       sep = ";",
                       row.names = FALSE,
                       col.names = FALSE,
                       verbose = FALSE,
                       na = NA)
  })
  
  # copy the existing file to a temporary file
  file.copy(file.path(dp_dir_in, curr_filename), 
            file.path(dp_dir_in, "temp.csv"))
  
  #Sys.sleep(5)
  
  # rename the temp file to the new filename
  new_filename <- paste0(curr_last,
                         file_suffix)
    file.rename(file.path(dp_dir_in, "temp.csv"),
              file.path(dp_dir_in, new_filename))
  #Sys.sleep(5)
    
  # delete the previous (old) file
  file.remove(file.path(dp_dir_in, curr_filename))

  return(1)
}

# # load in file and check for (and return existence of) duplicates
# # long_file <- data.table::fread(file.path(dp_dir, long_filename),
# #                    sep = ";")
# # 
# # names(long_file) <- c("price", "volume", "unix_time", 
# #                       "buy_sell", "mark_lim", "misc")
# # 
# # data.table::fwrite(in_file, file.path(dp_dir, long_filename),
# #                    append = TRUE,
# #                    sep = ";",
# #                    row.names = FALSE,
# #                    col.names = FALSE,
# #                    verbose = FALSE,
# #                    na = NA)