#'@title To long df
#'@description Convert downloaded data to long dataframe
#'
to_long <- function(folder_root, in_curr) {
  
  # get all filenames from the raw_data directory
  # order them by unix date
  raw_dir <- file.path(folder_root, in_curr, "raw_data")
  dp_dir <- file.path(folder_root, in_curr, "data_products")
  
  all_files <- gtools::mixedsort(list.files(raw_dir))
  return(all_files)
  
  # check to see if long_df exists
  long_filename <- paste0(in_curr, "_long_all_data.csv")
  
  file_exists <- file.exists(file.path(dp_dir, long_filename))
  
  # if doesnt exist:
  if(!file_exists) {
    
    curr_files <- all_files
    
    # do.call rbind curr_files
    new_long_df <- do.call(rbind, lapply(curr_files, 
                                         function(x) {
                                              read.csv(file.path(raw_dir, x),
                                                       header = TRUE)
                                         }))
  } else { # if does exist 
    
    # read in the existing long_df and catch an error
    tryCatch({
      
      old_long_df <- read.csv(file.path(raw_dir, long_filename),
                              header = TRUE)  
    }, warning = function(warn) {
      print(warn)
    }, error = function(err) {
      print(paste0("error reading existing long_df ", long_filename))
    })
   
    # get latest_date in the existing long_df
    rec_date <- min(old_long_df$unix_time)  
    
    # select filenames from all_data that have later 
    # date than rec_date
    
    curr_files <- all_files[all_files > rec_date]
    
    # do.call rbind curr_files
    new_long_df <- do.call(rbind, lapply(curr_files, 
                                         function(x) {
                                           read.csv(file.path(raw_dir, x),
                                                    header = TRUE)
                                         }))
    
    
  }
  
  
  
      
  
  
    
    
  
    # do.call rbind curr_files
    # rbind to existing long_df
  
  
  # check for duplicate rows - remove
  
  # check for missing dates
  
  # back and forwards fill missing dates - add comment column
  
  
}

