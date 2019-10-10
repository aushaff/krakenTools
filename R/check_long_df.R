#'@title check long df
#'@description Convert data downloaded from kraken to long dataframe
#'@export
check_long_df <- function(curr_raw_dir_in,
                          curr_dat_prod_dir_in,
                          curr_asset_in,
                          complete_check = FALSE) {
  
  # check that both necessary directories exist
  raw_bool <- dir.exists(curr_raw_dir_in)
  dp_bool <- dir.exists(curr_dat_prod_dir_in)
  
  if(isFALSE(raw_bool)) {
    return("Raw directory does not exist")
  }
  if(isFALSE(dp_bool)) {
    return("Data products directory does not exist")
  }
  
  #=============================================================
  # check for and then take in existing long dataframe to be checked
  # stopping if long dataframe doesn't exist
  curr_long_filename <- list.files(curr_dat_prod_dir_in,
                                   pattern = "long_tick_data.csv")
  
  curr_long_since <- strsplit(curr_long_filename,
                              paste0("__", curr_asset_in))[[1]][1]
  #print(curr_long_since)

  
  if(length(curr_long_filename)!=1) {
    return("Long file does not exist")
  }

  curr_long_df <- readr::read_delim(file.path(curr_dat_prod_dir_in,
                                              curr_long_filename),
                                    delim = ";")
  
  #================================================================
  # Check to see if a metadata file exists
  curr_meta_filename <- list.files(curr_dat_prod_dir_in,
                                   pattern = "long_metadata")
  
  if(length(curr_meta_filename)>0) {
    curr_meta_since <- strsplit(curr_meta_filename,
                                paste0("__", curr_asset_in))[[1]][1]
  } else {
    curr_meta_since <- NA
  }
  # print(curr_meta_since)

  #================================================================
  # Check to see if an eror file exists
  print(curr_dat_prod_dir_in)
  curr_error_filename <- list.files(curr_dat_prod_dir_in,
                                   pattern = "check_ERROR")

  if(length(curr_error_filename)>0) {
    
    curr_error_since <- strsplit(curr_error_filename,
                                 paste0("__", curr_asset_in))[[1]][1]
  } else {
    curr_error_since <- NA
  }

  #print(curr_error_since)
  
  #================================================================
  # Compare the since on the metadata, error and long files
  print(curr_long_filename)
  print(curr_long_since)
  print(curr_meta_filename)
  print(curr_meta_since)
  print(curr_error_filename)
  print(curr_error_since)
  
  # if the error and/or metadata files don't exist then check for errors'
  if(is.na(curr_meta_filename)|is.na(curr_error_filename)) {
    
    print("check files")
  } else if ((curr_long_since != curr_meta_since) != curr_error_since) {
    
    print("check files")
    
  } else {
    print("nothing to check")
  }
  # what about whe n olylong_df and meteadata?
  
  # if the since is the same for all three no files to check
  stop()
  
  
  # if the since is not the same for the error and metadata file then there is 
  # a problem with creation of the long_df
  
  # if the since is different between the long_df, and the error and metadata
  # files then check for errors. 
  #==============================================
  # all the files in the raw directory
  all_files <- list.files(curr_raw_dir_in)
  
  #=============================================================
  # Need to find which raw files have been compared and which haven't
  curr_meta_filename <- list.files(curr_dat_prod_dir_in,
                                      pattern = "long_metadata_df.csv")
  
  # if the metadata file exists the long_df has been checked before
  if(length(curr_meta_filename)==1) {
    
    last_checked_file <- paste0(strsplit(curr_meta_filename, "_")[[1]][1], 
                                "____", curr_asset_in, ".csv")
    
    # where the last checked file sits in all_files
    lc_ind <- match(last_checked_file, all_files)
    
    # if the index of the last checked file is the same as the number of 
    # raw files all current files have been checked
    if(lc_ind == length(all_files)) {
      
      return("No files to check: all up to date")
      
    } else { # subset just to those files that have not been checked
      
      # subset all_files to just those after the last_checked file
      files_to_check <- all_files[(lc_ind+1):length(all_files)]  
    }
  } else if (length(curr_meta_filename)==0) { # no previous check 
    
    files_to_check <- all_files
    
  } else {
    return("Incorrect number of metadata files present")
  }
 
  #=====================
  # find duplicated rows - we are still checking the whole long here
  dup_inds <- which(duplicated(curr_long_df) | 
                      duplicated(curr_long_df, fromLast = TRUE))
  
  dup_rows <- curr_long_df[dup_inds, ]
  dup_rows <- cbind(row_no = dup_inds, dup_rows)

  if(nrow(dup_rows)>0) {
    dup_rows$flag <- "DUPLICATED"
  }
  #============================================================
  # find rows from the raw data that don't exist in the long_df
  # find all the files
 
  # take in each raw file in turn
  # this returns a dataframe of the rows from the raw data that aren't in the 
  # long_df
  print("b4 rows_nil")
  rows_not_in_long <- do.call(rbind, lapply(files_to_check, function(curr_file) {
    
    curr_dat <- readr::read_delim(file.path(curr_raw_dir_in, curr_file),
                           delim = ";")
    
    not_in_long <- dplyr::anti_join(curr_dat, curr_long_df)
    
    return(not_in_long)
  }))
  print("aft rows nil")
  
  if(nrow(rows_not_in_long)>0) {
    
    rows_not_in_long$flag <- "NOT_IN_LONG"
  }
  
  #========================================================
  # data to be put out
  
  # meta_out
  last_date <- strsplit(curr_long_filename, "__")[[1]][1]
  n_rows <- nrow(curr_long_df)
  date_checked <- Sys.time()
  
  meta_out_df <- data.frame(last_date = last_date,
                            n_rows = n_rows,
                            date_checked = date_checked)
  # error_out
  error_out_df <- rbind(dup_rows, rows_not_in_long)
 
  #========================================================
  #Create metadata file 
  
  # if a metadata file exists read it in and add our new metadata to it
  if(length(curr_meta_filename)==1) {
    
    previous_meta <- readr::read_delim(file.path(curr_dat_prod_dir_in, 
                                          curr_meta_filename),
                                ";")
    meta_out_df <- rbind(previous_meta, meta_out_df)
    
    file.remove(file.path(curr_dat_prod_dir_in, curr_meta_filename))
  }
  
  # create the filename and write the meta data out
  meta_filename <- paste0(strsplit(curr_long_filename, "_long")[[1]][1],
                          "_long_metadata_df.csv")
  
  readr::write_delim(meta_out_df, file.path(curr_dat_prod_dir_in, 
                                       meta_filename),
                     delim = ";")
  
  #========================================================  
  # Error file
  
  # check for the existence of a previous error file
  previous_error_filename <- list.files(curr_dat_prod_dir_in, 
                                   pattern = "ERROR")
  
  # if it exists read it in and combine the two 
  if(length(previous_error_filename)==1) {
    
    prev_err <- readr::read_delim(file.path(curr_dat_prod_dir_in,
                                     previous_error_filename),
                           ";")
    
    error_out_df <- rbind(prev_err, error_out_df)
    error_out_df <- error_out_df[!duplicated(error_out_df), ]  
  }
  
  if(nrow(error_out_df)>0) { # this is if there are duplicates or missing rows in long_df
    
    new_error_filename <- paste0(strsplit(curr_long_filename, "_long")[[1]][1],
                                 "_check_ERROR_df.csv")
    
    readr::write_delim(error_out_df, 
                       file.path(curr_dat_prod_dir_in, 
                                 new_error_filename),
               delim = ";")
  }
  
  return("Long dataframe updated")
}