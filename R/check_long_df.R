#'@title check long df
#'@description DONT USE
#'@export
check_long_df <- function(curr_raw_dir_in,
                          curr_dat_prod_dir_in,
                          curr_asset_in,
                          complete_check = FALSE) {
  stop()
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
  # check for existing long dataframe to be checked
  curr_long_filename <- list.files(curr_dat_prod_dir_in,
                                   pattern = "long_tick_data")
  
  if(length(curr_long_filename)!=1) {
    return("Long file does not exist")
  }
  
  curr_long_since <- strsplit(curr_long_filename, 
                              paste0("__", curr_asset_in))[[1]][1]
  
  #=============================================================
  # check for existing metadata file
  curr_meta_filename <- list.files(curr_dat_prod_dir_in,
                                   pattern = "long_metadata")
  
  if(length(curr_meta_filename)>0) {
    curr_meta_since <- strsplit(curr_meta_filename, 
                                paste0("__", curr_asset_in))[[1]][1]
  } else {
    curr_meta_since <- 999999
  }
  
  #=============================================================
  # check for existing error file
  curr_error_filename <- list.files(curr_dat_prod_dir_in,
                                    pattern = "check_ERROR")
  
  if(length(curr_error_filename)>0) {
    curr_error_since <- strsplit(curr_error_filename, 
                                paste0("__", curr_asset_in))[[1]][1]
  } else {
    curr_error_since <- 999999
  }
  
  #============================================================================
  # check to see if all sinces are the same. If they are long has already been
  # checked
  bool <- ifelse(curr_error_since == curr_long_since&
                  curr_meta_since== curr_long_since, TRUE,
         FALSE)
  
  # if metadata and long are the same and there isn't an error file then long
  # has already been checked. 
  bool_alt <- ifelse(curr_meta_since == curr_long_since&
                     curr_error_since == 999999, 
                     TRUE,
                     FALSE)
  # print(curr_long_since)
  # print(curr_meta_since)
  # print(curr_error_since)
  # print(bool)
  # print(bool_alt)

  if(bool==TRUE|bool_alt==TRUE) {
    return("Nothing to check")
  }
  
  #============================================================================
  # After here it is just the current check_long process
  print("...reading current long_df...")
  # take in the long_df
  curr_long_df <- readr::read_delim(file.path(curr_dat_prod_dir_in,
                                              curr_long_filename),
                              delim = ";")
  
  # all the files in the raw directory
  all_files <- list.files(curr_raw_dir_in)
  
  #=============================================================
  # Need to find which raw files have been compared and which haven't
  curr_checked_filename <- list.files(curr_dat_prod_dir_in,
                                      pattern = "long_metadata_df.csv")
  
  # if the metadata file exist the long_df has been checked before
  if(length(curr_checked_filename)==1) {
    
    last_checked_file <- paste0(strsplit(curr_checked_filename, "_")[[1]][1], 
                                "____", curr_asset_in, ".csv")
    
    # where the last checked file sits in all_files
    lc_ind <- match(last_checked_file, all_files)
    
    if(is.na(lc_ind)) {
      stop("metadata since not in files")
    }
    #print(lc_ind)
    
    # if the index of the last checked file is the same as the number of 
    # raw files all current files have been checked
    if(lc_ind == length(all_files)) {
      
      return("No files to check: all up to date - check why this is happening")
      
    } else { # subset just to those files that have not been checked
      
      # subset all_files to just those after the last_checked file
      files_to_check <- all_files[(lc_ind+1):length(all_files)]  
    }
  } else if (length(curr_checked_filename)==0) { # no previous check 
    
    files_to_check <- all_files
    
  } else {
    return("Incorrect number of metadata files present")
  }
  #print(files_to_check)
  
  #=====================
  # This is skipped as should be included in file check
  # # find duplicated rows - we are still checking the whole long here
  # print("...checking duplicates...")
  # 
  # curr_long_sub_dt <- data.table(curr_long_df[,c("price", "volume", "unix_time")])
  # temp_dups <- duplicated(curr_long_sub_dt)
  # stop()  
  # 
  # dup_inds <- which(duplicated(curr_long_df) | 
  #                     duplicated(curr_long_df, fromLast = TRUE))
  # 
  # print(1)
  # dup_rows <- curr_long_df[dup_inds, ]
  # print(2)
  # if(nrow(dup_rows)>0) {
  #   dup_rows$flag <- "DUPLICATED"
  # }
  # #print(dup_inds)
  # #print(dup_rows)
  
  #============================================================
  # find rows from the raw data that don't exist in the long_df
  # find all the files
  #files_to_check <- gtools::mixedsort(list.files(curr_raw_dir_in))
  print("...checking rows...")
  # take in each raw file in turn
  # this returns a dataframe of the rows from the raw data that aren't in the 
  # long_df
  rows_not_in_long <- do.call(rbind, lapply(files_to_check, 
                                            function(curr_file) {
    
    curr_dat <- readr::read_delim(file.path(curr_raw_dir_in, curr_file),
                           delim = ";")
    
    not_in_long <- dplyr::anti_join(curr_dat, curr_long_df)
    rm(curr_dat)
    gc()
    
    return(not_in_long)
  }))
  #print(rows_not_in_long)
  
  if(nrow(rows_not_in_long)>0) {
    
    rows_not_in_long$flag <- "NOT_IN_LONG"
  }
  
  #==============================
  #Create metadata or error files
  print("...binding out information...")
  out_df <- rbind(dup_rows, rows_not_in_long)
  print(out_df)

  if(nrow(out_df)>0) { # this is if there are duplicates or 
                      # missing rows in long_df
  
    print("...writing error file...")
    error_filename <- paste0(strsplit(curr_long_filename, "_long")[[1]][1],
                             "_check_ERROR_df.csv")

    readr::write_delim(out_df, file.path(curr_dat_prod_dir_in,
                                         error_filename),
                       delim = ";")
    
    if((length(curr_error_filename)>0)) {
      
      file.remove(file.path(curr_dat_prod_dir_in, curr_error_filename))
      
    }
  
  } 
  
  print("...writing metadata file...")
  checked_filename <- paste0(strsplit(curr_long_filename, "_long")[[1]][1],
                             "_long_metadata_df.csv")
  
  last_date <- strsplit(curr_long_filename, "__")[[1]][1]
  n_rows <- nrow(curr_long_df)
  date_checked <- Sys.time()
  
  out_df <- data.frame(last_date = last_date,
                       n_rows = n_rows,
                       date_checked = date_checked)
  
  readr::write_delim(out_df, file.path(curr_dat_prod_dir_in, 
                                checked_filename),
              delim = ";")
  
  if((length(curr_meta_filename)>0)) {
    
    file.remove(file.path(curr_dat_prod_dir_in, curr_meta_filename))
    
  }
  
return("Long dataframe checked")

}