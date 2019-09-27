#'@title process the errors from check_long_df
#'@description Convert data downloaded from kraken to long dataframe

NOT_USED_error_proc_fun <- function(curr_he_in,
                           kraken_root_dir_in) {
  
  print("=======================")
  print(curr_he_in)
  
  curr_dat_prod_dir <- file.path(kraken_root_dir, curr_he_in , "data_products")
  
  error_file <- list.files(curr_dat_prod_dir, pattern = "ERROR")
  error_file_path <- file.path(curr_dat_prod_dir, error_file)

  # check the error file exists
  if(!file.exists(error_file_path)) {
    return("Error file doesn't exist")
  }
  
  #====================================
  # read in the error file
  error_df <- read_delim(error_file_path,
                         delim = ";")
  
  # add the group numbers
  error_df <- error_df %>% mutate(group_no = as.integer(factor(unix_time)))

  #====================================
  # break it into the different sections
  curr_dups <- filter(error_df, flag == "DUPLICATED")
  curr_miss <- filter(error_df, flag == "NOT_IN_LONG")
  
  # show user how many different duplicate(s) and how many missing rows
  print(paste0("Asset ", curr_he_in, " has ", nrow(unique(curr_dups)), 
               " duplicates with ", nrow(curr_dups), " rows in total. ", 
               nrow(curr_miss), " rows that exist in the raw data are missing in the long_df"))
  
  #====================================
  # process duplicates one by one
  process_dup_bloc <- function(curr_group) {
    
    dups_sub <- filter(curr_dups, group_no==curr_group)
    
    print(paste0("Current duplicates for group ", 
                 curr_group, 
                 " are: "))
    print(dups_sub)
     
    action <- menu(c("Yes", "No"), 
                   title = "Delete duplicates?")
    
    if(action==1) {
      print("Delete the duplicates")
      
      # take in longdataframe
      curr_long_filename <- list.files(curr_dat_prod_dir,
                                       pattern = "long_tick_data.csv")
      curr_long_df <- readr::read_delim(file.path(curr_dat_prod_dir,
                                                  curr_long_filename),
                                        delim = ";")
      
      # find the duplicates in the dataframe that match dups_sub
      # check that dups are in df and get the row numbers
      df_rows <- which(curr_long_df$price==dups_sub$price&
                       curr_long_df$volume==dups_sub$volume&
                       curr_long_df$unix_time==dups_sub$unix_time&
                       curr_long_df$buy_sell==dups_sub$buy_sell)
      
      # same number of rows in the dups and df
      stopifnot(all.equal(nrow(dups_sub), length(df_rows)))
      
      # keep only the first instance of the duplicates
      rows_to_remove <- df_rows[-1]
      return(rows_to_remove)
      # remove the duplicates (leave one row)
      new_df <- curr_long_df[-rows_to_remove, ]
      
      readr::write_delim(new_df, file.path(curr_dat_prod_dir,
                                                  "test_df.csv"),
                                        delim = ";")
      
    } else if (action == 2) {
      print("Keep the duplicates")
    } else {
      stop("Incorrect duplicate selection")
    }
  }
  
  # process the duplicates - save or delete
  sapply(unique(error_df$group_no), process_dup_bloc)
  
  # process the missing rows
  
  # update the metadata file
}