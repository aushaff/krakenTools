#============================================================================
# check to see if input file exists and if not create it
output_file_setup <- function(file_in){
  
  # if there is a file_in
  if(!is.na(file_in)) {
    
    # if file exists ...
    if(file.exists(file_in)) {
      
      cat("File exists and will be written to", "\n")
      
      # check file can be read and get first and last times
      tryCatch({
        
        # get first time
        first_row <- read.table(file = file_in,
                                nrows = 1,
                                header = TRUE,
                                sep =",")
        # get last time
        line_no <- countLines(file_in)
        
        last_row <- read.csv(file = file_in,
                             skip = line_no -1,
                             header = FALSE)
        if(nrow(first_row)==1&&nrow(last_row)==1) {
          
          # get first date of file and save as first_date
          first_time <- first_row[,3]
          last_time <- last_row[,3]  
          
        } else{
          
          first_time <- 999999999999
          last_time <- 999999999999
        }
      }, error = function(err) {
        
        stop("File exists but cannot be read")
      })
      
    } else { # if file doesnt exist ...
      
      cat("File input: ", "\n", file_in, " doesn't exist. Creating ...")
      
      # call function to create file
      tryCatch({
        create_kraken_tick_file(file_in)
        
        first_time <- 999999999999
        last_time <- 999999999999
        
      }, warning = function(war) {
        print(paste0("Warning recieved creating tick file: ", war))
      }, error = function (err){
        
        stop(paste0("File ", file_in, " cannot be created. ", err))
      })
      
    } # end if(file.exists)
  } else {
    cat("No output file and path specified. Function call requires:
          get_all_historical_trades(pair, path_and_file)", "\n")
  }# end if(!is.na(file_in))
  
  return(list(first_time, last_time))
}