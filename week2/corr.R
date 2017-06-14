corr <- function(directory, threshold = 0) {
  ## directory - directory of csv files
  ## threshold - whether to calculate cov
  
  path_var <- c(directory, '/')
  
  files <- list.files()
  
  results <- vector(mode="numeric", length=0)
  ind <- 1
  for(file_path in files) {
    
    current_data <- read.csv(file_path)
    
    both_available_rows <- !is.na(current_data[,"sulfate"]) & !is.na(current_data[,"nitrate"]) 
    
    result = nrow(current_data[both_available_rows,])
  
    if (result > threshold) {
      
      result_x=current_data[both_available_rows,"sulfate"]
      result_y=current_data[both_available_rows,"nitrate"]
      
      results[ind] <- cor(result_x, result_y)
      
      ind <- ind + 1
    }  
  }
  results
}