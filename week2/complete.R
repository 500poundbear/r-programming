complete <- function(directory, id = 1:332) {
  ## directory - directory of csv files
  ## id - the csv files to obtain mean
  
  path_var <- c(directory, '/')

  results <- vector(mode="numeric", length=0)
  ind <- 1
  for(current_id in id) {
    current_file <- paste(c(str_pad(current_id, 3, side="left", "0"), '.csv'), collapse="")
    
    current_data <- read.csv(current_file)
    
    results[ind] = nrow(current_data[!is.na(current_data[,"sulfate"]) & !is.na(current_data[,"nitrate"]),])
    ind <- ind + 1
  }
  results
  
  # Build matrix
  
  mat <- cbind(id, results)
  colnames(mat) <- c("id", "nobs")
  rownames(mat) <- 1:(ind - 1)
  as.data.frame(mat)
}