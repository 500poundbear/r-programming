pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## directory - directory of csv files
  ## pollutant - either 'sulfate' or 'nitrate'
  ## id - the csv files to obtain mean
  
  path_var <- c(directory, '/')
  
  if(!identical(pollutant, "nitrate") && !identical(pollutant, "sulfate")) {
    print("ERROR: Invalid pollutant type")
    return
  }
  
  vals = vector(mode="numeric", length=0)
  
  for(current_id in id) {
    
    current_file <- paste(c(str_pad(current_id, 3, side="left", "0"), '.csv'), collapse="")
    
    current_data <- read.csv(current_file)
    
    vals <- c(vals, current_data[,pollutant])
  }
  mean(vals, na.rm=TRUE)
}