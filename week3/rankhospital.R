rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## column numbers
  lc <- list(heartA = 11, heartF=17, pneum=23, raw_hospital_col=2)
  
  ## Check that state and outcome are valid
  
  valid_states <- unique(outcome_data[,"State"])
  
  if(!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  if(!identical(outcome, 'heart attack') && 
     !identical(outcome, 'heart failure') && 
     !identical(outcome, 'pneumonia')) {
    
    stop("invalid outcome")
  } 
  
  hname_label <- 'Hospital.Name'
  
  ## Obtain the filtered data (hospital name and rate)
  
  if(identical(outcome, 'heart attack')) {
    state_data <- outcome_data[outcome_data['State'] == state, c(lc$raw_hospital_col, lc$heartA)]
  } else if (identical(outcome, 'heart failure')) {
    state_data <- outcome_data[outcome_data['State'] == state, c(lc$raw_hospital_col, lc$heartF)]
  } else {
    state_data <- outcome_data[outcome_data['State'] == state, c(lc$raw_hospital_col, lc$pneum)]
  } 
  
  colnames(state_data)<- c("name", "rate")
  filtered_data <- state_data[!state_data$rate == "Not Available",]
  
  num_results <- nrow(filtered_data)
  
  ## Calculate rank to retrieve & detect if rank is valid or not
  
  if (identical(num, "best")) { rank = 1 }
  else if (identical(num, "worst")) { rank = num_results }
  else { rank = num }
  
  ## Check if rank is valid
  
  if (rank > num_results || rank <= 0) { stop("NA") }
  
  ## Return hospital name in that state with nth 30-day death
  
  rankings <- filtered_data[order(as.numeric(filtered_data$rate), filtered_data$name),]
  
  return(rankings[rank,"name"])
}