rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  valid_states <- sort(unique(outcome_data[,"State"]))
  
  
  ## Check that outcome is valid
  if(!identical(outcome, 'heart attack') && 
     !identical(outcome, 'heart failure') && 
     !identical(outcome, 'pneumonia')) {
    
    stop("invalid outcome")
  } 
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  hospital_names <- vector('character')
  
  for(i in 1:length(valid_states)) {
    state_rankings <- getData(valid_states[i], outcome, outcome_data)
    hospital_names[i] <- getrankforState(state_rankings, num)
  }
  
  resultdt <- data.frame(hospital_names, valid_states)
  rownames(resultdt) <- valid_states
  colnames(resultdt) <- c("hospital", "state")
  
  resultdt
}

getData <- function(state, outcome, raw_data) {
  
  ## column numbers
  lc <- list(heartA = 11, heartF=17, pneum=23, raw_hospital_col=2)
  
  if(identical(outcome, 'heart attack')) {
    state_data <- raw_data[raw_data['State'] == state, c(lc$raw_hospital_col, lc$heartA)]
  } else if (identical(outcome, 'heart failure')) {
    state_data <- raw_data[raw_data['State'] == state, c(lc$raw_hospital_col, lc$heartF)]
  } else {
    state_data <-raw_data[raw_data['State'] == state, c(lc$raw_hospital_col, lc$pneum)]
  } 
  
  colnames(state_data)<- c("name", "rate")
  filtered_data <- state_data[!state_data$rate == "Not Available",]
  
  filtered_ranked_data <- filtered_data[order(as.numeric(filtered_data$rate), filtered_data$name),]
  
  return(filtered_ranked_data)
}

getrankforState <- function(rankings, num="best") {
  
  num_results <- nrow(rankings)
  
  # Calculate rank
  if (identical(num, "best")) { rank = 1 }
  else if (identical(num, "worst")) { rank = num_results }
  else { rank = num }
  
  ## Check if rank is valid
  if (rank > num_results || rank <= 0) { return("<NA>") }
  
  return(rankings[rank,"name"])
}