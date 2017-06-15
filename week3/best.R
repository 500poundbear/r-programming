best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  #list(set = set, get = get, getinv = getinv, setinv = setinv)
  
  lc <- list(heartA = 11, heartF=17, pneum=23)
  
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
  ## Return hospital name in that state with lowest 30-day death
  
  hname_label <- 'Hospital.Name'
  
  if(identical(outcome, 'heart attack')) {
    state_data <- outcome_data[outcome_data['State'] == state, c(2, lc$heartA)]
      suppressWarnings(
        state_data[which.min(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),hname_label])
  } else if(identical(outcome, 'heart failure')) {
    state_data <- outcome_data[outcome_data['State'] == state, c(2, lc$heartF)]
    suppressWarnings(
      state_data[which.min(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),hname_label])
  }else {
    state_data <- outcome_data[outcome_data['State'] == state, c(2, lc$pneum)]
    suppressWarnings(
      state_data[which.min(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),hname_label])
  }
}