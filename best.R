best <- function(state, outcome) {

  # Reads the outcome-of-care-measures.csv file, selects given state and outcome
  # data, sorts by mortality to provide the hospital with the lowest mortality 
  # rate for the "outcome" and state selected. 
  #
  # Args: 
  #   state:   Two letter character string abbreviation for each state in the 
  #            State variable e.g. "OH"
  #   outcome: Can be one of “heart attack”, “heart failure”, or “pneumonia”. 
  #
  # Returns: 
  #   The name of the Hospital with the best (lowest) 30-day mortality rate for 
  #   the given disease "outcome" in that state. 
  #     
  # Read outcome data
  fulldata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # Select appropriate columns
  id <- c(2, 7, 11, 17, 23)
  outcome.list<- lapply(fulldata[, id], c)
  # Cut out the columns we want
  cut.data <- as.data.frame(do.call(cbind, fulldata[, id]))
  # Give the columns names       
  names(cut.data) <- c('Hospital', 'State', 'heart attack', 'heart failure', 
                       'pneumonia')
  # Check that state and outcome are valid
  comes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!is.element(outcome, comes)) stop("invalid outcome")
  places <- unique(fulldata$State)
  if (!is.element(state,places)) stop("invalid state")
        
  # Return hospital name in that state with lowest 30-day death rate
  # Pull out the right state (rows), outcome (column), and hospital names (col) 
  sel.data <- cut.data[cut.data$State == state, names(cut.data) %in% 
                       c(outcome, 'Hospital') ]
  sel.data[,2]<-suppressWarnings(as.numeric(as.character(sel.data[, 2])))
        
  # Sort and clean the data:
  # Order the rows by hospital name. 
  sel.data <- sel.data[order(sel.data[, 1]), ]
  # Get rid of missing values
  sel.data <- sel.data[complete.cases(sel.data), ]
  # Get the minimum  
  state.min<- sel.data[sel.data[,2]==min(sel.data[,2]),]

  # Return the hospital name     
  as.character(state.min[1,1])
        
}

