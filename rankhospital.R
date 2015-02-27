rankhospital <- function (state, outcome, num=1){
  # Reads the outcome-of-care-measures.csv file and returns a character vector
  # with the name of the hospital that has the ranking specified by the 'num' 
  # argument. 
  # Args:
  #   state:   Two letter character string abbreviation for each state in the 
  #            State variable e.g. "OH"
  #   outcome: Can be one of “heart attack”, “heart failure”, or “pneumonia”.   
  #   num:     The num argument can take values “best”, “worst”, or an integer 
  #            indicating the ranking (smaller numbers are better).
  # 
  # Returns: A character vector containing the name of the hospital with the nth
  # lowest 30-day death rate for that 'outcome'. 
  #
  # Read outcome data
  fulldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define appropriate columns
  id <- c(2, 7, 11, 17, 23)
 
  # Cut out the columns we want: 3 outcomes, state, and hospital name.
  cut.data <- as.data.frame(do.call(cbind, fulldata[, id]))
  
  # Give the columns names       
  names(cut.data) <- c('Hospital', 'State', 'heart attack', 'heart failure', 
                       'pneumonia')
  
  # Check that state and outcome are valid
  valid.outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!is.element(outcome, valid.outcomes)) stop("invalid outcome")
  places <- unique(fulldata$State)
  if (!is.element(state, places)) stop("invalid state")
  
  # Pull out the right state (rows), outcome (column), and hospital names (col) 
  sel.data <- cut.data[cut.data$State == state, names(cut.data) %in% 
                         c(outcome, 'Hospital') ]
  sel.data[,2]<-suppressWarnings(as.numeric(as.character(sel.data[, 2])))
  
  # Sort and clean the data:
  # Get rid of missing values
  sel.data <- sel.data[complete.cases(sel.data), ]
  
  # Order by outcome, then by name
  sel.data <- sel.data[order(sel.data[, 2],sel.data[, 1]), ]

  # Rank hospitals
  hospital.rank <- rank(sel.data[, 2], ties.method="first")
  sel.data <- cbind(sel.data, hospital.rank)
  
  # Define "best" and "worst"
  if (num=="best") num <- 1
  if (num=="worst") num <- max(sel.data$hospital.rank)
  
  # Select the hospital with the right rank
  nth.best <- sel.data[sel.data$hospital.rank==num, ]
  
  
  # Return the hospital name     
  as.character(nth.best[1,1])
  
  
}