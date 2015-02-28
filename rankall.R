rankall <- function (outcome, num="best"){
  # Reads the outcome-of-care-measures.csv file and returns a character vector
  # with the name of the hospital that has the ranking specified by the 'num' 
  # argument. 
  #
  # Args:
  #   outcome: Can be one of “heart attack”, “heart failure”, or “pneumonia”.   
  #   num:     The num argument can take values “best”, “worst”, or an integer 
  #            indicating the ranking (smaller numbers are better).
  # 
  # Returns: A two-column data frame with the name of the hospital with the nth
  # lowest 30-day death rate for that 'outcome' in each state. 
  #
  # Read outcome data
  fulldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define appropriate columns
  id <- c(2, 7, 11, 17, 23)
  
  # Cut out the columns we want: 3 outcomes, state, and hospital name.
  cut.data <- as.data.frame(do.call(cbind, fulldata[, id]))
  
  # Give the columns names       
  names(cut.data) <- c('hospital', 'state', 'heart attack', 'heart failure', 
                       'pneumonia')
  
  # Check that outcome is valid
  valid.outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!is.element(outcome, valid.outcomes)) stop("invalid outcome")

  # Pull out the right columns for outcome and hospitals
  sel.data <- cut.data[ , names(cut.data) %in% c('state', outcome, 'hospital') ]
  sel.data[,3]<-suppressWarnings(as.numeric(as.character(sel.data[, 3])))
  
  # Sort and clean the data:
  # Get rid of missing values
  sel.data <- sel.data[complete.cases(sel.data), ]
  
  # Order by state, then outcome, then by name
  sel.data <- sel.data[order(sel.data[, 2],sel.data[, 3],sel.data[, 1]), ]
  
  # Rank hospitals
  hospital.rank <- (tapply(sel.data[, 3], sel.data[, 2], 
                          rank, ties.method="first"))
  hospital.rank <- do.call(c, hospital.rank)
  sel.data <- cbind(sel.data, hospital.rank)
  
  # Select the hospital with the right rank
  
  # TODO(gherrick): leave the NAs in. 
  
  # find the max ranking for each state
  state.max <- tapply(sel.data[, 4], sel.data[, 2], max)
  state.max <- rep.int(state.max, state.max)
  sel.data <- cbind(sel.data, state.max)
  
  # Define "best"
  if (num=="best") num <- 1
  
  # Determine action for "worst"
  #for (i in 1:nrow(sel.data)){
    if (num=="worst") {
      sel.data <- sel.data[sel.data$hospital.rank==state.max, ] 
      
      # Select the columns we want to clean up the output
      output <- sel.data[, c('hospital','state')]
      
    } else if (num > min(sel.data$state.max)) {
      
      # Create a column for "num" and add to data frame
      num.col <- rep(num,nrow(sel.data))
      sel.data <- cbind(sel.data, state.max, num.col)
      
      # Subset all rows in states where num > state.max
      too.big <- sel.data[sel.data$num.col > sel.data$state.max, ]
      
      # select just the last row for each state
      too.big <- too.big[too.big$hospital.rank==too.big$state.max, ]
      
      # Name the hospital "NA" because there's actually no right answer
      too.big$hospital <- NA
      
      # subset out cases where rank = num 
      sel.data <- sel.data[sel.data$hospital.rank==num, ] 
      
      # Add cases where num > rank to cases where num = rank.
      sel.data <- rbind(too.big, sel.data)
      
      # Order by state, then outcome, then by name (again)
      sel.data <- sel.data[order(sel.data[, 2],sel.data[, 3],sel.data[, 1]), ]
      
      # Select the columns we want to clean up the output
      output <- sel.data[, c('hospital','state')]
      
    } else { sel.data <- sel.data[sel.data$hospital.rank==num, ] 
  
      # Select the columns we want to clean up the output
      output <- sel.data[, c('hospital','state')]
  
      }

  
  rownames(output) <- output$state
  
  output
  
}

# Example use: 
# rankall("heart attack", 20)

