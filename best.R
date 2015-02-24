##Finding the best Hosptial in the State
#Write a function called best that take two arguments: the 2-character 
#abbreviated name of a state and an outcome name. The function reads the 
#outcome-of-care-measures.csv file and returns a character vector with the name 
#of the hospital that has the best (i.e. lowest) 30-day mortality for the 
#specified outcome in that state. The hospital name is the name provided in the 
#Hospital.Name variable. The outcomes can be one of “heart attack”, “heart 
#failure”, or “pneumonia”. Hospitals that do not have data on a particular 
#outcome should be excluded from the set of hospitals when deciding the 
#rankings.

#Handling ties. If there is a tie for the best hospital for a given outcome, 
#then the hospital names should be sorted in alphabetical order and the first 
#hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are 
#tied for best, then hospital “b” should be returned).

best <- function(state, outcome) {
        
        ## Read outcome data
        fulldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        id<-c(2,7,11,17,23)
        outcome.list<- lapply(fulldata[,id], c)
        #Cut out the columns we want
        cut.data<-as.data.frame(do.call(cbind, fulldata[,id]))
        
        
        #cut.data<- cbind.data.frame(fulldata$State, as.numeric(fulldata[,11]), 
        #as.numeric(fulldata[,17]),as.numeric(fulldata[,23]))
         
        
        names(cut.data)<- c('Hospital', 'State','heart attack','heart failure', 'pneumonia')
        ## Check that state and outcome are valid
        comes <- c('heart attack', 'heart failure', 'pneumonia')
        if (!is.element(outcome,comes)) stop("invalid outcome")
        places<- unique(fulldata$State)
        if (!is.element(state,places)) stop("invalid state")
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## Pull out the state and outcome you need
        sel.data <- cut.data [cut.data$State==state, names(cut.data) %in% 
                                      c(outcome, 'Hospital') ]
        sel.data[,2]<-as.numeric(as.character(sel.data[,2]))
        
        sel.data<-sel.data[order(sel.data[,1]),]
        sel.data<-sel.data[complete.cases(sel.data),]
        
        state.min<- sel.data[sel.data[,2]==min(sel.data[,2]),]
        
        as.character(state.min[1,1])
        
}

###The function should check the validity of its arguments. If an invalid state 
###value is passed to best, the function should throw an error via the stop 
###function with the exact message “invalid state”. If an invalid outcome value 
###is passed to best, the function should throw an error via the stop function 
###with the exact message “invalid outcome”.
