best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
        
        ## Check that state and outcome are valid
        if(sum(data$State == state) == 0) stop("invalid state")
        if(outcome != "heart attack" &
           outcome != "heart failure" &
           outcome != "pneumonia") stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(outcome == "heart attack") o <- 11
        if(outcome == "heart failure") o <- 17
        if(outcome == "pneumonia") o <- 23
        
        statedata <- data[c(2, 7, o)][data$State == state, ]
        statedata[, 3] <- suppressWarnings(as.numeric(statedata[, 3]))

        lowest <- min(statedata[, 3], na.rm = TRUE)
        hospital <- statedata[statedata[, 3] == lowest, ]
        
        hospital <- sort(hospital[, 1])
        hospital[1]
        
}