rankall <- function(outcome, num = "best") {
        
        ## Read outcome data and store all states
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character") 
        states <- sort(unique(data$State))
        
        ## Check that outcome is valid
        if(outcome != "heart attack" &
                   outcome != "heart failure" &
                   outcome != "pneumonia") stop("invalid outcome")
        
        ## For each state, find the hospital of the given rank
        #  select right outcome column to extract
        if(outcome == "heart attack") o <- 11
        if(outcome == "heart failure") o <- 17
        if(outcome == "pneumonia") o <- 23
        
        # gather and process all relevant data for all states
        odata <- data[c(2, 7, o)]
        odata[, 3] <- suppressWarnings(as.numeric(odata[, 3]))
        odata <- odata[complete.cases(odata), ]
        odata <- odata[order(odata[, 3], odata[, 1]), ]
        
        # initialize
        hospitals <- data.frame("hospital" = character(length(states)),
                                "state" = character(length(states)),
                                row.names = states,
                                stringsAsFactors = FALSE)
        i <- 1
        
        # loop over each state to find the requested hospital
        for(state in states) {
                
                # state specific dataset
                statedata <- odata[odata$State == state, ]

                # set num right if best/worst                
                if(num == "best") rank <- 1
                else if(num == "worst") rank <- nrow(statedata)
                else rank <- num
                
                # select hospital and add to data frame
                hospitals[i, 1] <- statedata[rank, 1]
                hospitals[i, 2] <- state
                                
                i <- i + 1
                
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospitals
        
}