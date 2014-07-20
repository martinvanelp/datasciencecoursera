complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        name <- integer()
        nobs <- integer()
        
        for(i in 1:length(id)) {
                
                if(id[i] < 10) {
                        csv <- paste(directory, "/00", id[i], ".csv", sep = "")                
                } else if(id[i] < 100) {
                        csv <- paste(directory, "/0", id[i], ".csv", sep = "")
                } else {
                        csv <- paste(directory, "/", id[i], ".csv", sep = "")
                }
                
                data <- read.csv(csv)
                
                name <- c(name, id[i])
                nobs <- c(nobs, nrow(data[complete.cases(data), ]))
                
        }
        
        list <- data.frame(name, nobs)
        names(list) <- c("id", "nobs")
        
        list

}