corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        data <- complete(directory)
        
        select <- data["nobs"] > threshold
        
        id <- data["id"][select]
        
        stat <- numeric()
        
        if(length(id) == 0) { return(stat) }
        
        for(i in 1:length(id)) {
                
                if(id[i] < 10) {
                        csv <- paste(directory, "/00", id[i], ".csv", sep = "")                
                } else if(id[i] < 100) {
                        csv <- paste(directory, "/0", id[i], ".csv", sep = "")
                } else {
                        csv <- paste(directory, "/", id[i], ".csv", sep = "")
                }
                
                data <- read.csv(csv)
                complete <- complete.cases(data)
                data <- data[complete, ]
                
                n <- data["nitrate"]
                s <- data["sulfate"]
                
                stat <- c(stat, cor(n, s))
                
        }
        
        return(stat)
        
}