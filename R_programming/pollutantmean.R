pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        points <- numeric()
        
        for(i in 1:length(id)) {
                
                if(id[i] < 10) {
                        csv <- paste(directory, "/00", id[i], ".csv", sep = "")                
                } else if(id[i] < 100) {
                        csv <- paste(directory, "/0", id[i], ".csv", sep = "")
                } else {
                        csv <- paste(directory, "/", id[i], ".csv", sep = "")
                }
                
                data <- read.csv(csv)
                
                points <- c(points, 
                            data[pollutant][!is.na(data[pollutant])])
                        
        }
        
        mean(points, na.rm = TRUE)
}

