
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)

# load dictionary
load("2016-01-11.uDict.Rdata"); uDict <- object; object <- 0

# transform words (in letters) to numbers
LtoN <- function(Word) {
        # catching "errors"
        if(length(Word) == 0) { return(0) }
        if(Word == "")        { return(0) }
        
        # transformation
        lettered   <- strsplit(Word, "")
        numbers    <- match(lettered[[1]], letters)
        numbers[1] <- numbers[1] * 100
        
        o <- if(length(numbers) > 1) { sd (numbers) } else { numbers[1] }
        
        return(o)
}

# cleanup sentences: remove capitals, punctuation, stopwords & profanity and white space
cleanup <- function(x) {
        message("CLEANUP"); message(Sys.time())
        Encoding(x) <- "UTF-8"
        x <- gsub("[^[:graph:]]", " ", x); message("- Denongraphed"); message(Sys.time()) 
        x <- gsub("[[:punct:]]", " ", x); message("- Depunctuated"); message(Sys.time())
        x <- gsub("[[:digit:]]", " ", x); message("- Denumbered"); message(Sys.time())
        x <- tolower(x); message("- Lowered"); message(Sys.time())
        
        message("+ Start unprofaning")
        for(i in 1:length(profanity)) {
                gsub(paste(profanity[i]), " ", x)
                message(paste(".. Removed profane word ", i, 
                              "/", length(profanity))); message(Sys.time())
        }
        message("- Unprofaned"); message(Sys.time())
        
        # x <- gsub(paste(profanity,collapse="|"), " ", x); message("- Unprofaned"); message(Sys.time())
        # x <- gsub(paste(stopwords(),collapse=" | "), " ", x) # something wrong
        x <- trimws(x); x <- gsub("\\s+", " ", x)
        message("- Dewhitespaced"); message(Sys.time())
        return(x)
}

# Profanity word list
#      sources for profanity word list:
#      - https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
con <- file("profanity.txt", open = "rb")
profanity <- readLines(con)
close(con)

# prediction function
# prediction <- function(text) { return(rbind("the", "I", "text")) }

prediction <- function(dict, w1, w2, w3, n = 45) {
        x <- character()
        x <-      {dict %>% filter(i1 == w1, i2 == w2, i3 == w3) %>%
                        arrange(desc(count)) %>% head(15)}$pred
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(          i2 == w2, i3 == w3) %>%
                        arrange(desc(count)) %>% head(6)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(i1 == w1,           i3 == w3) %>% 
                        arrange(desc(count)) %>% head(6)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(                    i3 == w3) %>% 
                        arrange(desc(count)) %>% head(6)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(i1 == w1, i2 == w2          ) %>% 
                        arrange(desc(count)) %>% head(3)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(          i2 == w2          ) %>% 
                        arrange(desc(count)) %>% head(3)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% filter(i1 == w1                    ) %>% 
                        arrange(desc(count)) %>% head(3)}$pred)
        x <- unique(x); if(length(x) >= n) { return(x[1:n]) }
        x <- c(x, {dict %>% 
                        arrange(desc(count)) %>% head(3)}$pred)
        
        x <- unique(x); return(x[1:n])
}

predictor <- function(Dictionary, Sentence, NtoReturn = 1, StepsBack = 0) { 
        t <- suppressMessages(cleanup(Sentence))
        tSplit <- strsplit(t, " ")
        
        w3 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-0])
        w2 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-1])
        w1 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-2])

        prediction(Dictionary, w1, w2, w3, NtoReturn)
}

# server
shinyServer(function(input, output) {
        
        output$prediction <- renderUI({
                predicted <- predictor(uDict, input$text, NtoReturn = 3)
                HTML(paste(paste("<b><big>1. ", predicted[1], "</big></b>"), 
                           paste("<i>2. ", predicted[2]), 
                           paste("<i>3. ", predicted[3]),
                           sep = "<br/>"))
        })
                
})
