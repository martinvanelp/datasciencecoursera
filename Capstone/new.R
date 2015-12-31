# quickfix
setwd("C:/Users/Martin/Box Sync/Programming/GitHub/datasciencecoursera/Capstone")

# init libraries
library(tm)
library(NLP)
library(openNLP)
library(dplyr)

# constants
language <- "en_US"
lang_S   <- strsplit(language, split = "_")[[1]][1]

# Profanity word list
#      sources for profanity word list:
#      - https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
con <- file("profanity.txt", open = "rb")
profanity <- readLines(con)
close(con)

# load documents
con <- file(paste(getwd(), "/final/", language, 
                  "/", language, ".blogs.txt", sep = ""), open = "rb")
blogs <- readLines(con)
close(con)

con <- file(paste(getwd(), "/final/", language, 
                  "/", language, ".news.txt", sep = ""), open = "rb")
news <- readLines(con)
close(con)

con <- file(paste(getwd(), "/final/", language, 
                  "/", language, ".twitter.txt", sep = ""), open = "rb")
twitter <- readLines(con)
close(con)

# create document library
lib <- c(blogs, news, twitter)

# SENTENCES

# openNLP function to detect sentences
sent_token_annotator <- Maxent_Sent_Token_Annotator()

# store
sentences <- list()

# detect sentences in library and store
for(i in 1:length(lib)) {
        d <- as.String(lib[i])                  # convert
        
        a <- annotate(d, sent_token_annotator)  # detect sentences
        
        if(length(a) == 1) {                    # store single sentence
                x <- length(sentences)
                sentences[x+1] <- d[a]
        }
        else {                                  # store each sentence
                for(j in 1:length(a)) {
                        x <- length(sentences)
                        sentences[x+1] <- d[a][j]
                }
        }
}

saveSent <- function() {
        # save sentences
        con <- file(paste(getwd(), "/sentences.Rdata", sep = ""), open = "wb")
        save(sentences, file = con)
        close(con)
        
        # load dictionary
        con <- file(paste(getwd(), "/sentences.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

set.seed(123)
sentences <- sample(sentences, 1000)

# remove capitals, punctuation, stopwords & profanity and white space
cleanup <- function(x) {
        x <- tolower(x)
        x <- gsub("[[:punct:]]", "", x)
        x <- gsub(paste(profanity,collapse="|"), " ", x)
        # x <- gsub(paste(stopwords(),collapse=" | "), " ", x) # something wrong
        x <- trimws(x)
        
        return(x)
}
        
sentences <- cleanup(sentences)

# split sentences into words
sentSplit <- strsplit(sentences, " ")

# DICTIONARY

dictionary <- data.frame(i1 = "", i2 = "", i3 = "", pred = "",
                         stringsAsFactors = FALSE)

dictionize <- function(s) {
        dict <- data.frame(i1 = "", i2 = "", i3 = "", pred = "",
                           stringsAsFactors = FALSE)
        
        y <- length(s)
        
        for(i in 1:length(s)) {
                d <- nrow(dict)
                
                i1   <- s[y-3]
                i2   <- s[y-2]
                i3   <- s[y-1]
                pred <- s[y]
                
                if(y > 3) {
                        dict[d+1,] <- c(i1, i2, i3, pred)
                }
                else if(y > 2) {
                        dict[d+1,] <- c("", i2, i3, pred)

                }
                else if(y > 1) {
                        dict[d+1,] <- c("", "", i3, pred)
                }
                else if(y > 0) {
                        dict[d,]   <- c("", "", "", pred)
                }
                
                y <- y - 1
        }
        
        return(dict)
        
}

for(i in 1:length(sentSplit)) {
        dictionary <- rbind(dictionary, dictionize(sentSplit[[i]]))
}

# make rows unique in dictionary
gDict <- group_by(dictionary, i1, i2, i3, pred)
uDict <- arrange(summarise(gDict, count = n()), desc(count))

saveDict <- function() {
        # save dictionary
        con <- file(paste(getwd(), "/dictionary.Rdata", sep = ""), open = "wb")
        save(uDict, file = con)
        close(con)
        
        # load dictionary
        con <- file(paste(getwd(), "/dictionary.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

# PREDICTION

# experiment
t <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
t <- cleanup(t)

tSplit <- strsplit(t, " ")

u <- 0

w3 <- tSplit[[1]][length(tSplit[[1]])-u]
w2 <- tSplit[[1]][length(tSplit[[1]])-u-1]
w1 <- tSplit[[1]][length(tSplit[[1]])-u-2]

filter(uDict, i1 == w1, i2 == w2, i3 == w3)$pred[1]
filter(uDict,           i2 == w2, i3 == w3)$pred[1]
filter(uDict, i1 == w1,           i3 == w3)$pred[1]
filter(uDict, i1 == w1, i2 == w2          )$pred[1]
filter(uDict,                     i3 == w3)$pred[1]
filter(uDict,           i2 == w2          )$pred[1]
filter(uDict, i1 == w1                    )$pred[1]
filter(uDict                              )$pred[1]

# numerization is for later
for (i in 1:length(strSp)) {
        LtoN     <- match(strSp[[i]], letters)
        LtoN[1]  <- LtoN[1] * 100
        strSt[i] <- sd(LtoN)
}
