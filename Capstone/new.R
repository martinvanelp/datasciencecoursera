# quickfix
setwd("C:/Users/Martin/Box Sync/Programming/GitHub/datasciencecoursera/Capstone")

# init libraries
library(tm)
library(NLP)
library(openNLP)
library(dplyr)
library(data.table)

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
print("Store sentences")
for(i in 1:length(lib)) {
  print(Sys.time())
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
  print("Sentences stored")
  print(Sys.time())
}

saveSent <- function() {
        # save sentences
        con <- file(paste(getwd(), "/sentences.Rdata", sep = ""), open = "wb")
        save(sentences, file = con)
        close(con)
        
        # load sentences
        con <- file(paste(getwd(), "/sentences.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

# # sample sentences
# set.seed(123)
# sentences <- sample(sentences, 100000)

# # TOLSTOY
# tolstoyDoc <- function() {
#   con <- file(paste(getwd(), "/tolstoy.txt", sep = ""), open = "rb")
#   text <- readLines(con)
#   close(con)
#   
#   return(text)
# }
# 
# sentences <- tolstoyDoc()

# remove capitals, punctuation, stopwords & profanity and white space
cleanup <- function(x) {
        print("Cleanup:")
        x <- tolower(x); print("- Lowered")
        x <- gsub("[[:punct:]]", "", x); print("- Depunctuated")
        x <- gsub(paste(profanity,collapse="|"), " ", x); print("- Unprofaned")
        # x <- gsub(paste(stopwords(),collapse=" | "), " ", x) # something wrong
        x <- trimws(x); print("- Dewhitespaced")
        return(x)
}
        
sentences <- cleanup(sentences)

saveClSent <- function() {
        # save sentences
        con <- file(paste(getwd(), "/cleanSentences.Rdata", sep = ""), open = "wb")
        save(sentences, file = con)
        close(con)
        
        # load sentences
        con <- file(paste(getwd(), "/cleanSentences.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

# split sentences into words
sentSplit <- strsplit(sentences, " ")

# DICTIONARY

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

# dictionize <- function(s) {
#   dict.list <- vector('list', length(s))
# 
#   for(i in length(s):1) {
#     i1   <- s[i-3]
#     i2   <- s[i-2]
#     i3   <- s[i-1]
#     pred <- s[i]
#     
#     if(i > 3) {
#       dict.list[[i]] <- data.frame(i1=i1, i2=i2, i3=i3, pred=pred, stringsAsFactors = FALSE)
#     }
#     else if(i > 2) {
#       dict.list[[i]] <- data.frame(i1="", i2=i2, i3=i3, pred=pred, stringsAsFactors = FALSE)
#     }
#     else if(i > 1) {
#       dict.list[[i]] <- data.frame(i1="", i2="", i3=i3, pred=pred, stringsAsFactors = FALSE)
#     }
#     else if(i > 0) {
#       dict.list[[i]] <- data.frame(i1="", i2="", i3="", pred=pred, stringsAsFactors = FALSE)
#     }
#     
#   }
#   
#   dict.df <- data.frame(rbindlist(dict.list))
#   return(dict.df)
# }

makeDict <- function(s) { 
        dict.list <- vector('list', length(s))
        
        j <- 1000
        print(Sys.time())
        
        for(i in 1:length(s)) {
                dict.list[[i]] <- dictionize(s[[i]])
                
                # progress bar
                if(i == j) {
                        print(i)
                        print(Sys.time())
                        flush.console() # update GUI console
                        j <- j + 1000
                }
        }
        
        print(i)
        print(Sys.time())
    
        print("Combine sentence dictionaries")
        dict.df <- data.frame(rbindlist(dict.list))
        
        print(Sys.time())
        
        return(dict.df)
}

dictionary <- makeDict(sentSplit)

saveDict <- function() {
        # save dictionary
        con <- file(paste(getwd(), "/dictionary.Rdata", sep = ""), open = "wb")
        save(dictionary, file = con)
        close(con)
        
        # load dictionary
        con <- file(paste(getwd(), "/dictionary.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

# make rows unique in dictionary
gDict <- group_by(dictionary, i1, i2, i3, pred)
uDict <- ungroup( 
        filter(
        arrange(summarise(gDict, count = n()), desc(count)),
        count > 1, pred != "")
        )

saveUdict <- function() {
        # save dictionary
        con <- file(paste(getwd(), "/uDict.Rdata", sep = ""), open = "wb")
        save(uDict, file = con)
        close(con)
        
        # load dictionary
        con <- file(paste(getwd(), "/uDict.Rdata", sep = ""), open = "rb")
        load(file = con)
        close(con)
}

# PREDICTION

predict <- function(dict, w1, w2, w3, n = 45) {
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

# # TOLSTOY
# tolstoy <- "The world would be better did such a"

# quiz 2
q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
q3 <- "Hey sunshine, can you follow me and make me the"
q4 <- "Very early observations on the Bills game: Offense still struggling but the"
q5 <- "Go on a romantic date at the"
q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
q9 <- "Be grateful for the good times and keep the faith during the"
q10 <- "If this isn't the cutest thing you've ever seen, then you must be"

t <- cleanup(q2)
tSplit <- strsplit(t, " ")

u <- 0
w3 <- tSplit[[1]][length(tSplit[[1]])-u]
w2 <- tSplit[[1]][length(tSplit[[1]])-u-1]
w1 <- tSplit[[1]][length(tSplit[[1]])-u-2]

predict(uDict, w1, w2, w3, 30)

# numerization is for later
for (i in 1:length(strSp)) {
        LtoN     <- match(strSp[[i]], letters)
        LtoN[1]  <- LtoN[1] * 100
        strSt[i] <- sd(LtoN)
}
