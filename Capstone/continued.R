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

# generic functions
saveObj <- function(object, File) {
        con <- file(File, open = "wb")
        save(object, file = con)
        close(con)
}

loadObj <- function(File) {
        con <- file(File, open = "rb")
        load(file = con)
        close(con)
}

# numerization function
# for (i in 1:length(strSp)) {
#         LtoN     <- match(strSp[[i]], letters)
#         LtoN[1]  <- LtoN[1] * 100
#         strSt[i] <- sd(LtoN)
# }

LtoN <- function(Word) {
        if(Word == "") { return(0) }
        
        lettered   <- strsplit(Word, "")
        numbers    <- match(lettered[[1]], letters)
        numbers[1] <- numbers[1] * 100
        
        o <- if(length(numbers) > 1) { sd (numbers) } else { numbers[1] }
        
        return(o)
}

# Profanity word list
#      sources for profanity word list:
#      - https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
con <- file("profanity.txt", open = "rb")
profanity <- readLines(con)
close(con)

# LOAD DOCUMENTS AND TRANSFORM TO SENTENCES
documentLoader <- function(input, lines = -1L) {
        print("DOCUMENT LOADER")
        print(Sys.time())
        
        # open input
        print("Open input")
        con <- file(input, open = "rb")
        doc <- readLines(con, encoding="UTF-8", n = lines)
        close(con) 
        print(Sys.time())
        
        # openNLP function to detect sentences
        sent_token_annotator <- Maxent_Sent_Token_Annotator()
        
        # storing sentences
        sent.list <- vector('list', length(doc))
        
        # detect sentences in document and store
        print("Store sentences")
        print(Sys.time())
        z <- 1000
        
        for(i in 1:length(doc)) {                       # each line
                d <- as.String(doc[i])                  # convert
                a <- annotate(d, sent_token_annotator)  # detect sentences
                sentences <- vector()               # storage for each line
                
                if(length(a) == 1) {                    # store single sentence
                        sentences[1] <- d[a]
                }
                else {                                  # store each sentence
                        for(j in 1:length(a)) {
                                sentences[j] <- d[a][j]
                        }
                }
                
                sent.list[[i]] <- as.data.frame(sentences, stringsAsFactors = FALSE)
                
                # progress bar
                if(i == z) { print(z); print(Sys.time()); z <- z + 1000}
        }
        
        print(i)
        print(Sys.time())
        
        print("Combine sentences")
        sent.df <- data.frame(rbindlist(sent.list), stringsAsFactors = FALSE)
        print(Sys.time())
        
        return(sent.df)
}

blogs   <- documentLoader(paste(getwd(), "/final/", language,
                                "/", language, ".blogs.txt", sep = ""))

news    <- documentLoader(paste(getwd(), "/final/", language,
                                "/", language, ".news.txt", sep = ""))

twitter <- documentLoader(paste(getwd(), "/final/", language,
                                "/", language, ".twitter.txt", sep = ""))

# save block
saveObj(blogs,   paste(getwd(), "/output/sentences/", Sys.Date(),
                       ".blogs.Rdata", sep = ""))
saveObj(news,    paste(getwd(), "/output/sentences/", Sys.Date(),
                       ".news.Rdata", sep = ""))
saveObj(twitter, paste(getwd(), "/output/sentences/", Sys.Date(),
                       ".twitter.Rdata", sep = ""))

# load block
# blogs   <- loadObj(paste(getwd(), "/output/sentences/", Sys.Date(),
#                        ".blogs.Rdata", sep = ""))
# news    <- loadObj(paste(getwd(), "/output/sentences/", Sys.Date(),
#                        ".news.Rdata", sep = ""))
# twitter <- loadObj(paste(getwd(), "/output/sentences/", Sys.Date(),
#                        ".twitter.Rdata", sep = ""))
File <- paste(getwd(), "/output/sentences/", Sys.Date()-1,".blogs.Rdata", sep = "")
load(File); blogs <- object; object <- 0
File <- paste(getwd(), "/output/sentences/", Sys.Date()-1,".news.Rdata", sep = "")
load(File); news <- sent.df; sent.df <- 0
File <- paste(getwd(), "/output/sentences/", Sys.Date()-1,".twitter.Rdata", sep = "")
load(File); twitter <- sent.df; sent.df <- 0

# CLEANUP SENTENCES
# remove capitals, punctuation, stopwords & profanity and white space
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

blogsC   <- cleanup(blogs$sentences)
newsC    <- cleanup(news$sentences)
twitterC <- cleanup(twitter$sentences)

# save block
saveObj(blogsC,   paste(getwd(), "/output/cleaned/", Sys.Date(),
                        ".blogs.Rdata", sep = ""))
saveObj(newsC,    paste(getwd(), "/output/cleaned/", Sys.Date(),
                        ".news.Rdata", sep = ""))
saveObj(twitterC, paste(getwd(), "/output/cleaned/", Sys.Date(),
                        ".twitter.Rdata", sep = ""))

# load block
File <- paste(getwd(), "/output/cleaned/", Sys.Date(),".blogs.Rdata", sep = "")
load(File); blogsC <- object; object <- 0
File <- paste(getwd(), "/output/cleaned/", Sys.Date(),".news.Rdata", sep = "")
load(File); newsC <- object; object <- 0
File <- paste(getwd(), "/output/cleaned/", Sys.Date(),".twitter.Rdata", sep = "")
load(File); twitterC <- object; object <- 0

set.seed(123)
blogsC   <- sample(blogsC, 180000)
newsC    <- sample(newsC, 200000)
twitterC <- sample(twitterC, 480000)

# split sentences into words
blogsW   <- strsplit(blogsC, " ")
newsW    <- strsplit(newsC, " ")
twitterW <- strsplit(twitterC, " ")

# DICTIONARY

dictionize <- function(s) {     # a solution with rbindlist is WORSE
        dict <- data.frame(i1 = 0, i2 = 0, i3 = 0, pred = "",
                           stringsAsFactors = FALSE)
        
        y <- length(s)
        
        for(i in 1:length(s)) {
                if(y > 3) {                     # 4n-grams
                dict[i,] <- c(LtoN(s[y-3]),   # 3rd preceding word
                                LtoN(s[y-2]),   # 2nd preceding word
                                LtoN(s[y-1]),   # 1st preceding word
                                s[y])           # word to predict
                }
                else if(y > 2) {
                dict[i,] <- c( 0, 
                                 LtoN(s[y-2]), 
                                 LtoN(s[y-1]), 
                                 s[y])
                }
                else if(y > 1) {
                dict[i,] <- c( 0, 
                                 0, 
                                 LtoN(s[y-1]), 
                                 s[y])
                }
                else if(y > 0) {
                dict[i,]   <- c( 0, 
                                 0, 
                                 0, 
                                 s[y])
                }
                
                y <- y - 1
        }
        return(dict)
}

makeDict <- function(s) {
        print("MAKING DICTIONARY")
        dict.list <- vector('list', length(s))
        
        j <- 1000
        print(Sys.time())
        
        for(i in 1:length(s)) {
                dict.list[[i]] <- dictionize(s[[i]])
                
                # progress bar
                if(i == j) {
                        print(paste(i, "/", length(s)))
                        print(Sys.time())
                        flush.console() # update GUI console
                        j <- j + 1000
                }
        }
        
        print(paste(i, "/", length(s)))
        print(Sys.time())
        
        print("Combine sentence dictionaries")
        dict.df <- data.frame(rbindlist(dict.list))
        
        print(Sys.time())
        
        return(dict.df)
}

blogsD   <- makeDict(blogsW)
newsD    <- makeDict(newsW)
twitterD <- makeDict(twitterW)

# save block
saveObj(blogsD,   paste(getwd(), "/output/dictionized/", Sys.Date(),
                        ".blogs.Rdata", sep = ""))
saveObj(newsD,    paste(getwd(), "/output/dictionized/", Sys.Date(),
                        ".news.Rdata", sep = ""))
saveObj(twitterD, paste(getwd(), "/output/dictionized/", Sys.Date(),
                        ".twitter.Rdata", sep = ""))

# load block
File <- paste(getwd(), "/output/dictionized/", Sys.Date(),".blogs.Rdata", sep = "")
load(File); blogsD <- object; object <- 0
File <- paste(getwd(), "/output/dictionized/", Sys.Date(),".news.Rdata", sep = "")
load(File); newsD <- object; object <- 0
File <- paste(getwd(), "/output/dictionized/", Sys.Date(),".twitter.Rdata", sep = "")
load(File); twitterD <- object; object <- 0

# make rows unique in dictionary
dictionary <- rbind(blogsD, newsD, twitterD)

gDict <- group_by(dictionary, i1, i2, i3, pred)
uDict <- ungroup( 
        arrange(
                filter(summarise(gDict, count = n()),
                       count > 1, pred != ""),
                desc(count))
)

# save & load block
saveObj(uDict, paste(getwd(), "/output/dictionary/", Sys.Date(),
                        ".uDict.Rdata", sep = ""))
File <- paste(getwd(), "/output/dictionary/", Sys.Date(), 
                        ".uDict.Rdata", sep = "")
load(File); uDict <- object; object <- 0

# PREDICTION

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

# # TOLSTOY
# tolstoy <- "The world would be better did such a"

# Quiz 2: Natural language processing I
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

# Quiz 3: Natural language processing II
r1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
r2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
r3 <- "I'd give anything to see arctic monkeys this"
r4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
r5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
r6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
r7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
r8 <- "Every inch of you is perfect from the bottom to the"
r9 <- "I'm thankful my childhood was filled with imagination and bruises from playing"
r10 <- "I like how the same people are in almost all of Adam Sandler's"

predictor <- function(Dictionary, Sentence, NtoReturn = 1, StepsBack = 0) { 
        t <- suppressMessages(cleanup(Sentence))
        tSplit <- strsplit(t, " ")
        
        w3 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-0])
        w2 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-1])
        w1 <- LtoN(tSplit[[1]][length(tSplit[[1]])-StepsBack-2])

        prediction(Dictionary, w1, w2, w3, NtoReturn)
}

predictor(uDict, q2, NtoReturn = 3)
