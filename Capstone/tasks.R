# NOTES
# Packages to consider: Rweka (for Ngrams), RTextTools (for Analysis)
# Other word sources: Project Gutenberg, Wordnet

# quickfix
setwd("C:/Users/Martin/Box Sync/Programming/GitHub/datasciencecoursera/Capstone")

##############
##          ##
##  Task 0  ##
##          ##
##############

# DATASET
# This is the training data to get you started that will be the basis for most of the capstone. You must download the data from the Coursera site and not from external websites to start. Your original exploration of the data and modeling steps will be performed on this data set. Later in the capstone, if you find additional data sets that may be useful for building your model you may use them.
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

# TASKS TO ACCOMPLISH
# 1. Obtaining the data - Can you download the data and load/manipulate it in R?
if (!file.exists("data")) { dir.create("data") }
if (!file.exists("data/Coursera-SwiftKey.zip")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileUrl, destfile = "data/Coursera-SwiftKey.zip")
}
if (!file.exists("final")) { 
        unzip("data/Coursera-SwiftKey.zip")
}

# 2. Familiarizing yourself with NLP and text mining - Learn about the basics of natural language processing and how it relates to the data science process you have learned in the Data Science Specialization.

# https://en.wikipedia.org/wiki/Natural_language_processing
# https://en.wikipedia.org/wiki/Hidden_Markov_model
# https://en.wikipedia.org/wiki/Viterbi_algorithm#Example

# http://www.jstatsoft.org/article/view/v025i05
# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

# QUESTIONS TO CONSIDER
# 1. What do the data look like?
# 2. Where do the data come from?
# 3. Can you think of any other data sources that might help you in this project?
# 4. What are the common steps in natural language processing?
# 5. What are some common issues in the analysis of text data?
# 6. What is the relationship between NLP and the concepts you have learned in the Specialization?

# ANSWERS
# 1. big files with lines of text
# 2. blogs, news websites, twitter
# 3. project Gutenberg, Princeton Wordnet
# 4. make your text machine readable, make the content quantifiable
# 5. typos, synonyms, stemming
# 6. NLP is a specific form of data science, applied to text

##############
##          ##
##  Task 1  ##
##          ##
##############

# TASKS TO ACCOMPLISH
# 
# 1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.
# 2. Profanity filtering - removing profanity and other words you do not want to predict.

library(dplyr)
library(tm)
library(caret)
library(SnowballC)
library(rJava)
library(RWeka)

language <- "en_US"

# prepare other constants
lang_S   <- strsplit(language, split = "_")[[1]][1]

# Profanity word list
#      sources for profanity word list:
#      - https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
con <- file("profanity.txt", open = "r")
profanity <- readLines(con)
close(con)

# load all documents for a language
filepath <- paste(getwd(), "/final/", language, sep = "")
texts    <- Corpus(DirSource(filepath), 
                   readerControl = 
                           list(reader = readPlain, 
                                language = language, 
                                load = TRUE)
                   )

# focus on Twitter first, with workaround
con <- file(paste(getwd(), "/final/", language, 
                  "/", language, ".twitter.txt", sep = ""), open = "r")
doc    <- readLines(con, n = 100000)
close(con)

# vs     <- VectorSource(doc)
# elem   <- getElem(stepNext(vs))
# result <- readPlain(elem, "en", "id1")
# result$content  

set.seed(1234)
inTrain  <- sample(1:length(doc), 0.7 * length(doc))

training <- doc[inTrain]
testing  <- doc[-inTrain]

# turn training data into corpus
trainVS   <- VectorSource(training)
trainDocs <- Corpus(trainVS, readerControl = 
                            list(reader = readPlain, 
                                 language = lang_S, 
                                 load = TRUE))

# remove profanity, and for that convert to lower case
trainDocs <- tm_map(trainDocs, content_transformer(tolower))
trainDocs <- tm_map(trainDocs, removeWords, profanity)

# ?gsub + a loop could be an alternative to remove profanity

# first steps
trainDTM  <- DocumentTermMatrix(trainDocs)
trainCS   <- colSums(inspect(removeSparseTerms(trainDTM, 0.995)))

# check profanity presence
sort(colSums(inspect(DocumentTermMatrix(trainDocs, list(dictionary = profanity)))))

# tokenize
trainDTMt <- DocumentTermMatrix(trainDocs, 
                               control = list(tokenize = NGramTokenizer))
trainCSt  <- colSums(inspect(removeSparseTerms(trainDTMt, 0.995)))
sort(trainCSt, decreasing = TRUE)

##############
##          ##
##  QUIZ 1  ##
##          ##
##############

library(dplyr)
library(tm)
library(caret)
library(SnowballC)
library(rJava)
library(RWeka)

language <- "en_US"
lang_S   <- strsplit(language, split = "_")[[1]][1]

document <- ".twitter.txt"
con <- file(paste(getwd(), "/final/", language, 
                  "/", language, document, sep = ""), open = "r")
doc    <- readLines(con)
close(con)

# The en_US.twitter.txt has how many lines of text?
length(doc)

# What is the length of the longest line seen in any of the three en_US data sets?
Nchar <- lapply(doc, nchar)
NcharM <- as.numeric(Nchar)
head(sort(NcharM, decreasing=TRUE))

# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
length(doc[grep("* love *", doc)]) / length(doc[grep("* hate *", doc)])

# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
doc[grep("*biostats*", doc)]

# How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
doc[grep("A computer once beat me at chess, but it was no match for me at kickboxing", doc)]

##############
##          ##
##  Task 2  ##
##          ##
##############

# TASKS TO ACCOMPLISH
# 
# 1. Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora. 
# 2. Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

trainCSw  <- sort(trainCS[!(names(trainCS) %in% stopwords("en"))],
                  decreasing = TRUE)
trainPlot <- head(trainCSw,50)
plot(trainPlot,
     xlab = "Single words (excluding \"stopwords\")",
     ylab = "Frequency in 100k-line sample")
text(1:length(trainPlot), 
     trainPlot, names(trainPlot), cex=0.6, pos=4, col="blue")
as.matrix(trainPlot)

# tokenize
BiGramTokenizer <- function(x) 
        unlist(lapply(ngrams(words(x), 2), 
                      paste, collapse = " "), use.names = FALSE)
TriGramTokenizer <- function(x) 
        unlist(lapply(ngrams(words(x), 3), 
                      paste, collapse = " "), use.names = FALSE)

trainDTMb <- DocumentTermMatrix(trainDocs, 
                                control = list(removePunctuation = TRUE,
                                               tokenize = BiGramTokenizer))
trainCSb  <- colSums(inspect(removeSparseTerms(trainDTMb, 0.999)))

trainDTMt <- DocumentTermMatrix(trainDocs, 
                                control = list(removePunctuation = TRUE,
                                               tokenize = TriGramTokenizer))
trainCSt  <- colSums(inspect(removeSparseTerms(trainDTMt, 0.999)))

# biGrams plot
trainPlot <- head(sort(trainCSb, decreasing = TRUE),50)
plot(trainPlot,
     xlab = "Bigrams",
     ylab = "Frequency in 100k-line sample")
text(1:length(trainPlot), 
     trainPlot, names(trainPlot), cex=0.6, pos=4, col="blue")
as.matrix(trainPlot)

# triGrams plot
trainPlot <- head(sort(trainCSt, decreasing = TRUE),50)
plot(trainPlot[-1],
     xlab = "Trigrams",
     ylab = "Frequency in 100k-line sample")
text(1:length(trainPlot[-1]), 
     trainPlot[-1], names(trainPlot[-1]), cex=0.6, pos=4, col="blue")
as.matrix(trainPlot)

# other exploratory
for (i in 1:20) {
        x[i] <- names(trainCSw)[i]
        y[i] <- length(grep(names(trainCSw)[i], 
                         dimnames(as.matrix(trainCSb))[[1]]))
}
print(data.frame(x,y))

# QUESTIONS TO CONSIDER
#
# 1. Some words are more frequent than others - what are the distributions of word frequencies? 
# 2. What are the frequencies of 2-grams and 3-grams in the dataset? 
# 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# 4. How do you evaluate how many of the words come from foreign languages? 
# 5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

# ANSWERS
# 1. see plots before
# 2. see plots and tables before from the sampled data
# 3. a little over 200 based on the sample beneath
set.seed(42)
trainDTMall <- DocumentTermMatrix(sample(trainDocs,10000),
                                  control = list(removePunctuation = TRUE,
                                                 removeNumbers = TRUE))
trainCSall <- colSums(inspect(trainDTMall))
trainCSall <- as.data.frame(sort(trainCSall, decreasing = TRUE))
names(trainCSall) <- "count"

trainCSall$cumulative <- 0
trainCSall$cumulative[1] <- trainCSall$count[1]
for (i in 2:length(trainCSall$count)) {
        trainCSall$cumulative[i] <- 
                trainCSall$cumulative[i-1] + trainCSall$count[i]
}
for (i in 1:length(trainCSall$count)) {
        trainCSall$cumShare[i] <-
                trainCSall$cumulative[i] / sum(trainCSall$count)
}
trainCSall[210:220,]

# 4. I could check sparse words against the other word sources, though if a foreign word is used frequently I might still want to include it in my prediction algorithm.
# 5. Some words occur often in many different combinations, while others that occur fewer times in total only occur in a few combinations. In terms of space use one would prefer to include the latter words to cover as many phrases as possible.

##############
##          ##
##  QUIZ 2  ##
##          ##
##############

library(dplyr)
library(tm)
library(caret)
library(SnowballC)
library(rJava)
library(RWeka)

language <- "en_US"
lang_S   <- strsplit(language, split = "_")[[1]][1]

# Profanity word list
#      sources for profanity word list:
#      - https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
con <- file("profanity.txt", open = "r")
profanity <- readLines(con)
close(con)

document <- ".twitter.txt"
con <- file(paste(getwd(), "/final/", language, 
                  "/", language, document, sep = ""), open = "r")
twitter <- readLines(con)
close(con)

# turn data into corpus
nL <- 100000

set.seed(1234)
twtVS   <- VectorSource(sample(twitter, nL))
twtDocs <- Corpus(twtVS, readerControl = list(reader = readPlain, 
                                              language = lang_S, 
                                              load = TRUE))

# remove profanity, and for that convert to lower case
cleanup <- function(x) { x <- tm_map(x, content_transformer(tolower));
x <- tm_map(x, removeWords, profanity);
return(x) }

twtDocs <- cleanup(twtDocs)

x <- twtDocs

# tokenize functions
BiGramTokenizer <- function(x) 
        unlist(lapply(ngrams(words(x), 2), 
                      paste, collapse = " "), use.names = FALSE)
TriGramTokenizer <- function(x) 
        unlist(lapply(ngrams(words(x), 3), 
                      paste, collapse = " "), use.names = FALSE)

# unigrams
DTM  <- DocumentTermMatrix(x, 
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          removeNumbers = TRUE))

# bigrams
DTMb <- DocumentTermMatrix(x, 
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          removeNumbers = TRUE,
                                          tokenize = BiGramTokenizer))

# trigrams
DTMt <- DocumentTermMatrix(x, 
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          removeNumbers = TRUE,
                                          tokenize = TriGramTokenizer))

# inputs 1
unigram <- c("case", "bouquet", "bacon")
bigram <- "case of"
trigram <- "a case of"

# inputs 2
unigram <- c("please", "would", "mean")
bigram <- "would mean"
trigram <- "would mean the"

# prediction
findAssocs(DTM, unigram, 0.3)
findAssocs(DTMb, bigram, 0.2)
findAssocs(DTMt, trigram, 0.2)

sink("NUL")
col <- min(which(DTM$dimnames[[2]] == unigram))
CS  <- colSums(inspect(DTM[DTM$i > 1,]))
sink()
head(sort(CS, decreasing = TRUE),5)

sink("NUL")
CS   <- colSums(inspect(DTMb[DTMb$dimnames[[2]] == bigram,]))
sink()
head(sort(CS, decreasing = TRUE),5)

sink("NUL")
CS   <- colSums(inspect(DTMt[DTMt$dimnames[[2]] == trigram,]))
sink()
head(sort(CS, decreasing = TRUE),5)