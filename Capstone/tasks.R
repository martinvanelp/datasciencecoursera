# NOTES
# Packages to consider: Rweka (for Ngrams), RTextTools (for Analysis)
# Other word sources: Project Gutenberg, Wordnet

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
library(openNLP)
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
doc    <- readLines(con, n = 2000)
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

# first steps
trainDTM  <- DocumentTermMatrix(trainDocs)
trainCS   <- colSums(inspect(removeSparseTerms(trainDTM, 0.95)))
sort(trainCS, decreasing = TRUE)

# check profanity presence
sort(colSums(inspect(DocumentTermMatrix(trainDocs, list(dictionary = profanity)))))

# tokenize
trainTDM1 <- TermDocumentMatrix(trainDocs, 
                               control = list(tokenize = NGramTokenizer))
trainTDM2 <- TermDocumentMatrix(trainDocs, 
                               control = list(tokenize = tokenize))
