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
# 3. project Gutenberg
# 4. make your text machine readable, make the content quantifiable
# 5. typos, synonyms, stemming
# 6. NLP is a specific form of data science, applied to text

library(tm)

# first try to "simply" load the data and apply some {tm}-functions
doc    <- readLines("final/en_US/en_US.twitter.txt")
vs     <- VectorSource(text)
elem   <- getElem(stepNext(vs))
result <- readPlain(elem, "en", "id1")
meta(result)
