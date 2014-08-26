## Question 1
#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/communities.csv")

data <- read.csv("./data/communities.csv")

#  analysis
split <- strsplit(names(data), "wgtp")
print(split[123])

## Question 2-3
#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile = "./data/GDP.csv", mode="w")

gdpData <- read.csv("./data/GDP.csv", skip = 5, header = FALSE,
                    nrows = 190,
                    col.names = c("ABR", "Rank", "", "Country", "GDP",
                                  rep("", 5)))

#  analysis
print(mean(as.numeric(gsub(",", "", gdpData$GDP))))

print(grep("^United", gdpData$Country, value = TRUE))

## Question 4
#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl, destfile = "./data/education.csv", mode="w")

eduData <- read.csv("./data/education.csv")

#  analysis
gdpData$GDP <- as.numeric(gsub(',', '', gdpData$GDP))
gdpData$Rank <- as.numeric(gdpData$Rank)
gdpData <- gdpData[!is.na(gdpData$Rank), ]

mergeData <- merge(gdpData, eduData,
                   by.x = "ABR", by.y = "CountryCode", all = FALSE)

print(length(grep("^Fiscal year end: June*", mergeData$Special.Notes)))

## Question 5
#  fetch data
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

#  analysis
library(lubridate)
print(length(grep("2012", ymd(index(amzn)))))
print(length(grep(2, wday(ymd(index(amzn[grep("2012", ymd(index(amzn)))]))))))
