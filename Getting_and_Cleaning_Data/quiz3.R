## Question 1
#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/communities.csv")

data <- read.csv("./data/communities.csv")

#  analysis
agricultureLogical <- (data$ACR == 3 & data$AGS == 6)
print(which(agricultureLogical))

## Question 2
library(jpeg)

#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, destfile = "./data/jeff.jpg", mode="wb")

jpeg <- readJPEG("./data/jeff.jpg", native = TRUE)

# analysis
print(quantile(jpeg, c(0.30, 0.80)))

## Question 3-5
#  fetch data
if (!file.exists("data")) { dir.create("data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile = "./data/GDP.csv", mode="w")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl, destfile = "./data/education.csv", mode="w")

gdpData <- read.csv("./data/GDP.csv", skip = 5, header = FALSE,
                    nrows = 190,
                    col.names = c("ABR", "Rank", "", "Country", "GDP",
                                  rep("", 5)))
eduData <- read.csv("./data/education.csv")

# analysis
gdpData$GDP <- as.numeric(gsub(',', '', gdpData$GDP))
gdpData$Rank <- as.numeric(gdpData$Rank)
gdpData <- gdpData[!is.na(gdpData$Rank), ]

mergeData <- merge(gdpData, eduData,
                   by.x = "ABR", by.y = "CountryCode", all = FALSE)

mergeData <- mergeData[order(mergeData$GDP), ]
print(mergeData[13, 4])

print(mean(mergeData$Rank[mergeData$Income.Group
                          == "High income: OECD"]))
print(mean(mergeData$Rank[mergeData$Income.Group
                          == "High income: nonOECD"]))

gdpQuantiles <- quantile(mergeData$GDP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
mergeData$GDPgroup <- cut(mergeData$GDP, breaks = gdpQuantiles,
                        labels = FALSE)

print(table(mergeData$GDPgroup, mergeData$Income.Group))
