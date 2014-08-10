# Getting and Cleaning Data : Quiz 1

# question 1
if (!file.exists("data")) { dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(fileUrl, destfile = "./data/housing.csv")

csv <- read.csv("./data/housing.csv")

data <- csv[!is.na(csv$VAL), ]

print(nrow(data[data$VAL == 24, ]))

# question 3
if (!file.exists("data")) { dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

download.file(fileUrl, destfile = "./data/gas.xlsx", mode="wb")

library(xlsx)

colIndex <- 7:15
rowIndex <- 18:23
        
xlsx <- read.xlsx("./data/gas.xlsx", sheetIndex = 1, header = TRUE,
                  colIndex = colIndex, rowIndex = rowIndex)

dat <- xlsx

print(sum(dat$Zip*dat$Ext,na.rm=T))

# question 4
if (!file.exists("data")) { dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

download.file(fileUrl, destfile = "./data/restaurants.xml")

library(XML)

xml <- xmlTreeParse("./data/restaurants.xml", useInternal = TRUE)
rootNode <- xmlRoot(xml)

zip <- xpathSApply(rootNode, "//zipcode", xmlValue)

print(length(zip[zip == 21231]))

# question 5
if (!file.exists("data")) { dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

download.file(fileUrl, destfile = "./data/housing2.csv")

library(data.table)

DT <- fread("./data/housing2.csv")

# check which work
print(mean(DT$pwgtp15,by=DT$SEX))

print(sapply(split(DT$pwgtp15,DT$SEX),mean))
      
print(DT[,mean(pwgtp15),by=SEX])
      
print(tapply(DT$pwgtp15,DT$SEX,mean))
      
print(mean(DT[DT$SEX==1,]$pwgtp15))
print(mean(DT[DT$SEX==2,]$pwgtp15))
      
print(rowMeans(DT)[DT$SEX==1])
print(rowMeans(DT)[DT$SEX==2])

# check duration of working formula
print(system.time(
        for (i in 1:1000) {
                sapply(split(DT$pwgtp15,DT$SEX),mean)
        }
        )
      )

print(system.time(
        for (i in 1:1000) {
                DT[,mean(pwgtp15),by=SEX]
        }
        )
      )

print(system.time(
        for (i in 1:1000) {
                tapply(DT$pwgtp15,DT$SEX,mean)
        }
        )
      )

print(system.time(
        for (i in 1:1000) {
                mean(DT[DT$SEX==1,]$pwgtp15)
                mean(DT[DT$SEX==2,]$pwgtp15)
        }
        )
      )
