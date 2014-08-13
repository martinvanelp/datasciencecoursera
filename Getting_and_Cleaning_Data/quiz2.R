library(httr)
library(jsonlite)

## QUESTION 1 ##

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
#    Insert your values below - if secret is omitted, it will look it up in
#    the GITHUB_CONSUMER_SECRET environmental variable.
#
#    Use http://localhost:1410 as the callback url
myapp <- oauth_app("github", "a78832712078f18f61d1", NULL)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)

data1 <- content(req)
data2 <- jsonlite::fromJSON(toJSON(data1))

print(cbind(data2$name, data2$created_at))

# OR:
#req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
#stop_for_status(req)
#content(req)

## QUESTION 2 ##

library(sqldf)

file <- "./data/getdata-data-ss06pid.csv"
acs <- read.csv(file)

#head(sqldf("select * from acs where AGEP < 50"))
#tail(sqldf("select pwgtp1 from acs"))
tail(sqldf("select pwgtp1 from acs where AGEP < 50"))
#head(sqldf("select * from acs where AGEP < 50 and pwgtp1"))

unique(acs$AGEP)

#sqldf("select unique AGEP from acs")
sqldf("select distinct AGEP from acs")
#sqldf("select unique * from acs")
#sqldf("select AGEP where unique from acs")

## QUESTION 4 ##

con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(con)
close(con)
nchar(htmlCode[c(10, 20, 30, 100)])

## QUESTION 5 ##

file <- "./data/getdata-wksst8110.for"
fwf <- read.fwf(file, widths = c(10, 9,4, 9,4, 9,4, 9,4), skip = 4)
sum(fwf[,4])
