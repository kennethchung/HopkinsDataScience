library(httr)
library(httpuv)
library(jsonlite)
library(sqldf)
read_github <- function(){
    # 1. Find OAuth settings for github:
    # http://developer.github.com/v3/oauth/
    oauth_endpoints("github")
    
    # 2. Register an application at https://github.com/settings/applications
    # Insert your values below - if secret is omitted, it will look it up in
    # the GITHUB_CONSUMER_SECRET environmental variable.
    #
    # Use http://localhost:1410 as the callback url
    myapp <- oauth_app("github", "5ba5cf4e2237292c19a7", secret="c8a72517941639f1b9039f4a297007368c78cf8f")
    
    # 3. Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
    
    # 4. Use API
    req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
    stop_for_status(req)
    content(req)
    json1 = content(req)
    json2 = jsonlite::fromJSON(toJSON(json1))
    json2[json2$name=='datasharing','created_at']
}

read_ffs06<- function(){
  
  filename <- "./data/getdata_data_ss06pid.csv"
  acs <- read.table(filename, sep=",", header= TRUE);
  result<-sqldf("select pwgtp1 from acs where AGEP < 50")
  result<-sqldf("select distinct AGEP from acs")
  unique(acs$AGEP)
}


read_htmlline<- function(){
  
  url = "http://biostat.jhsph.edu/~jleek/contact.html"
  html2=readLines(url)
  html2
  print(nchar(html2[10]))
  print(nchar(html2[20]))
  print(nchar(html2[30]))
  print(nchar(html2[100]))
  
  #content2 = content(html2,as="text")

  
}

read_for<- function(){
  
  #url = "http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"
  #download.file(url, destfile = "./data/wksst8110.for")
  #list.files("./data")
  #dateDownloaded <-date()
  #dateDownloaded
  #x <- read.fwf(file=url("http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"),skip=4,widths=c(12, 7,4, 9,4, 9,4, 9,4))
  x <- read.fwf(file=url("https://d396qusza40orc.cloudfront.net/getdata/wksst8110.for"),skip=4,widths=c(12,7,4,9,4,9,4,9,4))
  
 sum(x$V4)
  
}