library(vioplot)
#library(RJDBC)
#library(TTR)
library(xtable)
library(rhdf5)
library(XML)
library(httr)
library(jsonlite)
library(sqldf)
library(RJSONIO)
library(RCurl)

read_db <- function(){
  drv <- JDBC("com.mysql.jdbc.Driver",
              "D:/Apps/MySQL/MySQL Server 5.5/mysql-connector-java-5.1.27/mysql-connector-java-5.1.27-bin.jar",
              identifier.quote="`")
  
  hg19 <- dbConnect(drv, user="genome", url="jdbc:mysql://genome-mysql.cse.ucsc.edu/hg19")
  result <- dbGetQuery(hg19,"show databases"); 
  allTables <- dbListTables(hg19)
  allFields <- dbListFields(hg19,"affyU133Plus2")
  count <- dbGetQuery(hg19, "select count(*) from affyU133Plus2")
  
  affyData <- dbReadTable(hg19,"affyU133Plus2")
  affyMis <- dbGetQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
  #affyMis <- fetch(query); 
  qt<- quantile(affyMis$misMatches)
  
  
  dbDisconnect(hg19)
  length(allTables)
  allTables[1:5]
  print(allFields)
  print(count)
  #head(affyData)
  qt
}

rhdf5_f <-function(){
  if (file.exists("example.h5")){
    file.remove("example.h5")
  } 
  
  created = h5createFile("example.h5")
  created = h5createGroup("example.h5","foo")
  created = h5createGroup("example.h5","baa")
  created = h5createGroup("example.h5","foo/foobaa")
  h5ls("example.h5")
  
  A = matrix(1:10, nr=5, nc=2)
  h5write(A, "example.h5","foo/A")
  B = array(seq(0.1,2.0, by=0.1), dim=c(5,2,2))
  attr(B, "scale") <- "Liter"
  h5write(B, "example.h5", "foo/foobaa/B")
  h5ls("example.h5")
  df = data.frame(1L:5L, seq(0,1,length.out=5), c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
  h5write(df,"example.h5","df")
  h5ls("example.h5")
  
  readA = h5read("example.h5","foo/A")
  readB = h5read("example.h5","foo/foobaa/B")
  readC = h5read("example.h5","df")
  readC
  
  h5write(c(12,13,14),"example.h5","foo/A", index=list(1:3,1))
  readD<-h5read("example.h5","foo/A",index=list(1:3,1))
  readD

}

read_web <- function(){
  link = "http://scholar.google.com/citations?user=HI-I6C0AAAAJ"
  con = url(link)
  #htmlCode=readLines(con)
  html = htmlTreeParse(link, useInternalNodes=1)
  close(con)
  #xpathSApply(html,"//title",xmlValue)
   xpathSApply(html,"//td[@id='col-citedby']",xmlValue)

}
  

read_web2 <- function(){
  url = "http://scholar.google.com/citations?user=HI-I6C0AAAAJ"
  html2=GET(url)
  content2 = content(html2,as="text")
  parsedHTML = htmlParse(content2,asText=TRUE)
  xpathSApply(parsedHTML, "//title", xmlValue)
  
  url1 = "http://httpbin.org/basic-auth/user/passwd"
  pg2 = GET(url1, authenticate("user", "passwd"))
  pg2
  names(pg2)
  
}

read_web3 <- function(){
  google = handle("http://google.com")
  pg1 = GET(handle=google, path="/")
  pg2 = GET(handle=google, path="search")
  pg2
  
}

read_api <- function(){
  google = handle("http://google.com")
  pg1 = GET(handle=google, path="/")
  pg2 = GET(handle=google, path="search")
  pg2
  
}

read_twitter <- function(){
  
  myapp = oauth_app("twitter",
                    key="hCX9zsRik0E9xEaFkX0hXPOSA",
                    secret="pp0w6NWZjkMY7ECqAZVD3itAGaoM4UIGk07emnDmMm0Qo3vY9A")
  sig = sign_oauth1.0(myapp,
                      token="2400820903-FHjZXqKAHVaBEev4ogxqrKImFFHShmiGGXQZM9R",
                      token_secret="FHkXo3eeJ2kIjHeUYhJec6h1BbNnfFQeoEGWR43NJsz8A")
  homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
  json1 = content(homeTL)
  json2 = jsonlite::fromJSON(toJSON(json1))
 
  write.csv(json2[1,1:4],"Final.csv")

}


convertJson <- function(){
  myapp = oauth_app("twitter",
                    key="hCX9zsRik0E9xEaFkX0hXPOSA",
                    secret="pp0w6NWZjkMY7ECqAZVD3itAGaoM4UIGk07emnDmMm0Qo3vY9A")
  sig = sign_oauth1.0(myapp,
                      token="2400820903-FHjZXqKAHVaBEev4ogxqrKImFFHShmiGGXQZM9R",
                      token_secret="FHkXo3eeJ2kIjHeUYhJec6h1BbNnfFQeoEGWR43NJsz8A")
  homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
  data <- fromJSON(homeTL)
}
