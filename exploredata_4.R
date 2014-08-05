
library(plyr)
library(Hmisc)
library(reshape2)
library(jpeg) 
library(stringr)
library(quantmod)

session1 <- function(){
  
  if (!file.exists("./data")){
    dir.create("./data")
  }
  #fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
  #download.file(fileUrl,destfile="./data/cameras.csv")
  cameraData <- read.csv("./data/cameras.csv")
  
  names(cameraData)
  tolower(names(cameraData))
  splitNames = strsplit(names(cameraData),"\\.")
  splitNames[[6]]
  mylist <- list(letters=c("A","B","C"), numbers = 1:3, matrix(1:25, ncol=5))
  head(mylist)
  firstElement <- function (x){x[1]}
  sapply(splitNames, firstElement)
  
  reviews <- read.csv("./data/reviews.csv")
  solutions <- read.csv("./data/solutions.csv")
  head(reviews,2)
  sub("_","",names(reviews),)
  
  testName <- "this_is_a_test"
  gsub("_","",testName)
  
  grep("Alameda", cameraData$intersection)
  
  table(  grepl("Alameda", cameraData$intersection))
  
  cameraData2 <-  cameraData[!grepl("Alameda", cameraData$intersection),]
  cameraData2
  grep("Alameda", cameraData$intersection, value=TRUE)
  grep("JeffStreet", cameraData$intersection)
  nchar("Jeffrey Leek")
  
  paste0("Jeffery","Leek")
  
}

question1 <- function(){
  data <- read.csv("./data/getdata_data_ss06hid.csv")
  splitNames = strsplit(names(data),"\\wgtp")
  splitNames
}

question2 <- function(){
  data <- read.csv("./data/getdata_data_GDP1.csv",header = FALSE,stringsAsFactors=F)
 
  gdpincome_v <- data$V4
  gdpincome_v1 <- as.numeric(gsub(",","", gdpincome_v),na.rm=TRUE)
  
  gdpincome_v1
  #v<-c(4,5,6)
  #result<-mean( gdpincome_v1,na.rm=TRUE)
  #result
}

question3 <- function(){
  data <- read.csv("./data/getdata_data_GDP1.csv",header = FALSE,stringsAsFactors=F)

  countryName <-  data[grepl("^United", data$V3),]
  countryName
}

question4 <- function(){
  country <- read.csv("./data/getdata_data_EDSTATS_Country.csv",stringsAsFactors=F)
  gdp <- read.csv("./data/getdata_data_GDP1.csv",header = FALSE,stringsAsFactors=F)
  
  names(gdp)
  
  #gdp.subset <-gdp[c(5:236),c(1,2,4)]
  gdp.subset <- gdp 
  colnames(gdp.subset) <- c('CountryCode','Rank', "ShortName","NotUse")
  gdp.subset$Rank <- as.numeric(gdp.subset$Rank)
  #print(gdp.subset)
  #print(nrow(gdp))
  
  mergedData = merge(country,  gdp.subset, by="CountryCode")
  #print(mergedData[,"CountryCode"])
  #print(nrow(mergedData))
  mergedData = mergedData[complete.cases(mergedData[,"CountryCode"], mergedData[,"Special.Notes"]),]
  mergedData[,"Special.Notes"]
  
  result <-  grepl("^Fiscal year end: June", mergedData$Special.Notes)
  table(result)
  
}
question5 <- function(){

  amzn = getSymbols("AMZN",auto.assign=FALSE,from= '2012-1-01', to ='2012-12-31')
  table(weekdays(index(amzn)))
  #print(nrow(amzn))
  #rownames(amzn)
  #sampleTimes = index(amzn) 
  #head(sampleTimes)
  #result<-grep("^2012", sampleTimes)

  #t<-table(amzn)

  #count(result)
  #head(amzn)
}
session2<-function(){

  set.seed(13435)
  x <- data.frame("var1"=sample(1:5), "var2"=sample(6:10),"var3"=sample(11:15))
  x <- x[sample(1:5),]; 
  x$var2[c(1,3)] = NA;
  x
  x[,1]
  x[,"var1"]
  x[1:2,"var1"]
  x[(x$var1<=3 & x$var3>11),]
  x[(x$var1<=3 | x$var3>11),]
  x[which(x$var2 > 8),]
  sort(x$var1)
  sort(x$var1, decreasing=TRUE)
  sort(x$var2, na.last=TRUE)
  x[order(x$var1),]
  x[order(x$var1,x$var3),]
  arrange(x, var1)
  arrange(x, desc(var1))
  x$var4 <-rnorm(5)
  x
  x<- cbind(x,rnorm(5))
  x
  #^i thnk
  #morning$
  #[Bb][Uu][Ss][Hh]
  #[Ii] am
  #^[0-9][a-zA-Z]
  #[^?.]$
}




session3 <- function()
{
  #9.11 any matching single character
  #flood|fire or
  #^([Gglood][Bb]ad)
  #[Gg]eorge( [Ww]\.)? [Bb]ush, \. don't consider .  meta character
  #(.*)
  #[0-9]+ (.*)[0-9]+
  #[Bb]ush( +[^ ]+ +){1,5} debates}
  # +([a-zA-Z]+) +\1 +
}

session4 <- function(){
  d1= date()
  d1
  d2 = Sys.Date()
  d2
  d3 = format(d2,"%a %b %d")
  d3
}
session5 <- function(){
  if (!file.exists("./data")){
    dir.create("./data")
  }
  fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
  fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
  
  download.file(fileUrl1,destfile="./data/reviews.csv")
  download.file(fileUrl2,destfile="./data/solutions.csv")
  
  reviews <- read.csv("./data/reviews.csv")
  solutions <- read.csv("./data/solutions.csv")
  head(reviews,2)
  print(names(reviews))
  print(names(solutions))
  
  mergedData = merge(reviews, solutions, by.x="solution_id", by.y="id",all=TRUE)
  head(mergedData)
  intersect(names(solutions), names(reviews))
  
  
  df1 = data.frame(id=sample(1:10), x=rnorm(10))
  df2 = data.frame(id=sample(1:10), y=rnorm(10))
  arrange(join(df1,df2),id)
  
  df3 = data.frame(id=sample(1:10), z=rnorm(10))
  dfList = list(df1,df2,df3)
  join_all(dfList)
  
}



question21 <- function(){
  pic<-readJPEG("./data/getdata_jeff.jpg", native = TRUE)
  picPre=cut2(pic,g=10)
  table(picPre)
}

question31 <- function(){
  country <- read.csv("./data/getdata_data_EDSTATS_Country.csv",stringsAsFactors=F)
  gdp <- read.csv("./data/getdata_data_GDP1.csv",header = FALSE,stringsAsFactors=F)

  names(gdp)

  #gdp.subset <-gdp[c(5:236),c(1,2,4)]
  gdp.subset <- gdp 
  colnames(gdp.subset) <- c('CountryCode','Rank', "ShortName","NotUse")
  gdp.subset$Rank <- as.numeric(gdp.subset$Rank)
  #print(gdp.subset)
  print(nrow(gdp))

  mergedData = merge(country,  gdp.subset, by="CountryCode")
  print(mergedData[,"CountryCode"])
  print(nrow(mergedData))
  mergedData = mergedData[complete.cases(mergedData[,"CountryCode"], mergedData[,"Rank"]),]
  #mergedData[,c('CountryCode','Rank', "ShortName","Income.Group")]
  #head(mergedData)
 
  #mergedData
  #mergedData.subset <- mergedData[,c("CountryCode",'Rank',"ShortName","Income.Group")]
  #mergedData.subset
  #nrow(mergedData.subset)
  #mergedData.subset
  #mergedData.subset.sorted <-  mergedData.subset[order(mergedData.subset$Rank),]
  #mergedData.subset.sorted <- arrange(mergedData.subset, desc(Rank))
  #print(nrow(mergedData.subset.sorted))
  #print(mergedData.subset.sorted)


  #highIncome.OECD <-  mergedData.subset.sorted[ which( mergedData.subset.sorted$Income.Group=='High income: OECD'), ]
  #highIncome.OECD
  #mean(highIncome.OECD[,c("Rank")])
  
  #highIncome.NoOECD <-  mergedData.subset.sorted[ which( mergedData.subset.sorted$Income.Group=='High income: nonOECD'), ]
  #highIncome.NoOECD
  #mean(highIncome.NoOECD[,c("Rank")])
  
  #mergedData.subset.sorted$IG = cut2(mergedData.subset.sorted$Rank,g=5)
  
  #mergedData.subset.sorted
  #table(mergedData.subset.sorted$IG,mergedData.subset.sorted$Income.Group =="Lower middle income")
}


