
library(plyr)
library(Hmisc)
library(reshape2)
library(jpeg) 
session1<-function(){

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
  
}


session2 <- function(){
  
  if (!file.exists("./data")){
    dir.create("./data")
  }
  fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
  download.file(fileUrl,destfile="./data/restaurants.csv")
  restData <- read.csv("./data/restaurants.csv")
  
  head(restData, n=3)
  tail(restData, n=3)
  
  #summary(restData)
  #str(restData)
  quantile(restData$councilDistricts, na.rm=TRUE)
  table(restData$zipCode, useNA="ifany")
  table(restData$councilDistrict, restData$zipCode);
  sum(is.na(restData$councilDistrict))
  any(is.na(restData$councilDistrict))
  all(restData$zipCode>0)
  
  colSums(is.na(restData))
  all(colSums(is.na(restData))==0)
  
  table(restData$zipCode %in% c("21212"))
  table(restData$zipCode %in% c("21212","21213"))
  
  restData[restData$zipCode %in% c("21212","21213"),]
  data(UCBAdmissions)
  DF = as.data.frame(UCBAdmissions)
  summary(DF)
  
  xt <- xtabs(Freq ~ Gender + Admit, data=DF)
  xt
  warpbreaks$replicate <-rep (1:9, len=54)
  xt = xtabs(breaks ~., data=warpbreaks)
  xt
  ftable(xt)
  fakeData = rnorm(1e5)
  object.size(fakeData)
  print(object.size(fakeData), units="Mb")
}

session3 <- function()
{
  if (!file.exists("./data")){
    dir.create("./data")
  }
  fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
  download.file(fileUrl,destfile="./data/restaurants.csv")
  restData <- read.csv("./data/restaurants.csv")
  
  s1 <-seq(1,10,by=2)
  s1
  s1 <-seq(1,10, length=3)
  s1
  s1 <- c(1,3,8,25,100)
  seq(along = s1)
  restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")
  table(restData$nearMe)
  
  restData$zipWrong = ifelse(restData$zipCode<0, TRUE, FALSE)
  table(restData$zipWrong, restData$zipCode <0)
  
  restData$zipGroups = cut(restData$zipCode, breaks=quantile(restData$zipCode))
  table(restData$zipGroups)
  #table(restData$zipGroups,  restData$zipCode)
  #restData$zipGroups = cut2(restData$zipCode,g=4)
  #table(restData$zipGroups)
  
  # factor variable
  restData$zcf <- factor(restData$zipCode)
  restData$zcf[1:10]
  
  yesno <- sample(c("yes","no"), size=10, replace=TRUE)
  yesnofac = factor(yesno, levels=c("yes","no"))
  relevel(yesnofac, ref="yes")
  
  restData2 = mutate(restData,zipGroups=cut2(zipCode,g=4))
  table(restData2$zipGroups)
  
}

session4 <- function(){
  head(mtcars)
  mtcars$carname <- rownames(mtcars)
  carMelt <- melt(mtcars, id=c("carname","gear","cyl"), measure.vars=c("mpg","hp"))
  head(carMelt)
  tail(carMelt)
  
  cylData = dcast(carMelt, cyl ~ variable)
  cylData
  cylData = dcast(carMelt, cyl ~ variable, mean)
  cylData
  
  head(InsectSprays)
  tapply(InsectSprays$count, InsectSprays$spray, sum)
  spIns = split(InsectSprays$count, InsectSprays$spray)
  spIns
  sprCount = lapply(spIns,sum)
  sprCount
  unlist(sprCount)
  sapply(spIns, sum)
  #sprCount <- ddply(InsectSprays,.(spray),summarize,sum=sum(count))
  #sprCount
  
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

question1 <- function(){
  data <- read.table("./data/getdata_data_ss06hid.csv", header = TRUE, sep = ",")
  names(data)
  agricultureLogical <- data$ACR==3 & data$AGS==6
  whichArg = which(agricultureLogical)
  whichArg[1:3]
}


question2 <- function(){
  pic<-readJPEG("./data/getdata_jeff.jpg", native = TRUE)
  picPre=cut2(pic,g=10)
  table(picPre)
}

question3 <- function(){
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


#question2 <- function(){
#  data <- read.table("./data/getdata_data_ss06hid.csv", header = TRUE, sep = ",")
  #pic<-readJPEG("./data/getdata_jeff.jpg", native = TRUE)
  #pic<-readJPEG(system.file("img", "./data/getdata_jeff.jpg", package="jpeg"), TRUE)
  #picPre=cut2(pic,g=10)
#}

