library(xlsx)
library(jsonlite)
library(data.table)
tba1_1<-function(){
  
  if (!file.exists("data")){
    dir.create("data")
    
  }


  fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata/data/ss06hid.csv"
  #download.file(fileUrl, destfile = "./data/Fss06hid.csv")
  #list.files("./data")
  #dateDownloaded <-date()
  #dateDownloaded
  df <- read.table("./data/getdata_data_ss06hid.csv", sep=",", header= TRUE);
  df_val<-table(df$VAL==24)
  #df_val
  df_fes<-unique(df$FES)
  df_fes
}

tba1_2<-function(){
  colIndex <- 7:15
  rowIndex <- 18:23
  dat <- read.xlsx("./data/getdata_data_DATA.gov_NGAP.xlsx", sheetIndex=1, header= TRUE, colIndex=colIndex, rowIndex = rowIndex);
  sum(dat$Zip*dat$Ext,na.rm=T) 
  #write.xlsx
}

tba1_3<-function(){
  colIndex <- 2:3
  rowIndex <- 1:4
  
  #fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata/data/restaurants.xml"
  #download.file(fileUrl, destfile = "./data/restaurants.xml")
  #list.files("./data")
  doc <- xmlTreeParse("./data/restaurants.xml", useInternal=TRUE)
  rootNode <- xmlRoot(doc)
  xmlName(rootNode)
  names(rootNode)
  rootNode[[1]][[1]]
  ##xmlSApply(rootNode, xmlValue)
  v_zipcode<- xpathSApply(rootNode,"//zipcode",xmlValue)
  a <- table(v_zipcode)
  zipcode_count <- a[names(a) == 21231]
  zipcode_count
  #df
  #write.xlsx
}



tba1_4<-function(){
  colIndex <- 2:3
  rowIndex <- 1:4
  
  doc<- htmlTreeParse("./data/baltimore.xml", useInternal=TRUE)
  scores <- xpathSApply (doc,"//li[@class='score']", xmlValue)
  teams <- xpathSApply (doc,"//li[@class='team-name']", xmlValue)
  teams
}


tba1_5<-function(){

  fileUrl <- "https://api.github.com/users/jtleek/repos"
  jsonData<- fromJSON("./data/data.json")
  #download.file(fileUrl, destfile = "./data/data.json")
  names(jsonData)
  names(jsonData$owner)
  jsonData$owner$login
  
  myjson <- toJSON(iris, pretty=TRUE)
  cat(myjson)
  
  iris2 <- fromJSON(myjson)
  head(iris2)
}


tba1_6<-function(){
  
  DT = data.frame(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9))
  head(DT,3)
  DT[2,] # subset rows
  DT[DT$y=="a",]
  
  DT[c(2,3)] 
  
  #subset col
  DT[,c(2,3)]
  
  #DT [, list<mean(x,sum(z))]
  
  #DT[, table(y)]
  #DT[, w:=z^2]
  
  #multipe operations
  #DT[, m:= {tmp <- (x+z); log2(tmp+5)}]
  #plyr
  #DT[,a:=x>0]
  DT1 <- data.table(x=rep(c("a", "b", "c"), each=100),y= rnorm(300))
  set(DT1,x)
  DT1['a']
  
  
  
}



tba1_7<-function(){
  

  #DT[,a:=x>0]
  DT1 <- data.table(x=rep(c("a", "b", "c"), each=100),y= rnorm(300))
  setkey(DT1,x)
  DT1['a']
  
}

tba1_8<-function(){
  
  
  #DT[,a:=x>0]
  DT1 <- data.table(x=c("a", "a", "b", "dt1"), y=1:4)
  DT2 <- data.table(x=c("a", "b", "dt2"), z=5:7)
  
  setkey(DT1,x); setkey(DT2,x)
  result <-merge(DT1, DT2)
  result
}

tba1_9<-function(){
  big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
  file <- tempfile()
  write.table(big_df, file=file, row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
  system.time(fread(file))
  
  #system.time(read.table(file, head=TRUE, sep="\t"))
}


tba1_10<-function(){
  
  file<-"./data/getdata_data_ss06pid.csv"
  #file <- "https://d396qusza40orc.cloudfront.net/getdata/data/ss06pid.csv ";
 
  #system.time(DT<-read.csv(file))
  #DT<-read.csv(file)
  #DT_1 <-DT[,mean(pwgtp15), by = SEX]
  
  ptm <- proc.time()
  system.time(DT<-fread(file))
  DT_1<-tapply(DT$pwgtp15,DT$SEX,mean)
  print(proc.time() - ptm)
  #DT_1<-tapply(DT$pwgtp15,DT$SEX, function (x) mean(x))
  
  ptm <- proc.time()
  system.time(DT<-fread(file))
  DT_1<- mean(DT$pwgtp15,by=DT$SEX)
  print(proc.time() - ptm)
  
  
  ptm <- proc.time()
  system.time(DT<-fread(file))
  DT_1 <-sapply(split(DT$pwgtp15,DT$SEX),mean)
  print(proc.time() - ptm)
  
  ptm <- proc.time()
  system.time(DT<-fread(file))
  DT_1 <-mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
  print(proc.time() - ptm)
  
  #ptm <- proc.time()
  #rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
  #proc.time() - ptm
  

  #DT_1
}