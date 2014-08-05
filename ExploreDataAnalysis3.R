session1 <-function(){


  set.seed(1234)
  par(mar = c(0,0,0,0))
  x = rnorm (12, mean = rep(1:3, each=4), sd= 0.2)
  y = rnorm (12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
  #plot(x,y, col = "blue", pch= 19, cex=2)
  #text(x + 0.05, y+0.05, labels = as.character(1:12))
  
  df <- data.frame(x=x, y=y)
  distxy <-dist(df)
  
  hClustering <- hclust(distxy)
  
  #myplclust(hClustering, lab=rep(1:3, each = 4), lab.col = rep(1:3, each = 4))
  set.seed(143)
  dataM <- as.matrix(df)[sample(1:12),]
  heatmap(dataM)
  
}
library(datasets)
library(lattice)
library(ggplot2)
library(data.table)
library(nlme)
library(lattice)
library(RColorBrewer)

session3 <- function(){
  
  set.seed(1234)
  par(mar = c(0,0,0,0))
  x = rnorm (12, mean = rep(1:3, each=4), sd= 0.2)
  y = rnorm (12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
  plot(x,y, col = "blue", pch= 19, cex=2)
  text(x + 0.05, y+0.05, labels = as.character(1:12))
  
  df = data.frame(x,y)
  kmobj = kmeans(df, centers = 3)
  names (kmobj)
  kmobj$cluster
  
  par (mar = rep(0.2,4))
  plot(x,y, col= kmobj$cluster, pch=19, cex=2)
  points(kmobj$centers, col = 1:3, pch =3, cex = 3, lwd=3)
  
  dataM = as.matrix (df)[sample(1:12),]
  kmobj2 = kmeans(dataM, centers=3)
  par(mfrow = c(1,2), mar=c(2,3,0.1,0.1))
  image(t(dataM)[, nrow(dataM):1], yaxt = "n")
  image(t(dataM)[, order(kmobj2$cluster)],yaxt = "n")
}

session4 <- function(){
  set.seed(1234)
  par(mar = rep(0.2,4))
  dataM = matrix(rnorm(400),nrow=40)
  #image (1:10, 1:40, t(dataM)[,nrow(dataM):1])
  #par(mar = rep(0.2,4))
  #heatmap(dataM)
  

  set.seed(678910)
  for (i in 1:40){
    #flip a coin
    conFlip = rbinom(1, size =1, prob=0.5)
    # if coin is heads add a common pattern to that row
    if (conFlip){
      dataM[i,] <- dataM[i,] + rep(c(0,3), each = 5)
    }
    
  }
  #par (mar = rep(0.2,4))
  #image(1:10,1:40, t(dataM)[,nrow(dataM):1])
  #par(mar = rep(0.2,4))
  #heatmap(dataM)
  
  hh = hclust(dist(dataM))
  dataMOrder = dataM[hh$order,]
  #par(mfrow= c(1,3))
  #image(t(dataMOrder)[,nrow(dataMOrder):1])
  #plot(rowMeans(dataMOrder), 40:1,, xlab="Row Mean",ylab = "Row", pch=19)
  #plot(colMeans(dataMOrder), xlab="Column", ylab = "Colum Mean", pch=19)
  
  svd1 = svd(scale(dataMOrder))
  par(mfrow = c(1,3))
  image(t(dataMOrder) [,nrow(dataMOrder):1])
  plot(svd1$u[,1], 40:1,, xlab="Row", ylab="First left singular vector", pch = 19)
  plot(svd1$v[,1], xlab = "Column", ylab="First right singular vector", pch=19)
  
  par(mfrow = c(1,2))
  plot(svd1$d, xlab = "Column", ylab="Singular value", pch = 19)
  plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  
  
  svd2 = svd(scale(dataMOrder))
  pca1 = prcomp(dataMOrder, scale=TRUE)
  plot(pca1$rotation[,1], svd2$v[,1], pch=19, xlab="Principal Compnent 1", 
       ylab="Right Signular Vector 1", cex = 0.8)
  abline(c(0,1))
  
  constantM = dataMOrder*0
  
  for (i in 1:dim(dataMOrder)[1]){
    constantM[i,]<- rep(c(0,1), each=5)
  }
  constantM
  
  svd3 = svd(constantM)
  #par(mfrow = c(1,3))
  #image(t(constantM) [,nrow(constantM):1])
  #plot(svd3$d, xlab = "Column", ylab="Singular value", pch = 19)
  #plot(svd3$d^2/sum(svd3$d^2), xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  svd1$u[,1]
  
}


session5 <- function(){
  set.seed(678910)

  dataM = matrix(rnorm(400),nrow=40)
  for (i in 1:40){
    #flip a coin
    conFlip1 = rbinom(1, size =1, prob=0.5)
    conFlip2 = rbinom(1, size =1, prob=0.5)
    
    # if coin is heads add a common pattern to that row
    if (conFlip1){
      dataM[i,] <- dataM[i,] + rep(c(0,5), each = 5)
    }

    if (conFlip2){
      dataM[i,] <- dataM[i,] + rep(c(0,5),5)
    }
    
  }
  
  hh= hclust(dist(dataM))
  dataMOrder = dataM[hh$order,]
  dataMOrder
  svd3 = svd(scale(dataMOrder))
  par(mfrow = c(1,3))
  image(t(dataMOrder) [,nrow(dataMOrder):1])
  plot(svd3$d, xlab = "Column", ylab="Singular value", pch = 19)
  plot(svd3$d^2/sum(svd3$d^2), xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  
  
  #plot(svd3$v[,1], xlab = "Column", ylab="Singular value", pch = 19)
  #plot(svd3$v[,2], xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  
  #plot(rep(c(0,1), each = 5),pch=19, xlab="column", ylab="pattern 1")
  #plot(rep(c(0,1), 5), pch=19, xlab="column", ylab="Pattern 2")
  
}


session6 <- function(){
  set.seed(678910)
  
  dataM = matrix(rnorm(400),nrow=40)
  for (i in 1:40){
    #flip a coin
    conFlip1 = rbinom(1, size =1, prob=0.5)
    conFlip2 = rbinom(1, size =1, prob=0.5)
    
    # if coin is heads add a common pattern to that row
    if (conFlip1){
      dataM[i,] <- dataM[i,] + rep(c(0,5), each = 5)
    }
    
    if (conFlip2){
      dataM[i,] <- dataM[i,] + rep(c(0,5),5)
    }
    
  }
  
  
  hh= hclust(dist(dataM))
  dataMOrder = dataM[hh$order,]
  dataMOrder
  #insert some missing data
  dataMOrder[sample(1:100, size=40, replace = FALSE)]= NA
  #dataMOrder = impute.knn(dataMOrder)$data
  svd3 = svd(scale(dataMOrder))
  par(mfrow = c(1,3))
  image(t(dataMOrder) [,nrow(dataMOrder):1])
  plot(svd3$d, xlab = "Column", ylab="Singular value", pch = 19)
  plot(svd3$d^2/sum(svd3$d^2), xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  
  
  #plot(svd3$v[,1], xlab = "Column", ylab="Singular value", pch = 19)
  #plot(svd3$v[,2], xlab = "Column", ylab="Prop. of variance Explained", pch=19)
  
  #plot(rep(c(0,1), each = 5),pch=19, xlab="column", ylab="pattern 1")
  #plot(rep(c(0,1), 5), pch=19, xlab="column", ylab="Pattern 2")
  
}


session7 <- function(){

  pal = colorRamp(c("red","blue"))
  pal(seq(0,1,len=10))
  
  
  pal = colorRampPalette(c("red","yellow"))
  pal(2)
  pal(10)
  cols<- brewer.pal(3,"BuGn")
  cols
  pal1 <- colorRampPalette(cols)
  #image(volcano, col=pal1(20))
  
  x <- rnorm(10000)
  y <- rnorm(10000)
  smoothScatter(x,y)
  plot(x,y, col=rgb(0,0,0,0.2), pch=19)
  
}




session8 <- function(){
  
  load("data/samsungData.rda")
  names(samsungData)[1:12]
  table(samsungData$activity)
  par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
  samsungData <- transform(samsungData, activity = factor(activity))
  sub1 <- subset(samsungData, subject == 1)
  plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
  plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
  legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), 
         pch = 1)
  
  
  source("myplclust.R")
  distanceMatrix <- dist(sub1[, 1:3])
  hclustering <- hclust(distanceMatrix)
  myplclust(hclustering, lab.col = unclass(sub1$activity))
}
session9<-function(){
  
  ## setwd("~/CourseraModules/04_ExploratoryAnalysis/CaseStudy/pm25_data")
  
  ## Has fine particle pollution in the U.S. decreased from 1999 to
  ## 2012?
  
  ## Read in data from 1999
  
  pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
  dim(pm0)
  head(pm0)
  cnames <- readLines("RD_501_88101_1999-0.txt", 1)
  #print(cnames)
  cnames <- strsplit(cnames, "|", fixed=TRUE)
  #print(cnames)
  names(pm0) = make.names(cnames[[1]])
  head(pm0)
  x0 <- pm0$Sample.Value
  class(x0)
  #str(x0)
  summary(x0)
  mean(is.na(x0))
  
  ## Read in data from 2012
  
  pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
  names(pm1) <- make.names(cnames[[1]])
  head(pm1)
  dim(pm1)
  x1 <- pm1$Sample.Value
  class(x1)
  
  summary(x1)
  summary(x0)
  print(mean(is.na(x1)))
  print(mean(is.na(x0)))
  
  ## Make a boxplot of both 1999 and 2012
  boxplot(x0, x1)
  #boxplot(log10(x0), log10(x1))

  
  ## Check negative values in 'x1'
  summary(x1)
  negative <- x1 < 0
  sum(negative, na.rm = T)
  mean(negative, na.rm = T)
  dates <- pm1$Date
  str(dates)
  dates <- as.Date(as.character(dates), "%Y%m%d")
  str(dates)
  hist(dates, "month") ## Check what's going on in months 1--6
  
  
  ## Find a monitor for New York State that exists in both datasets
  site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
  site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
  site0 <- paste(site0[,1], site0[,2], sep = ".")
  site1 <- paste(site1[,1], site1[,2], sep = ".")
  str(site0)
  str(site1)
  print(site0)
  print(site1)
  
  both <- intersect(site0, site1)
  print(both)
  
  
  
}


project2 <- function(){
  #load("summarySCC_PM25.rds")
  #summarySCC_PM25 <- readRDS("summarySCC_PM25.rds")
  head(NEI);
  
  
}
cloud1 <-function(){
  
  #lineitem1 =read.table("./data/lineitem.tbl", header = FALSE, sep = "|",stringsAsFactors=F)
  lineitem =fread("./data/lineitem.tbl", header = FALSE, sep = "|",stringsAsFactors=FALSE,verbose=FALSE)
  lineitem.sub <- lineitem;
  lineitem.sub$V11 <- strftime(as.POSIXct(as.Date(lineitem.sub$V11)), format="%Y-%m-%d %H:%M:%S", tz="UTC")
  lineitem.sub$V12 <- strftime(as.POSIXct(as.Date(lineitem.sub$V12)), format="%Y-%m-%d %H:%M:%S", tz="UTC")
  lineitem.sub$V13 <- strftime(as.POSIXct(as.Date(lineitem.sub$V13)), format="%Y-%m-%d %H:%M:%S", tz="UTC")
 
  write.table(lineitem.sub,"./data/lineitem_r.tbl", sep = "|", col.names = FALSE, row.names=FALSE,na = " ",quote = FALSE)
}

cloud2 <-function(){
  
  #lineitem1 =read.table("./data/lineitem.tbl", header = FALSE, sep = "|",stringsAsFactors=F)
  orders =fread("./data/orders.tbl", header = FALSE, sep = "|",stringsAsFactors=FALSE,verbose=FALSE)
  orders.sub <- orders;
  #head(orders)
  orders.sub$V5 <- strftime(as.POSIXct(as.Date(orders.sub$V5)), format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  write.table(orders.sub,"./data/orders_r.tbl", sep = "|", col.names = FALSE, row.names=FALSE,na = " ",quote = FALSE)
}

