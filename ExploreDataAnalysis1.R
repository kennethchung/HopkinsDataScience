session2 <-function(){
  pollution <-read.csv("data/household_power_consumption.txt", colclasses=c("numeric","character","factor","numeric","numeric"))
  head(pollution)
  #summary(pollution$pm25)
  #boxplot(pulltion$pm25,col="blue")
  #abline(h=12)
  #hist(pulltion$pm25,col="blue", breaks = 100)
  #rug(pulltion$pm25)
  #abline(v=12, lwd=2)
  #abline(v= median(pollution$pm25),col = "megenta", lwd=4)
  #barplot(table(pollution$region), col = "wheat", main = "Title")
  
  
  #boxplot(pm25 ~ region, data = pollution, col="red")
  
  #par (mfrow = (c2,1), mar=c(4,4,2,1))
  #hist(subset(pollution, region =="east")$pm25, col="green")
  #hist(subset(pollution, region =="west")$pm25, col="green")
  
  #with(pollution, plot(latitude, pm25))
  #abline(h=12, lwd=2, lty=2)
  
  #par(mfrow = c(1,2), mar=c(5,4,2,1))
  #with(subset(pollution, region =="west"), plot(latitude, pm25, main="West"))
  #with(subset(pollution, region =="west"), plot(latitude, pm25, main="West"))
  
}
library(datasets)
library(lattice)
library(ggplot2)
library(data.table)
session3 <- function(){
  data(cars)
  #with(cars, plot(speed,dist))
  data(mpg)
  qplot(displ, hwy,data=mpg)
  
}

session4 <- function(){

  #hist(airquality$Ozone)
  
  #with(airquality, plot(Wind, Ozone))
  #airquality <-transform(airquality, Month=factor(Month))
  #boxplot(Ozone ~ Month, airquality, xlab="Month", ylab="Ozone (ppb)")
  
  #pch, lty, lwd, col, xlab, ylab
  #par
  # las, bg, mar, oma, mfrow, mfcol
  
  par("lty")
  par("mfcol")
  
  #plot, lines, text, title, mtext, axis
  
  #with(airquality, plot(Wind, Ozone))
  #title(main="Title")
  
  with(airquality, plot(Wind, Ozone, main="Ozone Title", type="n"))
  with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
  with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
  legend("topright", pch=1, col=c("blue","red"),  c("May", "Other Months"))
  
  
  #with(airquality, plot(Wind, Ozone, main="Ozone Title", pch=20))
  #model <- lm (Ozone ~ Wind, airquality)
  #abline(model, lwd = 2)
  
  #par (mfrow = c(1,2))
  #with(airquality, {
  #     plot(Wind, Ozone, main="Ozone Title")
  #     plot(Solar.R, Ozone, main="Solar Title")
  #     mtext("Outer title", outer = TRUE)
  #})
  # pdf(file = "myplolt.pdf")
  #dev.off()
  #dev.set(1)
  #dev.cur()
  #dev.copy(png, file="fille.png)
  #dev.off()
  #example(pch)
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

