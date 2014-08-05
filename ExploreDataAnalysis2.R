session1 <-function(){

  #airquality <- transform(airquality, Month=factor(Month))
  #xyplot(Ozone ~ Wind | Month, dat = airquality, layout = c(5,1))
  
  #head(BodyWeight)
  xyplot(weight ~ Time | Diet, BodyWeight)
  
  
}
library(datasets)
library(lattice)
library(ggplot2)
library(data.table)
library(nlme)
library(lattice)
session3 <- function(){
  str(mpg)
  qplot(displ ,hwy, data=mpg, color=drv)
  #qplot(displ ,hwy, data=mpg, geom = c("point", "smooth"))
  
  qplot(hwy, data=mpg, fill=drv)
  #qplot(displ ,hwy, data=mpg, facets=.~drv)
  #qplot(hwy, data=mpg, facets=drv~., binwidth=2)
  
#http://goo.gl/WqE9j8
  #str(maacs)
  

  #data(airquality)
  #p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
  #p
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

