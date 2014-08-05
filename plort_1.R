library(data.table)
library(vioplot)
library(aplpack)
library(datasets)
library(lattice)
library(ggplot2)
library(nlme)
library(lattice)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(plyr)
plot1 <- function(){
  
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  dataset <-tapply(NEI$Emissions/1000, NEI$year, sum)
  barplot(dataset,xlab="Year", ylab=expression('PM'[2.5]*' in 1000'),
                 main="US Emission from All Sources",col=c("darkblue","red", "gold", "blue"))


  dev.copy(png, file="plot1.png")
  dev.off()
}

plot2 <- function(){
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  NEI.baltimore <- NEI[which(NEI$fips=='24510'),] 
  dataset <-tapply(NEI.baltimore$Emissions, NEI.baltimore$year, sum)
  
  barplot(dataset,xlab="Year", ylab=expression('PM'[2.5]),
          main="Baltimore Emission from All Sources",col=c("darkblue","red", "gold", "blue"))

  #dev.copy(png, file="plot2.png")
  #dev.off()

}

plot3 <- function(){
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  NEI.baltimore <- NEI[which(NEI$fips=='24510'),] 
  NEI.baltimore$year = as.character(NEI.baltimore$year)

  qp<- qplot(year, Emissions, data=NEI.baltimore, stat="summary", fun.y="sum",facets= .~type, geom="bar", fill=year)

  qp + ggtitle("Baltimore Emission from all sources by types")
  #dev.copy(png, file="plot3.png")
  #dev.off()
  
}

plot4 <- function(){
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  par(mfrow=c(2,1))
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  sccIndex_v <-grep('*Coal',SCC$EI.Sector)
  scc.coal <- SCC[sccIndex_v,]

  NEI.us.coal <- NEI[which(NEI$SCC %in% scc.coal$SCC),]
  
  NEI.us.coal$year = as.character(NEI.us.coal$year)
  mergedData = merge(NEI.us.coal,  SCC, by="SCC")
  mergedData$year = as.character(mergedData$year)
  
  #dataset <-tapply( NEI.us.coal$Emissions/1000,  NEI.us.coal$year, sum)
  #plot1<-barplot(dataset,xlab="Year", ylab=expression('PM'[2.5]),
  #         main="US Emission from coal combustion",col=c("darkblue","red", "gold", "blue"))

  
  qp<- qplot(year, Emissions/1000, data=mergedData, stat="summary", fun.y="sum",facets= .~EI.Sector, geom="bar", fill=year)
  qp + ggtitle("US Emission from coal combustion by Combustion Types(EI.Sector)")

  #dev.copy(png, file="plot4.png")
  #dev.off()
}



plot5 <- function(){
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  sccIndex_v <-grep("^Mobile.*On-Road.*(Diesel|Gasoline)",SCC$EI.Sector)
  scc.motor <- SCC[sccIndex_v,]
  NEI.baltimore <- NEI[which(NEI$fips=='24510'),] 
  
  NEI.baltimore.motor <- NEI.baltimore[which(NEI.baltimore$SCC %in% scc.motor$SCC),]
  mergedData = merge(NEI.baltimore.motor,  SCC, by="SCC")
  mergedData$year = as.character(mergedData$year)
  qp<- qplot(year, Emissions, data=mergedData, stat="summary", fun.y="sum",facets= .~EI.Sector, geom="bar", fill=year)
  qp + ggtitle("Baltimore Emission from motor by Mobile Types(EI.Sector)")
  
  
  #dataset <-tapply( NEI.baltimore.motor$Emissions,  NEI.baltimore.motor$year, sum) 
  #barplot(dataset,xlab="Year", ylab=expression('PM'[2.5]),
  #        main="Emission from Baltimore Motors",col=c("darkblue","red", "gold", "blue"))

  dev.copy(png, file="plot5.png")
  dev.off()
}

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}
stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)
}
plot6 <- function(){
  par(mar=c(4, 4, 4, 4)) 
  par(oma=c(2, 2, 2, 2)) 
  #NEI <- readRDS("summarySCC_PM25.rds")
  #SCC <- readRDS("Source_Classification_Code.rds")
  sccIndex_v <-grep("*Vehicles*", SCC$SCC.Level.Two)
  scc.motor <- SCC[sccIndex_v,]

  

  #NEI.baltimore <- NEI[which(NEI$fips=='24510'),] 
  #NEI.LA <- NEI[which(NEI$fips=='06037'),] 
  #NEI.baltimore.motor <- NEI.baltimore[which(NEI.baltimore$SCC %in% scc.motor$SCC),]
  #NEI.LA.motor <- NEI.LA[which(NEI.LA$SCC %in% scc.motor$SCC),]
  #array.baltimore <-tapply( NEI.baltimore.motor$Emissions,  NEI.baltimore.motor$year, sum)
  #array.LA <-tapply( NEI.LA.motor$Emissions,  NEI.LA.motor$year, sum)


  
  #xnames<-names(array.baltimore)
  #df.baltimore <- data.frame(xnames, as.vector(array.baltimore))
  #df.LA <- data.frame(xnames, as.vector(array.LA))
  
  #df<-data.frame(xnames, as.vector(array.LA),as.vector(array.baltimore))

  NEI.two <-NEI[which(NEI$fips=='06037' | NEI$fips=='24510'),] 
  NEI.two$year = as.character(NEI.two$year)
  #c3[,1][which(c3[,1]<12)] <- -1 
  
  NEI.two[,1][ which(NEI.two$fips=='06037')] <- 'LA'
  NEI.two[,1][ which(NEI.two$fips=='24510')] <- 'Baltimore'
  qp<- qplot(year, Emissions, data=NEI.two, stat="summary", fun.y="sum",facets= .~fips, geom="bar", fill=year)
  qp + ggtitle("LA vs Baltimore Emission")+stat_sum_df("normalize")
  
  #dev.copy(png, file="plot6.png")
  #dev.off()
  #x<-xnames
  #y1<-df[,2]
  #y2<-df[,3]

  #barplot(counts,xlab="Year", ylab=expression('PM'[2.5]),
  #        main="Emission from Baltimore Motors",col=c("darkblue","red", "gold", "blue"))
  #plot(x,y1,
  #     type='n', ylab=expression('PM'[2.5]),  xlab="Year", ylim=range(200,7500)
  #     ,main="Emission from LA/Baltimore Motors for year 1999, 2002, 2005, and 2008", col="blue")

  #lines(x,y2, type='l', col="blue")
  #lines(x,y1, type='l', col="red")
}

plot_v2<-function(){
  
  x  <- seq(-2, 2, 0.05)
  y1 <- pnorm(x)
  y2 <- pnorm(x,1,1)
  plot(x,y1,type="n")
  lines(x,y2,col="green")
  lines(x,y1,col="red")
}


plot_v0 <-function()
{
  x1 <- NEI$Emission[NEI$year==1999] 
  x2 <- NEI$Emission[NEI$year==2002] 
  x3 <- NEI$Emission[NEI$year==2005] 
  x4 <- NEI$Emission[NEI$year==2008] 
  vioplot(x1, x2, x3,x4 ,
          names=c("1999", "2002", "2005", "2008"), col=c("red","blue","yellow", "pink")) 
  title("US Emission Violin plot")
}


plot_v <-function()
{
  NEI.baltimore <- NEI[which(NEI$fips==24510),] 
  x1 <- NEI.baltimore$Emission[NEI.baltimore$year==1999] 
  x2 <- NEI.baltimore$Emission[NEI.baltimore$year==2002] 
  x3 <- NEI.baltimore$Emission[NEI.baltimore$year==2005] 
  x4 <- NEI.baltimore$Emission[NEI.baltimore$year==2008] 
  vioplot(x1, x2, x3,x4 ,
          names=c("1999", "2002", "2005", "2008"), col=c("red","blue","yellow", "pink")) 
  title("Baltimore Emission Violin plot")
}

plot_v1 <- function()
{
  
  x1 <- mtcars$mpg[mtcars$cyl==4] 
  x2 <- mtcars$mpg[mtcars$cyl==6] 
  x3 <- mtcars$mpg[mtcars$cyl==8] 
  vioplot(x1, x2, x3, 
          names=c("4 cyl", "6 cyl", "8 cyl"), col=c("red","blue","yellow")) 
  title("Violin Plots of Miles Per Gallon")
  
}