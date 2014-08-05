
library(data.table)
plot3 <- function(){
  
  powercon =read.table("./data/household_power_consumption.txt", header = TRUE, sep = ";",stringsAsFactors=F)
  #powercon =fread("./data/household_power_consumption.txt", header = TRUE, sep = ";",
  #                stringsAsFactors=FALSE,
  #                verbose=FALSE,
  #                na.strings=c("?","NA","N/A",""))
  powercon.sub <-subset(powercon, Date=="1/2/2007" | Date=="2/2/2007" )
  
  datetime.v = paste(powercon.sub$Date,powercon.sub$Time);
  powercon.sub$DateTime = datetime.v
  powercon.sub$DateTime <- strptime(powercon.sub$DateTime,format="%d/%m/%Y %H:%M:%S")
  powercon.sub$Sub_metering_1 <- as.numeric(powercon.sub$Sub_metering_1)
  powercon.sub$Sub_metering_2 <- as.numeric(powercon.sub$Sub_metering_2)
  powercon.sub$Sub_metering_3 <- as.numeric(powercon.sub$Sub_metering_3)
  
  with(powercon.sub, {
    plot(DateTime, Global_active_power,xlab="Date", 
         ylab="Energy sub metering",type='n',ylim=c(0,40))
  })
  with(subset(powercon.sub), lines(DateTime, Sub_metering_1))
  with(subset(powercon.sub), lines(DateTime, Sub_metering_2,col="red"))
  with(subset(powercon.sub), lines(DateTime, Sub_metering_3,col="blue"))
  legend("topright", col=c("black","red","blue"), lty=c(1,1,1),lwd=c(2.5,2.5),cex=0.8,
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.copy(png, file="plot3.png")
  dev.off()

}