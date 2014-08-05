
library(data.table)
plot4 <- function(){
  
  powercon =read.table("./data/household_power_consumption.txt", header = TRUE, sep = ";",stringsAsFactors=F)

  powercon.sub <-subset(powercon, Date=="1/2/2007" | Date=="2/2/2007" )
  
  datetime.v = paste(powercon.sub$Date,powercon.sub$Time);
  powercon.sub$DateTime = datetime.v
  powercon.sub$DateTime <- strptime(powercon.sub$DateTime,format="%d/%m/%Y %H:%M:%S")
  powercon.sub$Sub_metering_1 <- as.numeric(powercon.sub$Sub_metering_1)
  powercon.sub$Sub_metering_2 <- as.numeric(powercon.sub$Sub_metering_2)
  powercon.sub$Sub_metering_3 <- as.numeric(powercon.sub$Sub_metering_3)
  powercon.sub$Global_active_power <- as.numeric(powercon.sub$Global_active_power)
  powercon.sub$Global_reactive_power <- as.numeric(powercon.sub$Global_reactive_power)
  powercon.sub$Voltage <- as.numeric(powercon.sub$Voltage)
  par (mfrow = c(2,2))
  
  with(powercon.sub, {
    plot(DateTime, Global_active_power,xlab="Date", 
         ylab="Global Active Power",type='n')
    with(subset(powercon.sub), lines(DateTime, Global_active_power))
    
    plot(DateTime, Voltage,xlab="Date", 
         ylab="Voltage",type='n')
    with(subset(powercon.sub), lines(DateTime, Voltage))
    
    
    plot(DateTime, Global_active_power,xlab="Date", 
         ylab="Energy sub metering",type='n',ylim=c(0,40))
    with(subset(powercon.sub), lines(DateTime, Sub_metering_1))
    with(subset(powercon.sub), lines(DateTime, Sub_metering_2,col="red"))
    with(subset(powercon.sub), lines(DateTime, Sub_metering_3,col="blue"))
    legend("topright", col=c("black","red","blue"), lty=c(1,1,1),lwd=c(2.5,2.5),cex=0.2,
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    plot(DateTime, Global_reactive_power,xlab="Date", 
         ylab="Global Reactive Power",type='n')
    with(subset(powercon.sub), lines(DateTime, Global_reactive_power))
    
    
  })

  dev.copy(png, file="plot4.png")
  dev.off()

}