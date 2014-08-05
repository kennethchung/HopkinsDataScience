
library(data.table)
plot2 <- function(){
  
  powercon =read.table("./data/household_power_consumption.txt", header = TRUE, sep = ";",stringsAsFactors=F,na.strings=c("?","NA","N/A",""))
  #powercon =fread("./data/household_power_consumption.txt", header = TRUE, sep = ";",
  #                stringsAsFactors=FALSE,
  #                verbose=FALSE,
  #                na.strings=c("?","NA","N/A",""))
  powercon.sub <-subset(powercon, Date=="1/2/2007" | Date=="2/2/2007" )
  
  datetime.v = paste(powercon.sub$Date,powercon.sub$Time);
   

  powercon.sub$DateTime = datetime.v
  powercon.sub$DateTime <- strptime(powercon.sub$DateTime,format="%d/%m/%Y %H:%M:%S")
  powercon.sub$Global_active_power <- as.numeric(powercon.sub$Global_active_power)
  
  with(powercon.sub, {
    plot(DateTime, Global_active_power,xlab="Date", ylab="Global Active Power (kilowatts)",type='n')
  })
  with(subset(powercon.sub), lines(DateTime, Global_active_power))
  dev.copy(png, file="plot2.png")
  dev.off()
}