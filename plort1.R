
library(data.table)
plot1 <- function(){
  
  #powercon =read.table("./data/household_power_consumption.txt", header = TRUE, sep = ";",stringsAsFactors=F,na.strings=c("?","NA","N/A",""))
  powercon =fread("./data/household_power_consumption.txt", header = TRUE, sep = ";",
                  stringsAsFactors=FALSE,
                  verbose=FALSE,
                  na.strings=c("?","NA","N/A",""))

  powercon.sub <-subset(powercon, Date=="1/2/2007" | Date=="2/2/2007")
  powercon.sub$Date <- as.Date(powercon.sub$Date,"%d/%m/%Y")
  powercon.sub$Global_active_power <- as.numeric(powercon.sub$Global_active_power)
  #range(powercon.sub$Global_active_power)
  hist(powercon.sub$Global_active_power,xlab="Global Active Power (kilowatts)",  
       col="red", main="Global Active Power")
  dev.copy(png, file="plot1.png")
  dev.off()
  
}