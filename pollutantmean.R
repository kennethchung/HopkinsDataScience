pollutantmean <- function(directory, pollutant, id = 1:332) {

	
	pollutantMean <- numeric()
	## 'pollutant' is a character vector of length 1 indicating
	## the name fof the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	for (i in id){
		
		## 'directory' is a character vector of length 1 indicating
		## the location of the CSV files
		fileId = sprintf('%03d', i)
		dataset <- read.csv(paste (getwd(),"/",directory,"/",fileId,".csv",sep = ""))
		pollutantMean = c(pollutantMean,dataset[[pollutant]])	
	}
	
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	mean(pollutantMean, na.rm=TRUE)

	

	
	
	

}




