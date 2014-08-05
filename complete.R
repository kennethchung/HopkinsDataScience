complete <- function(directory, id = 1:332) {

	ids <- numeric()
	nobs  <- numeric()
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	for (i in id){
		
		## 'directory' is a character vector of length 1 indicating
		## the location of the CSV files
		fileId = sprintf('%03d', i)
		dataset <- read.csv(paste (getwd(),"/",directory,"/",fileId,".csv",sep = ""))
		
		d<-dataset[complete.cases(dataset),]
		ids <- c(ids,i)
		nobs <- c(nobs,nrow(d))
	}	
	nobs.df <- data.frame(ids, nobs)
	colnames(nobs.df) <- c("id", "nobs")
	
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	nobs.df
	


}
