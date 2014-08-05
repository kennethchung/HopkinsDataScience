corr <- function(directory, threshold = 0) {
	
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	corvec <- numeric()

	for (i in 1:332){
		fileId = sprintf('%03d', i)
		dataset <- read.csv(paste (getwd(),"/",directory,"/",fileId,".csv",sep = ""))
		
		## 'threshold' is a numeric vector of length 1 indicating the
		## number of completely observed observations (on all
		## variables) required to compute the correlation between
		## nitrate and sulfate; the default is 0
		d<-dataset[complete.cases(dataset),]
		
		if (nrow(d) > threshold){
			corvec <- c(corvec, cor(d$nitrate,d$sulfate,use="complete.obs"))
		}
	}	
	## Return a numeric vector of correlations
	corvec
	
	#cor.df <- data.frame(corvec, sum.sul,sum.nit, idvec)
	#colnames(cor.df) <- c("correlation", "sulfate_sum","nitrate_sum","fileid")
	#cor.df
}