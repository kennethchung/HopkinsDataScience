rankall <- function (outcome, num="best"){
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
	
	mortalityCol <-1
	if (outcome == "heart failure") {
		mortalityCol <- 17
	}
	else if (outcome == "heart attack"){
		mortalityCol <- 11
	}else if (outcome == "pneumonia"){
		mortalityCol <- 23
	}else{
		stop("invalid outcome")
	}
	
	#result<-colnames(dt.state )
	df[,mortalityCol ] <- as.numeric(df[,mortalityCol])
	df.mortality <- df[complete.cases(df[,mortalityCol],df[,2],df[,7]),]
	s<- split(df.mortality,df.mortality$State,)
	
	l<-lapply(s, function(x) rankhospital1(x$State[[1]],outcome,num))
	l
	result <- as.data.frame(do.call(rbind, l))
	names(result) <-c("hospital","state")
	result
	
	#dt.sorted <- df.mortality[ order(df.mortality[,mortalityCol], df.mortality[,2]), ]
	#result<-dt.sorted[,c(2,mortalityCol)]
	
	#dt.sorted <- df.mortality[ order(df.mortality[,mortalityCol], df.mortality[,2]), ]
	
	
	
}

