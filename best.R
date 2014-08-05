best <- function (state, outcome){
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
	df.state <- subset(df,State == state)
	
	stateRow <- nrow(df.state)
	if (stateRow == 0){
		stop("invalid state")
	}
		
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
	df.state[,mortalityCol ] <- as.numeric(df.state[,mortalityCol])
	df.mortality <-df.state[complete.cases(df.state[,mortalityCol]),]
	result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == min(df.mortality[,mortalityCol ], na.rm=TRUE)]
	#result<- with(dt.state, Hospital.Name[dt.state[,mortalityCol ] == min(dt.state[,mortalityCol])]
	result
}


best1 <- function (state, outcome){
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
	df.state <- subset(df,State == state)
	
	stateRow <- nrow(df.state)
	if (stateRow == 0){
		stop("invalid state")
	}
	
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
	df.state[,mortalityCol ] <- as.numeric(df.state[,mortalityCol])
	df.mortality <-df.state[complete.cases(df.state[,mortalityCol]),]
	#result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == min(df.mortality[,mortalityCol ], na.rm=TRUE)]
	#result<- with(dt.state, Hospital.Name[dt.state[,mortalityCol ] == min(dt.state[,mortalityCol])]
	dt.sorted <- df.mortality[ order(df.mortality[,mortalityCol], df.mortality[,2]), ]
	result<-dt.sorted[1,c(2)]
	endresult <- c(result,state)
	endresult
}