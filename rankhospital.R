rankhospital <- function (state, outcome, num="best"){
	
	if (num=="best"){
		best(state,outcome)
	}else if (num == "worst")
	{
		
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
		
		df.state[,mortalityCol ] <- as.numeric(df.state[,mortalityCol])
		df.mortality <-df.state[complete.cases(df.state[,mortalityCol]),]
		result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == max(df.mortality[,mortalityCol ], na.rm=TRUE)]
		result
		
	}
	else if ( is.numeric(num) & !is.na(num))
	{
		rank <- as.numeric(num)
		
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
		df.mortality <- df.state[complete.cases(df.state[,mortalityCol],df.state[,2]),]
	
		
		dt.sorted <- df.mortality[ order(df.mortality[,mortalityCol], df.mortality[,2]), ]
		result<-dt.sorted[rank,c(2)]
		#result <- result(1:2, dimnames=list(1:2, c("hospital","State")))
		#result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == min(df.mortality[,mortalityCol ]
		#result<- with(dt.state, Hospital.Name[dt.state[,mortalityCol ] == min(dt.state[,mortalityCol])]
		#names(result) <- c("hospital","state")
		result
	}else
	{
		stop("invalid num")	
	}
}

rankhospital1 <- function (state, outcome, num="best"){
	
	selectState<-state

	if (num=="best"){
		best1(state,outcome)
	}else if (num == "worst")
	{
		
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
		
		df.state[,mortalityCol ] <- as.numeric(df.state[,mortalityCol])
		df.mortality <- df.state[complete.cases(df.state[,mortalityCol],df.state[,7]),]
		#result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == max(df.mortality[,mortalityCol ], na.rm=TRUE)]
		#print(typeof(result))
		dt.sorted <- df.mortality[ order(-df.mortality[,mortalityCol], df.mortality[,2]), ]
		result<-dt.sorted[1 ,c(2)]
		endresult <- c(result,state)
		endresult
		
	}
	else if ( is.numeric(num) & !is.na(num))
	{
		rank <- as.numeric(num)
		
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
		
		
		df.state[,mortalityCol ] <- as.numeric(df.state[,mortalityCol])
		df.mortality <- df.state[complete.cases(df.state[,mortalityCol],df.state[,7]),]
		dt.sorted <- df.mortality[ order(df.mortality[,mortalityCol], df.mortality[,2]), ]
		result<-dt.sorted[rank,c(2)]
		#result <- result(1:2, dimnames=list(1:2, c("hospital","State")))
		#result<-df.mortality$Hospital.Name[ df.mortality[,mortalityCol ] == min(df.mortality[,mortalityCol ]
		#result<- with(dt.state, Hospital.Name[dt.state[,mortalityCol ] == min(dt.state[,mortalityCol])]
		
		endresult <- c(result,state)
		endresult
	}else
	{
		stop("invalid num")	
	}
}