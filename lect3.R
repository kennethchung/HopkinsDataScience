myapp <- function(){
	x<-list(a=1:5,b=rnorm(10))
	lapply(x,mean)
}



myapp1 <- function(){
	x<-list(a=1:5,b=rnorm(10),c=rnorm(10),d=rnorm(10))
	lapply(x,mean)
}


myapp2 <- function(){
	x<-1:5
	lapply(x,runif)
}

myapp3 <- function(){
	x<-1:4
	lapply(x,runif,min=0,max=10)
}




myapp4 <- function(){
	x<-list(a=matrix(1:4,2,2), matrix(1:6,3,2))
	x	
	
}

myapp5 <- function(){
	lapply(myapp4(), function(elt) elt[,1])
	
}


myapp6 <- function(){
	lapply(myapp4(), function(elt) elt[,1])
	
}

myapp7 <- function(){
	x<-list(a=1:5,b=rnorm(10),c=rnorm(10),d=rnorm(10))
	sapply(x,mean)
}


myapp8 <- function(){
	x<-list(a=1:5,b=rnorm(10),c=rnorm(10),d=rnorm(10))
	sapply(x,mean)
}

library(datasets)
data(iris)
data(mtcars)


q1<-function(){
	
	mean( subset(iris,Species == "virginica")$Sepal.Length, na.rm=TRUE)
	
}
q11 <-function(){
	with(iris,Sepal.Length[Sepal.Width== max(Sepal.Width[Species=="setosa"])])
	#with(df, d[v== max(v[c=="foo"])])
}
q2<-function(){
	apply(iris[,1:4],2,mean,na.rm=TRUE)
	
}

q3<-function(){
	#tapply(mtcars$mpg, mtcars$cyl,mean)
	with(mtcars, tapply(mpg,  cyl,mean))
	#sapply(mtcars, cyl,mean)
	#lapply(mtcars, mean)
	#mean(mtcars$mpg,mtcars$cyl)
	
}

q4<-function(){
	tapply(mtcars$hp, mtcars$cyl,mean)
	#sapply(mtcars, cyl,mean)
	#lapply(mtcars, mean)
	#mean(mtcars$mpg,mtcars$cyl)
	#abs(x[1]-x[3])
}


myapp9 <-function(){
	x<- matrix(rnorm(200), 20,10)
	#collaps dim 2, keep col, collaps rows
	apply(x,2,mean)
}

myapp10 <-function()
{   
	x<- matrix(rnorm(200), 20,10)
	colMeans(x)
	colSum(x)
	rowMeans(x)
	rowSum(x)
}


myapp11 <-function()
{   
	x<- matrix(rnorm(200), 20,10)
	apply(x,1,quantile, probs = c(0.25,0.75))
}


myapp12 <- function()
{
	a <- array(rnorm(2*2*10), c(2,2,10))
	apply(a,c(1,2),mean)
	rowMeans(a, dims=2)
}


myapp13 <- function()
{
	a <- array(rnorm(2*2*10), c(2,2,10))
	apply(a,c(1,2),mean)
	rowMeans(a, dims=2)
}

myapp14 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	tapply(a,f,mean, simplify=FALSE)
}

myapp15 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	tapply(a,f,range, simplify=FALSE)
}

myapp16 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	tapply(a,f,range, simplify=FALSE)
}

myapp17 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	split(a,f)
}

myapp18 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	lapply(split(a,f),mean)
}


myapp19 <- function()
{
	a <- c(rnorm(10),runif(10),rnorm(10,1))
	f<-gl(3,10)
	lapply(split(a,f),mean)
}

myapp20 <- function()
{
	s <- split(airquality, airquality$Month)
	lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
	
}

myapp21 <- function()
{
	s <- split(airquality, airquality$Month)
	sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
	
}


myapp22 <- function()
{
	s <- split(airquality, airquality$Month)
	sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")],na.rm=TRUE))
	
}


myapp23 <- function()
{
	x <- rnorm(10)
	f1<-gl(2,5)
	f2 <- gl(5,2)
	interaction(f1,f2)
	str(split(x,list(f1,f2),drop=TRUE))
	f1
	f2
	interaction(f1,f2)
}

myapp24 <- function()
{
	list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))
	mapply(rep,1:4,4:1)
}

noise <- function(n,mean,sd)
{
	rnorm(n,mean,sd)
}

myapp25 <- function()
{
	noise(5,1,2)
	mapply(noise,1:5,1:5,2)
	
	list(noise(1,1,2),noise(2,2,2),noise(3,3,2),noise(4,5,2),noise(5,5,2))
}

pmsg <- function(x){
	
	
	if (is.na(x))
			print ("x is a missing value")
	else if (x > 0){
			print("x is greater than zero")
			print(x)
		}
	else
			print ("x is less than or equal zero")
	invisible(x)
}


myapp26 <- function()
{
	x <- matrix(1:12, 4) 
	colMins(x) 
	rowMins(x) 
	colRanges(x) 
}

