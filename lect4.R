
l4_1<-function(){
	f <-gl(40,1)
	str(f)
	str(airquality)
}

l4_2<-function(){
	s<- split(airquality,airquality$Month)
	s
}

l4_3<-function(){
	set.seed(1)
	x<-rnorm(10)
	y<-mean(x)
	summary(x)
}

l4_4<-function(){
	x<-rpois(10,1)
	x
}

l4_5<-function(){
	set.seed(20)
	x <-rnorm(100)
	e <-rnorm(100,0,2)
	y <- 0.5 + 2* x + e
	summary(y)
	plot(x,y)
}

l4_6<-function(){
	set.seed(20)
	x <-rbinom(100,1,0.5)
	e <-rnorm(100,0,2)
	y <- 0.5 + 2* x + e
	summary(y)
	plot(x,y)
}


l4_7<-function(){
	set.seed(1)
	x <-rnorm(100)
	log.mu <- 0.5 + 0.3*x
	
	y <- rpois(100,exp(log.mu))
	s<-summary(y)
	print(s)
	plot(x,y)
}

l4_8 <- function(){
	set.seed(1)
	s<-sample(1:10,4)
	s
}

l4_9 <- function(){
	t <-system.time(readLines("http://www.jhsph.edu"))
	t
}

l4_10 <- function(n){
	i<-1:n
	1 / outer(i-1, i,"+")
}

l4_11 <- function(){
	x<-l4_10(2000)
	t<-system.time(svd(x))
	t
}

q4_1 <-function(){
	set.seed(1)
	x<-rpois(5,2)
	x
}
