# register these custom functions which creates descriptive statistics
# create descriptive statistics function - this need be done ONLY ONE TIME on your computer.
# HIGHLIGHT THE WHOLE BLOCK OF CODE (cntl A) and RUN

printLines<-function(){
  # printLines(250000)  
  # specifies maximum number of lines of an object (dataframe) to print
  options(max.print=250000)}

fyrmth<-
# function to return yrMth (yyyy.mth) from a date vector (mmddyyyy)
function(datee){as.numeric(substring(datee,7,10))+as.numeric(substring(datee,1,2))/100}

fskew<-function(x){
  # custom skewness function  
  3*mean(x,na.rm=T)/sd(x,na.rm=T)}

fkurt<-function(x){NA}

# function to return descriptive statistics on a dataframe
fdesstat<-function(inputDataFrame){
  meanVec<-apply(inputDataFrame,2,mean,na.rm=TRUE)
  medVec<-apply(inputDataFrame,2,median,na.rm=TRUE)
  sdVec<-apply(inputDataFrame,2,sd,na.rm=TRUE)
  skewVec<-apply(inputDataFrame,2,fskew)
  kurtVec<-apply(inputDataFrame,2,fkurt)
  minVec<-apply(inputDataFrame,2,min,na.rm=TRUE)
  maxVec<-apply(inputDataFrame,2,max,na.rm=TRUE)
  nVec<-apply(inputDataFrame,2,length)
  resultsDF<-data.frame(cbind(meanVec,medVec,sdVec,skewVec,kurtVec,minVec,maxVec,nVec))
  names(resultsDF)<-c("mean","med","stdev","skew","kurt","min","max","n")
  return(resultsDF)}

fplot3<-function(x,y,z){
  plot(x,y,type="n"); text(x,y,z,cex=.4)
}

ftsplot2 <- function(x,y){
  # displays time series plot with 2 lines
  # Time Series Plot for Two Variables
  # x is in solid line and y is in dotted line
  # this function is courtesy of Peter Yin
  plot(x,type="l",lty=1); lines(y,lty=2)}


# Time Series Plot for Two Variables
# x is in solid line and y is in dotted line
ftsplot2a <- function(x,y){
	plot(x,type="l",lty=1); lines(y,lty=2)
}

fsortdfAsc<-function(mydf,sortColNo){
  # sorts a dataframe in ascending order
  mydf<-mydf[order(mydf[,sortColNo], decreasing=FALSE),]
  return(mydf)
}

fsortdfDec<-function(mydf,sortColNo){
  # sorts a dataframe in decending order
  mydf<-mydf[order(mydf[,sortColNo], decreasing=TRUE),]
  return(mydf)
}

flag<-function(myVec,n){
  # creates a lagged value of a vector
  # returns previous nth value in a series
  as.numeric(c(rep(NA,n),myVec[1:(length(myVec)-n)]))}

fdiff<-function(x,n){
  # creates the difference between the current value and the previous value of a vector
  x-c(rep(NA,n),x[1:(length(x)-n)])}

fpctcng<-function(x,n){
  # returns the percentage difference between the current value and the previous value of a vector
  numVec<-x[(n+1):length(x)]
  denomVec<-x[1:(length(x)-n)]
  numVec/denomVec-1
  c(rep(NA,n),numVec/denomVec-1)}

fhelp<-function(){
  # printLines()
      # printLines(250000)  
      # specifies maximum number of lines of an object to display
  # fyrmth(dateVector)
      # function to return yrMth (yyyy.mth) from a date vector (mmddyyyy)
  # fdesstat(dataFrame)
	# displays descriptive statistics for all variables included 
  # ftsplot2(vector1, vector2)
      # displays time series plot with 2 lines
  # fsortdfAsc(aDataframe, sortColNo)
  # fsortdfDec(aDataframe, sortColNo)
  # flag(vector,nPeriods)
      # creates a lagged value of a vector
      # returns previous nth value in a series
  # fdiff(vector,nPers)
      # creates the difference between the current value and the previous value of a vector
  # fpctcng(vector,nPers)
      # returns the percentage difference between the current value and the previous value of a vector
  }

