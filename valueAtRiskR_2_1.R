indf<- bbts500cos2[ ,c("amzn","trip","msft","nflx","aapl")]
indf
indf<- na.omit(indf)
indf
fVar_custom1<- function(df,amt,p){
    p<- as.vector(p)
  fpctcng<-function(x,n){
    # returns the percentage difference between the current value and the previous value of a vector
    numVec<-x[(n+1):length(x)]
    denomVec<-x[1:(length(x)-n)]
    # numVec/denomVec-1
    c(rep(NA,n),numVec/denomVec-1)}
  
  vdf<-df
  vdf<-as.data.frame(df)
  vdf<- na.omit(vdf)
  pordf<-vdf
  pordf<- na.omit(pordf)
  nSecs<-length(names(pordf))
  nSecs
  
  
  #sapply(pordf,fpctcng,1)
  rrdf<- data.frame(sapply(pordf,fpctcng,1))
  rrdf<- na.omit(rrdf)
  dimrrdf<- dim(rrdf)
  
  mult<- function(x)
  {
    x<- as.data.frame(x)
    return(p*amt*x)
  }
  
  dollarReturnDF<- as.data.frame(apply(rrdf,1,mult))
  
  #dfdim<- dim(dollarReturnDF)
  #dollarReturnVector<- sapply(1:dfdim[1],function(i){sum(dollarReturnDF[i,],na.rm = T)}) 
  
  
  dollarReturnVector<- as.vector(apply(dollarReturnDF,2,sum))
  #dollarReturnVector == dollarReturVector1
  #while(i <= nSecs){
   # z =0
  #  z<- matrix(z,nrow = dim[1], ncol = 1)
   # z<- rrdf[,i]*p[i]*amt # in millions
    #dollarReturnDF= (z+dollarReturnDF)
    #i=i+1
    #}
  
  #dollarReturnVector<-apply(dollarReturnDF,1,sum,na.rm=T)  # in millions
  
  #5% and 1% parametric VAR
  p_var5<-(mean(dollarReturnVector,na.rm=T)-1.65*sd(dollarReturnVector,na.rm=T))
  p_var1<-(mean(dollarReturnVector,na.rm=T)-2.33*sd(dollarReturnVector,na.rm=T))
  
  #5% and 1% historical VAR
  n<-length(dollarReturnVector)
  h_var5<-sort(dollarReturnVector)[round(n*.05,0)]
  h_var1<-sort(dollarReturnVector)[round(n*.01,0)]
  
  varResultsdf1<-data.frame(matrix(2,2))
  varResultsdf1[1,1]<-p_var1
  varResultsdf1[1,2]<-p_var5
  varResultsdf1[2,1]<-h_var1
  varResultsdf1[2,2]<-h_var5
  names(varResultsdf1)<- c("1%","5%")
  row.names(varResultsdf1)<-c("parametric","historical")
  return(varResultsdf1)
  p_var5
}
df
rrdf
d<- c(0.1,0.1,0.1,0.2,0.5)
fVar_custom1(indf,10,d)

fVar(df)
