faddThem<- function(x,y){
  return(x+y)
}
faddThem(2,3)

fmult<- function(x,y){
  return(x*y)
}

mult(2,3)

fpv<- function(fv,r,n){
  return(fv/((1+r)^n))
}

fpv(10000,.10,15)
fpv(10000,.10,1:100)
1:100
i<- seq(.01,.15,.01)


fpv(10000,i,15)

##PV of annuities

# = CF(1-1/(1+r)^n)/r
fpva<- function(cf,r,n){
  a<- (cf*(1-(1/(1+r)^n)))/r
  return(a)
  
}

fpva(10000,.15,20)


####Price of the bond
#CF =CR*Face value.... Face value is 10,000 and CR is 8%
#Face Value, Coupon Rate, Yeild to Maturity, Time periid

fbondprice<- function(faceval,cr,ytm,n){
  cf<- faceval*cr
  pvfv<- fpv(faceval,ytm,n)
  pvcf<- fpva(cf,ytm,n)
  p<- pvfv+pvcf
  return(p)
}


fbondprice(10000,.08,.08,10)
fbondprice(10000,.08,.12,10)


fbondprice1<- function(faceval,cr,ytm,n){
  cf<- faceval*cr
  a<- (cf*(1-(1/(1+ytm)^n)))/ytm
  b<- faceval/((1+ytm)^n)
  #pvfv<- fpv(faceval,ytm,n)
  #pvcf<- fpva(cf,ytm,n)
  p<- a+b
  return(p)
  
}



fbondprice1(10000,.08,.08,10)
fbondprice1(10000,.08,.12,10)
ytmvec<- seq(0.1,.30,.01)
bpvec<- fbondprice1(10000,.08,seq(0.1,.30,.01),10)

plot(ytmvec,bpvec)


notoday<- function(x){
  x<- 
  
}

ffirstlast<- function(xfirst,ylast){
  
  z<- paste(ylast,",",xfirst,sep = "")
  
  return(z)
}
  


ffirstlast("Chayan","Gujral")

dndtext<- function(x){
  if (x==1) y <- "Sunday"
  if (x==2) y<- "Monday"
  if (x==3) y<- "Tuesday"
  if (x==4) y<- "Wednesday"
  if (x==5) y<- "Thursday"
  if (x==6) y<- "Friday"
  if (x==7) y<- "Saturday"
return(y)
}
  
dndtext1<- function(x){
  lapply(x)
  
}



