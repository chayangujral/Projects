

fsdf<-fs1500y
dim(fsdf)
names(fsdf)

fsdf$date[1:40]
fsdf$year<-as.numeric(substring(fsdf$date,1,4))

# TABLE
table(fsdf$year)
table(fsdf$sector)
data.frame(table(fsdf$sector))

smdf<-fsdf[fsdf$year==2015,]
dim(smdf)
dim(smdf)

# APPLY
names(smdf)
apply(smdf[,7:11],2,mean,na.rm=T)
apply(fsdf[fsdf$year==2013,7:11],2,mean,na.rm=T)

# S-APPLY – (a bit advanced)
sapply(1999:2015,function(i){mean(fsdf[fsdf$year==i,7:11],na.rm=T)})


# PIVOT TABLE - TAPPLY
smdf$eps
mean(smdf$eps,na.rm=T)
tapply(smdf$eps,smdf$sector,mean,na.rm=T)

rsmdf<-smdf[smdf$io500==1,]
rsmdf<-as.data.frame(rsmdf)
dim(rsmdf)
smv<-sum(rsmdf$mv,na.rm=T)
rsmdf$wgt<-rsmdf$mv/smv
rsmdf[,c("name","wgt")]

data.frame(tapply(rsmdf$wgt,rsmdf$sector,sum,na.rm=T))

data.frame(tapply(smdf$eps,smdf$sector,mean,na.rm=T))
data.frame(tapply(smdf$eps,smdf$sector,sd,na.rm=T))
data.frame(tapply(smdf$eps,smdf$sector,length))

data.frame(mu=(tapply(smdf$eps,smdf$sector,mean,na.rm=T)),
sd=(tapply(smdf$eps,smdf$sector,sd,na.rm=T)),
n=(tapply(smdf$eps,smdf$sector,length)))

data.frame(mu=(tapply(smdf$eps,smdf$industry,mean,na.rm=T)),
           sd=(tapply(smdf$eps,smdf$industry,sd,na.rm=T)),
           n=(tapply(smdf$eps,smdf$industry,length)))

round(tapply(fsdf$eps,list(fsdf$sector,fsdf$year),mean,na.rm=T),2)
smdf<-yearDF[yearDF$tkr==”IBM”,c(“price”,”name”,”sector”,
“industry”,”eps”)]
	smdf<-na.omit(smdf)
	corPriceEps<-cor(smdf$price,smdf$eps)
	n<-dim(smdf	)

# For Loops
# 1 single loop
rowIndex<-0
beginTime<-Proc.time()`
For (I in 100:150){
	rowIndex<-rowIndex+1
	print(rowIndex)
	}
endTime<-Proc.time()
endTime-beginTime

# 2 nested loop
beginTime<-Proc.time()
For (I in 1:50){{
	For(j in 200:300)
	Print(i*j)
	}}
endTime<-Proc.time()
endTime-beginTime

# 3 triple loop
beginTime<-Proc.time()
For (I in 1:50){
	For(j in 200:300){
		For (z in 1:5){
		Print(i*j*z)
	}}}
endTime<-Proc.time()
endTime-beginTime




fsdf<-fs1500y
fsdf$sid<-as.numeric(fsdf$sector)
fsdf$iid<-as.numeric(fsdf$industry)
fsdf$cid<-as.numeric(fsdf$tkr)

companyIdVec<-unique(fsdf$cid)
sectorIdVec<-unique(fsdf$cid)
industryIdVec<-unique(fsdf$iid)

resultsDF<-as.data.fram(matrix(nrow=52,ncol=4))
yearDF<-fsdf[fsdf$year==2015,]
for (i in companyIdVec){
	smdf<-yearDF[yearDF$cid==i,c(“price”,”name”,”sector”,
“industry”,”eps”)]
	smdf<-na.omit(smdf)
	corPriceEps<-cor(smdf$price,smdf$eps)
	n<-dim(smdf	)
	resultsDF[i,1]<-smdf$tkr[1]
	resultsDF[i,2]<-smdf$name[1]
	resultsDF[i,3]<-smdf$sector[1]
	resultsDF[i,4]<-smdf$industry[1]
	resultsDF[i,5]<-n

	if (n>5){
		resultsDF[i,5]<-corPriceEps
		}
	print(smdf$name[1])	
}



maxID<-max(companyIdVec)
for (i in 1:maxID)
	smdf<-yearDF[yearDF$cid==i,]
}



