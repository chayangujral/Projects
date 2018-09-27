library(psych)
library(caret)
library(FFTrees)
library(randomForest)
library(readr)
library(randomForestSRC)
library(dplyr)
library(ranger)
library(Rborist)
creditcard <- read_csv("~/creditcardfraud (1)/creditcard.csv")

mydata<- creditcard
mydata$Class<- as.factor(mydata$Class)
names(mydata)
table(mydata$Class)
T_stamp<- proc.time()
mydata<- na.omit(mydata)
dim(mydata)

p<- sample(2,nrow(mydata),replace=T,prob = c(0.2,0.7))

dim(newdata)
newdata<- mydata[p==1,]

p1<- sample(2,nrow(newdata),replace=T,prob = c(0.7,0.3))

train<- newdata[p1==1,]   
test<-  newdata[p1==2,]
dim(train)
dim(test)

#Fastand Frugal Method

Time_SF<- proc.time()
rf2<- FFForest1(Class~.,data =train,ntree = 50)
names(rf2)
rf2$fft.models
model_F<- rf2$fft.models[[50]]
predict<- predict(model,test)


TFF<-table(predict,test$Class)
Acc_F<- sum(diag(TFF))/sum(TFF)
Acc_F
TIME_F<- proc.time()-Time_SF

#Random Forest
Time_RF<- proc.time()
train1<- train
train1$Class <- as.factor(train1$Class)
rf3<- randomForest(Class~., data =train1 ,mtry = 5, ntree =50,do.trace =TRUE)
names(rf3)

print(rf3)
predictR<- predict(rf3,test)
Ytest<- test$Class


TRF<-table(predictR,test$Class)
AccRF<- sum(diag(TRF))/sum(TRF)
AccRF
Time_R<- proc.time()-Time_RF

###Random Foest Using randomforestSRC

Time_rfrc<- proc.time()

test1<- test
test1$Class<- as.factor(test1$Class)
rfs <- rfsrc(formula = Class~., data = as.data.frame(train1),mtry=5, ntree = 50,do.trace= TRUE)


predict_rfs<- predict(rfs,newdata= as.data.frame(test1))


names(predict_rfs)

ACCRC<- 1-predict_rfs$err.rate[50,1]
ACCRC
time_rfrc<- proc.time()-Time_rfrc



####Ranger Package

Timeranger<- proc.time()
ranger_rf<- ranger(Class~., data = train1,num.trees = 50, mtry = 5)

predict_ranger<- predict(ranger_rf,test)


table_ranger<- table(predict_ranger$predictions,test$Class)
Acc_ranger = sum(diag(table_ranger))/sum(table_ranger)
Acc_ranger
Time_ranger<- proc.time() - Timeranger


####Rborist

help("Rborist")

Time1<- proc.time()

z<- as.vector(train1[[31]])
length(z)

Rb<- Rborist(train1[-31],as.factor(z), nTree = 50,ctgCensus="votes" ,classWeight = "balance",predFixed =30)
Rb$validation$confusion
Predict_RB<- predict(Rb,newdata =test)
Pred<- Predict_RB$yPred-1
TRB<- table(Pred,test$Class)
TRB
Predict_RB$
ACc_RB<- sum(diag(TRB)/sum(TRB))
ACc_RB
Time_RB<- proc.time()-Time1
Time_RB




#####################3===========================Comparison

table_comp<- matrix(1
                    ,5)
table_comp<- matrix(c(2.30,3.64,13.44,11.82,2202.42))
table_comp<- table_comp[1:5]
colnames(table_comp)<- colname


table_Acc<- matrix(c(ACc_RB,Acc_ranger,ACCRC,ACc_RB,Acc_F))

Table_Compa<- cbind(table_comp,table_Acc)
Table_Compa
rownames(Table_Compa) <- c('Rborist','Ranger','randomForestSRC','RandomForest','FFTRees')
colnames(Table_Compa)<- c("Time(sec)","Accuracy")
plot(Table_Compa,xlim = )
