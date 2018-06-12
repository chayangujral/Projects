library(readxl)
library(ggplot2)
library(psych)
library(lmtest)
library(gam)
snp_500_index <- read_excel("~/Spring 2018/DS 633/Final/snp 500 index.xlsx", 
                            sheet = "Sheet1")
View(snp_500_index)
spDF<- snp_500_index
spDF<- as.data.frame(spDF)
spDF<- na.omit(spDF)
dim(spDF)

#histograms

par(mfcol=c(1,1))
pdf(file= "hist3.pdf")
par(mfcol=c(1,1))
hist(spDF$index,main ="Fig.1    Hist of index", prob =1, xlab = "index", col = "blue"); lines(density(spDF$index),lwd = 4)
abline(v = median(spDF$index),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$index),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$unr,main ="Fig.2  Hist of unr", prob =1, xlab = "unr", col = "blue"); lines(density(spDF$unr),lwd = 4)
abline(v = median(spDF$unr),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$unr),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$importts,main ="Fig.3    Hist of importts", prob =1, xlab = "imports", col = "blue"); lines(density(spDF$importts),lwd = 4)
abline(v = median(spDF$importts),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$importts),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$Epi,main ="Fig.4   Hist of Global Energy Price Index", prob =1, xlab = "Global Energy Price", col = "blue"); lines(density(spDF$Epi),lwd = 4)
abline(v = median(spDF$Epi),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$Epi),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$Int_rate,main ="Fig.5   Hist of Federal Fund Rate", prob =1, xlab = "Fedral Funds Rate", col = "blue"); lines(density(spDF$Int_rate),lwd = 4)
abline(v = median(spDF$Int_rate),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$Int_rate),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$cpi,main ="Fig.6   Hist of Consumer price MOM%", prob =1, xlab = "cpi", col = "blue"); lines(density(spDF$Int_rate),lwd = 4)
abline(v = median(spDF$cpi),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$cpi),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))



dev.off()
graphics.off()

#Time-Series Plot
pdf(file = "tsplot5.pdf")
par(mfcol = c(2,2))

ggplot(data = spDF, aes(x = Date, y = index)) + labs(y = "SnP Index")+ggtitle("Fig.7 Time Series for Index") +
  geom_line(color = "#00AFBB", size = 2)
ggplot(data = spDF, aes(x = Date, y = unr))+ labs(y = "UnemPloyment Rate")+ggtitle("Fig.8 Time Series for Unemployment Rate") +
  geom_line(color = "#00AFBB", size = 2)

ggplot(data = spDF, aes(x = Date, y = importts)) + labs(y = "US Imports in $")+ggtitle("Fig.9 Time Series US Imports in $") +
  geom_line(color = "#00AFBB", size = 2)

ggplot(data = spDF, aes(x = Date, y = cpi))+ labs(y = "Consumer price MOM%")+ggtitle("Fig.10 Time Series for Consumer price MOM%") +
  geom_line(color = "#00AFBB", size = 2)

ggplot(data = spDF, aes(x = Date, y = Int_rate))+ labs(y = "Federal Fund Rate")+ggtitle("Fig.11 Time Series for Federal Fund Rate") +
  geom_line(color = "#00AFBB", size = 2)

ggplot(data = spDF, aes(x = Date, y = Epi))+ labs(y = "Global Energy Price Index")+ggtitle("Fig.12 Time Series for Global Energy Price") +
  geom_line(color = "#00AFBB", size = 2)

dev.off()
graphics.off()

#scatterplots
pdf(file = "scatterplots3.pdf")
par(mfcol = c(1,1))

ggplot(data=spDF, aes(unr,index)) +   labs(x="Unemployment Rate", y="Index") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.13 Index vs Unemployment Rate")

ggplot(data=spDF, aes(importts,index)) +   labs(x="US. Imports", y="Index") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.14 Index vs US. Imports")

ggplot(data=spDF, aes(cpi,index)) +   labs(x="Consumer Price MOM%", y="Index") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.15 Index vs Consumer Price MoM%")

ggplot(data=spDF, aes(Int_rate,index)) +   labs(x="Federal Fund Rate", y="Index") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.16 Index vs Federal Fund Rate")

ggplot(data=spDF, aes(Epi,index)) +   labs(x="Global Energy Index", y="Index") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.17 Index vs Global Energy Index")

dev.off()
graphics.off()

#descriptive statistics

names(spDF)
desc<- round(describe(spDF[,c("index","unr","importts","cpi","Int_rate","Epi")]),3)
 
 #correlation matrix
 cor<- round(cor(spDF[,c("index","unr","importts","Epi","Int_rate","cpi")],use="na.or.complete"),3)
 
 

 # linear regression
 names(spDF)
 fit<-lm(index~unr+cpi+importts+Epi+Int_rate,data=spDF,na.action=na.omit)
 summary(fit)
 dwtest(fit)
 
 #Linear residual dataframe
 rdf<-data.frame(spDF,r=fit$residuals,p=fit$fitted.values)
 pdf(file = "linearresid2.pdf")
 par(mfcol=c(1,1))
 
 hist(rdf$r,main ="Fig.18    Hist of Residuals for lm", prob =1, 
      xlab = "Residuals for lm", col = "blue"); 
 lines(density(rdf$r),lwd = 4)
 abline(v = median(rdf$r),col = "Red",lty = 3,lwd = 4)
 abline(v = mean(rdf$r),col = "yellow",lty = 5,lwd = 4)
 legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))
 
dev.off()
graphics.off()

skew(rdf$r)
pdf(file = "Scatter_Res_LM2.pdf")

ggplot(data=rdf, aes(index,p)) +   labs(x="Predicted", y="Actual") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.19 Actuals vs Predicted")

ggplot(data=rdf, aes(unr,r)) +   labs(x="Unemployment Rate", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.20 Residuals vs Unemployment Rate")

ggplot(data=rdf, aes(cpi,r)) +   labs(x="Consumer Price MOM%", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.21 Residuals vs Consumer Price MoM%");

ggplot(data=rdf, aes(importts,r)) +   labs(x="US. Imports", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.22 Residuals vs US. Imports")

ggplot(data=rdf, aes(Int_rate,r)) +   labs(x="Federal Fund Rate", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.23 Residuals vs Federal Fund Rate")

ggplot(data=rdf, aes(Epi,r)) +   labs(x="Global Energy Index", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.24 Residuals vs Global Energy Index")

dev.off()
graphics.off()

 

 # gam regression
 library(gam)
 fit<-gam(index~s(unr,1)+s(importts)+s(Epi,1)+s(Int_rate,1),na.action=na.omit,data=spDF)
 par(mfcol=c(2,2)); plot.gam(fit)
 dwtest(fit)
 #residual dataframe
 rdf<-data.frame(spDF,r=fit$residuals,p=fit$fitted.values)

 pdf(file = "Gamresid1.pdf")
 par(mfcol=c(1,1))
 
 hist(rdf$r,main ="Fig.26    Hist of Residuals for GAM", prob =1, 
      xlab = "Residuals for lm", col = "blue"); 
 lines(density(rdf$r),lwd = 4)
 abline(v = median(rdf$r),col = "Red",lty = 3,lwd = 4)
 abline(v = mean(rdf$r),col = "yellow",lty = 5,lwd = 4)
 legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))
 skew(rdf$r)
 dev.off()
 graphics.off()
 
 
 pdf(file = "Scatter_Res_GAM1.pdf")
 
 ggplot(data=rdf, aes(index,p)) +   labs(x="Predicted", y="Actual") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.27 Actuals vs Predicted")
 
 ggplot(data=rdf, aes(unr,r)) +   labs(x="Unemployment Rate", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.28 Residuals vs Unemployment Rate")
 
 ggplot(data=rdf, aes(importts,r)) +   labs(x="US. Imports", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.29 Residuals vs US. Imports")
 
 ggplot(data=rdf, aes(Int_rate,r)) +   labs(x="Federal Fund Rate", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.30 Residuals vs Federal Fund Rate")
 
 ggplot(data=rdf, aes(Epi,r)) +   labs(x="Global Energy Index", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.31 Residuals vs Global Energy Index")
 
 ggplot(data=rdf, aes(cpi,r)) +   labs(x="GConsumer Price Index", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.32 Residuals vs Consumer Price Inex")
 
 
 dev.off()
 graphics.off()
 
 R_Square_GAM<- cor(rdf$index,rdf$p)^2
 