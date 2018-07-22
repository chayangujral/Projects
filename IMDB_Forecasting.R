library(readxl)
library(ggplot2)
library(psych)
library(lmtest)
library(gam)
library(caret)# lots of dependencies try #install.packages("caret",repos = "http://cran.r-project.org", dependencies = c("Depends", "Imports", "Suggests"))
library(glmnet)
library(mlbench)
library(dplyr)

myfile <- read.csv("~/Rang/movie_metadata.csv")
View(myfile)
spDF<- myfile
spDF<- as.data.frame(spDF)
spDF<- na.omit(spDF)
dim(spDF)
names(spDF)
spDF$budget_m<- spDF$budget/1000000 # budget in million
spDF$gross_m<- spDF$gross/1000000 #gross_m in million

###Exploratory Data Analysis

#histograms

par(mfcol=c(1,1))
#pdf(file= "hist3.pdf")
par(mfcol=c(1,1))
hist(spDF$imdb_score,main ="Fig.1    Hist of Ratings", prob =1, xlab = "Ratings", col = "blue"); lines(density(spDF$imdb_score),lwd = 4)
abline(v = median(spDF$imdb_score),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$imdb_score),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$budget_m,main ="Fig.2  Hist of budget_m", prob =1, xlab = "budget_m", col = "blue"); lines(density(spDF$budget_m),lwd = 4)
abline(v = median(spDF$budget_m),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$budget_m),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$gross_m,main ="Fig.3    Hist of gross_m collection", prob =1, xlab = "gross_m Collection", col = "blue"); lines(density(spDF$gross_m),lwd = 4)
abline(v = median(spDF$gross_m),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$gross_m),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$title_year,main ="Fig.4   Hist of Title years", prob =1, xlab = "Title year", col = "blue"); lines(density(spDF$title_year),lwd = 4)
abline(v = median(spDF$title_year),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$title_year),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$actor_1_facebook_likes,main ="Fig.5   Hist of Lead actor's fb likes", prob =1, xlab = "Lead Actor FB likes", col = "blue"); lines(density(spDF$actor_1_facebook_likes),lwd = 4)
abline(v = median(spDF$actor_1_facebook_likes),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$actor_1_facebook_likes),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$director_facebook_likes,main ="Fig.6   Hist of Director's fb likes", prob =1, xlab = "Director's FB likes", col = "blue"); lines(density(spDF$director_facebook_likes),lwd = 4)
abline(v = median(spDF$director_facebook_likes),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$director_facebook_likes),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$movie_facebook_likes,main ="Fig.7   Hist of Movie's fb likes", prob =1, xlab = "Movie's FB likes", col = "blue"); lines(density(spDF$movie_facebook_likes),lwd = 4)
abline(v = median(spDF$movie_facebook_likes),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$movie_facebook_likes),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$duration,main ="Fig.8   Hist of Duration", prob =1, xlab = "Duration", col = "blue"); lines(density(spDF$duration),lwd = 4)
abline(v = median(spDF$duration),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$duration),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))

hist(spDF$num_voted_users,main ="Fig.9   Hist of num_voted_users", prob =1, xlab = "num_voted_users", col = "blue"); lines(density(spDF$num_voted_users),lwd = 4)
abline(v = median(spDF$num_voted_users),col = "Red",lty = 3,lwd = 4)
abline(v = mean(spDF$num_voted_users),col = "yellow",lty = 5,lwd = 4)
legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))


#scatterplots(dependednt vs all independent variables)
#pdf(file = "scatterplots3.pdf")
par(mfcol = c(1,1))

ggplot(data=spDF, aes(budget_m,imdb_score)) +   labs(x="Budget in mil", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.10 imdb_score vs Budget")

ggplot(data=spDF, aes(gross_m,imdb_score)) +   labs(x="Gross in mil", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.11 imdb_score vs US. Gross")

ggplot(data=spDF, aes(actor_1_facebook_likes,imdb_score)) +   labs(x="actor_1_facebook_likes", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.12 imdb_score vs actor_1_facebook_likes%")

ggplot(data=spDF, aes(director_facebook_likes,imdb_score)) +   labs(x="director_facebook_likes", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.13 imdb_score vs director_facebook_likes")

ggplot(data=spDF, aes(movie_facebook_likes,imdb_score)) +   labs(x="movie_facebook_likes", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.14 imdb_score vs movie_facebook_likes")

ggplot(data=spDF, aes(num_voted_users,imdb_score)) +   labs(x="num_voted_users", y="imdb_score") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.15 imdb_score vs num_voted_users")

#dev.off()
#graphics.off()

#descriptive statistics

names(spDF)
desc<- round(describe(spDF[,c("imdb_score","budget_m","gross_m","actor_1_facebook_likes","director_facebook_likes","movie_facebook_likes","num_voted_users")]),3)
 

##How many movies with same Genere
g1 = spDF %>% select(genres) %>% 
  group_by(genres) %>% summarise(appear.count=n()) %>% arrange(desc(appear.count))

##How many movies directed by every director

d1<- spDF %>% select(director_name) %>% 
  group_by(director_name) %>% summarise(appear.count=n()) %>% arrange(desc(appear.count))

##How many movies by each actor as lead

a1<- spDF %>% select(actor_1_name) %>% 
  group_by(actor_1_name) %>% summarise(appear.count=n()) %>% arrange(desc(appear.count))

#Gross collection of each Genres

g2 = as.data.frame(spDF %>% select(genres,gross_m) %>% 
  group_by(genres) %>% summarise(sum(gross_m)))


#Highest Grossing Directior
d2 = as.data.frame(spDF %>% select(director_name,gross_m) %>% 
                     group_by(director_name) %>% summarise(sum(gross_m)))


#Highest Grossing Actor
a2 = as.data.frame(spDF %>% select(actor_1_name,gross_m) %>% 
                          group_by(actor_1_name) %>% summarise(sum(gross_m)))


 #correlation matrix
 cor<- round(cor(spDF[,c("imdb_score","budget_m","gross_m","actor_1_facebook_likes","director_facebook_likes","movie_facebook_likes","num_voted_users")]),3)
 

 #  regression (linear and parametric)
 
 spDF_v = spDF[, c("imdb_score",
                   "director_facebook_likes", 
                   "cast_total_facebook_likes", 
                   "actor_1_facebook_likes",
                   "actor_2_facebook_likes",
                   "actor_3_facebook_likes",
                   "movie_facebook_likes", 
                   "facenumber_in_poster",
                   "gross",
                   "budget")]

 
 # Training Set
 set.seed(1234)
 ds<- sample(2, nrow(mvs), replace = T, prob = c(0.7, 0.3))
 test = spDF_v[ds==2,]
 train = spDF_v[ds==1,]
 
 custom <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        verboseIter = T)
 
 lm<- train(imdb_score ~ .,
            train,
            method = 'lm',
            trControl = custom)
 fit<- lm(imdb_score~. , data = spDF_v)
 
 
 grid = 10^seq(5, -2, length = 100)
 ##Ridge Regression
 ridge<- train(imdb_score ~ .,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = grid),
               trControl = custom
 )
 
 
 
 #Lasso
 
 lasso<- train(imdb_score ~ .,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = grid),
               trControl = custom
 )
 
 
 
 #EN regression
 
 en<- train(imdb_score ~ .,
            train,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0,1,length = 10),
                                   lambda = grid),
            trControl = custom
 )
 
 #Results
 summary(fit)
 summary(lm)
 lm$results
 plot(lm$finalModel)
 summary(ridge)
 plot(ridge) # value of lambad greater than 0.221 increases rmse upto 1.08 before getting constant.
 ridge
 plot(ridge$finalModel,xvar = "lambda",lable =T)
 plot(ridge$finalModel,xvar = "dev",lable =T)
 plot(varImp(ridge,scale = F))
 
 lasso
 plot(lasso)
 plot(lasso$finalModel,xvar = "lambda")
 plot(lasso$finalModel,xvar = "dev")
 plot(varImp(lasso,scale = F))
 en
 
 plot(en)
 plot(en$finalModel,xvar = "lambda")
 plot(en$finalModel,xvar = "dev")
 plot(varImp(en,scale = F))
 
 
 model_list<- list(Linear = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
 
 res<- resamples(model_list)
 
 summary(res)
 
 
 
 #Trying parametric and non parametric using statistical analysis to increase R-squared
 
 names(spDF)
 fit<-lm(imdb_score~.,data=spDF_v,na.action=na.omit)
 summary(fit)
 dwtest(fit)
 
 #removing non significant variables(removing budget and actor1 fb likes)
 spDF_v_e<- spDF_v[,1:9]
 names(spDF_v_e)
 fit1<-lm(imdb_score~.,data=spDF_v_e,na.action=na.omit)
 summary(fit1)
 dwtest(fit1)
 
 #Linear residual dataframe
 rdf<-data.frame(spDF_v_e,r=fit1$residuals,p=fit1$fitted.values)
 #pdf(file = "linearresid2.pdf")
 par(mfcol=c(1,1))
 
 hist(rdf$r,main ="Fig.18    Hist of Residuals for lm", prob =1, 
      xlab = "Residuals for lm", col = "blue"); 
 lines(density(rdf$r),lwd = 4)
 abline(v = median(rdf$r),col = "Red",lty = 3,lwd = 4)
 abline(v = mean(rdf$r),col = "yellow",lty = 5,lwd = 4)
 legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))
 
 #Residuals follow a bell shaped curve signifying accuracy of the modle
 
#dev.off()
#graphics.off()

skew(rdf$r)
#pdf(file = "Scatter_Res_LM2.pdf")

ggplot(data=rdf, aes(imdb_score,p)) +   labs(x="Predicted", y="Actual") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.16 Actuals vs Predicted")# straight line shows residuals and predicitive are highly corelated which is good for a modle

#Residuals scatter plots a flat line show that there is no autocorelation in slelective data


ggplot(data=rdf, aes(gross_m,r)) +   labs(x="Gross in mil", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.17 Residuals vs US. Gross")

ggplot(data=rdf, aes(director_facebook_likes,r)) +   labs(x="director_facebook_likes", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.18 Residuals vs director_facebook_likes")

ggplot(data=rdf, aes(movie_facebook_likes,r)) +   labs(x="movie_facebook_likes", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.19 Residuals vs movie_facebook_likes")

ggplot(data=rdf, aes(num_voted_users,r)) +   labs(x="num_voted_users", y="Residuals") +
  geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.20 Residuals vs num_voted_users")

names(spDF)
 # gam regression non parametric non linear
 library(gam)

#selecting and changing degree of freedom for statisticaly significant variables
fit_s<- gam(imdb_score~s(gross_m,2)+s(director_facebook_likes,1)+s(movie_facebook_likes,4)+s(num_voted_users,1),data=spDF,na.action=na.omit)
 par(mfcol=c(2,2)); plot.gam(fit_s)
 dwtest(fit_s)#any where closer to 2 is considered good model\
 #here movie likes seems to be insignificant even after changing the degree of freedom, trying again by removing it
 
 fit_s1<- gam(imdb_score~s(gross_m,2)+s(director_facebook_likes,1)+s(num_voted_users,1),data=spDF,na.action=na.omit)
 par(mfcol=c(2,2)); plot.gam(fit_s1)
 dwtest(fit_s1)
 
 #residual dataframe
 rdf_s<-data.frame(spDF,r=fit_s1$residuals,p=fit_s1$fitted.values)

 #pdf(file = "Gamresid1.pdf")
 par(mfcol=c(1,1))
 
 hist(rdf_s$r,main ="Fig.21    Hist of Residuals for GAM", prob =1, 
      xlab = "Residuals for lm", col = "blue"); 
 lines(density(rdf_s$r),lwd = 4)
 abline(v = median(rdf_s$r),col = "Red",lty = 3,lwd = 4)
 abline(v = mean(rdf_s$r),col = "yellow",lty = 5,lwd = 4)
 legend("topright", lty = c(3, 5), col = c("red","yellow"), legend = c("Median","Mean"))
 
 
 skew(rdf_s$r)
 #dev.off()
 #graphics.off()
 
 
 #pdf(file = "Scatter_Res_GAM1.pdf")
 
 ggplot(data=rdf_s, aes(imdb_score,p)) +   labs(x="Predicted", y="Actual") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.22 Actuals vs Predicted")# straight line shows residuals and predicitive are highly corelated which is good for a modle
 
 #Residuals scatter plots a flat line show that there is no autocorelation in slelective data
 
 
 ggplot(data=rdf_s, aes(gross_m,r)) +   labs(x="Gross in mil", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.23 Residuals vs US. Gross")
 
 ggplot(data=rdf_s, aes(director_facebook_likes,r)) +   labs(x="director_facebook_likes", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.24 Residuals vs director_facebook_likes")
 
 #ggplot(data=rdf_s, aes(movie_facebook_likes,r)) +   labs(x="movie_facebook_likes", y="Residuals") +
  # geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.25 Residuals vs movie_facebook_likes")
 
 ggplot(data=rdf_s, aes(num_voted_users,r)) +   labs(x="num_voted_users", y="Residuals") +
   geom_point() +stat_smooth(method = "lm") +ggtitle("Fig.25 Residuals vs num_voted_users")
 
 R_Square_GAM<- cor(rdf_s$imdb_score,rdf_s$p)^2
 
 rdf_s
 