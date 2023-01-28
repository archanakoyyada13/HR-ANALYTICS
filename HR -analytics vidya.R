hrtrain=read.csv(file.choose(),na.strings = c("","NA"))
hrtest=read.csv(file.choose(),na.strings =c("","NA"))
# Check for Missing Values
sort(colSums(is.na(hrtrain)),decreasing = T)
sort(colSums(is.na(hrtest)),decreasing = T)
combinedf=rbind(hrtrain[-14],hrtest)
combinedf
table(combinedf$education,useNA = "always")
combinedf$education=ifelse(is.na(combinedf$education),
                           which.max(table(combinedf$education)),
                           combinedf$education)
table(combinedf$education,useNA = "always")
combinedf=rbind(hrtrain[-14],hrtest)
table(combinedf$education,useNA = "always")
combinedf$education=ifelse(is.na(combinedf$education),
                           names(which.max(table(combinedf$education))),
                           combinedf$education)
table(combinedf$education,useNA = "always")
table(combinedf$previous_year_rating)
table(combinedf$previous_year_rating,useNA = "always")
combinedf$previous_year_rating=ifelse(is.na(combinedf$previous_year_rating),
                                      names(which.max(table(combinedf$previous_year_rating))),
                                      combinedf$previous_year_rating)
combinedf$previous_year_rating=ifelse(is.na(combinedf$previous_year_rating),
                                      names(which.max(table(combinedf$previous_year_rating))),
                                      combinedf$previous_year_rating)
table(combinedf$previous_year_rating,useNA = "always")
# Split Dataframe into train and test
hrtraindf=combinedf[1:54808,]
hrtestdf=combinedf[54809:78298,]
hrtraindf["is_promoted"]=hrtrain$is_promoted
table(hrtraindf$is_promoted)
str(combinedf)
library(tidyverse)
library(dplyr)
chrcols=select_if(combinedf,is.character)
names(chrcols)
numcols=select_if(combinedf,is.numeric)
colnames(chrcols)
chrcolsdummy=lapply(chrcols,factor)
chrcolsdummy=data.frame(chrcolsdummy)
colnames(numcols)
numcols=numcols[-1]
numcols$KPIs_met..80.=as.factor(numcols$KPIs_met..80.)
numcols$awards_won.=as.factor(numcols$awards_won.)
combinedfclean=cbind(numcols,chrcolsdummy)
# Split Dataframe into train and test
hrtraindf=combinedfclean[1:54808,]
hrtestdf=combinedfclean[54809:78298,]
hrtraindf["is_promoted"]=hrtrain$is_promoted
# Build Binary Logistic Regression Model
logreg=glm(is_promoted~.,data=hrtraindf,family = "binomial")
logreg
summary(logreg)
#f1 score is 95%
#Null deviance: 31922  on 54807  degrees of freedom
#Residual deviance: 21373  on 54751  degrees of freedom
logregpredict=predict(logreg,type="response")
# Confusion Matrix
table(Actual=hrtraindf$is_promoted,Predicted=logregpredict>0.5)
(49808+1272)/(49808+332+3396+1272)
# Convert the logregpredict(predicted probability) to 0 & 1
logregpredict=ifelse(logregpredict>=0.50,1,0)
library(caret)
table(hrtraindf$is_promoted)
table(logregpredict)
confusionMatrix(as.factor(hrtraindf$is_promoted),
                as.factor(logregpredict))
# Predict on test data
logregtestpredict=predict(logreg,hrtestdf,type="response")
logregtestpredict
logregtestpredict=ifelse(logregtestpredict>=0.50,1,0)
logregtestpredict
write.csv(logregtestpredict,"logit.csv")
####################################
# Decision Tree
library(rpart)
library(rpart.plot)
hrrpart=rpart(is_promoted~.,data=hrtraindf)
summary(hrrpart)
hrrpart=rpart(as.factor(is_promoted)~.,data=hrtraindf)
summary(hrrpart)
rpart.plot(hrrpart)
rpart.rules(hrrpart)
parms=list(split="information")
hrrpart=rpart(as.factor(is_promoted)~.,data=hrtraindf,
              parms=list(split="information"))
summary(hrrpart)
rpart.plot(hrrpart,cex=0.50)
rpart.rules(hrrpart)
hrrpartinformationgain=rpart(as.factor(is_promoted)~.,
                             data=hrtraindf,
                             parms=list(split="information"))
summary(hrrpartinformationgain)
rpart.plot(hrrpartinformationgain,cex=0.50)
rpart.rules(hrrpart)
hrrpartgini=rpart(as.factor(is_promoted)~.,data=hrtraindf)
summary(hrrpartgini)
rpart.plot(hrrpartgini)
rpart.plot(hrrpartgini,cex=0.50)
rpart.rules(hrrpartgini)
######################### Random Forest
install.packages("randomForest")
library(randomForest)
hrRF=randomForest(as.factor(is_promoted)~.,data=hrtraindf,ntree=1000,do.trace=100)
print(hrRF)
plot(hrRF)
#################### Gradient Boosting Machine
install.packages("gbm")
library(gbm)
hrgbm=gbm(is_promoted~.,data=hrtraindf,distribution = "bernoulli", n.trees = 1000)
print(hrgbm)
hrgbm=gbm(is_promoted~.,data=hrtraindf,distribution = "bernoulli",
          n.trees = 1000,cv.folds = 2)
print(hrgbm)
best.iter=gbm.perf(hrgbm,method="cv")
best.iter
hrgbm=gbm(is_promoted~.,data=hrtraindf,distribution = "bernoulli",
          n.trees = 2000,cv.folds = 2)
print(hrgbm)
best.iter=gbm.perf(hrgbm,method="cv")
best.iter
hrgbmpredict=predict(hrgbm,hrtraindf)
table(hrtraindf$is_promoted,hrgbmpredict)
hrgbmpredict=predict(hrgbm,hrtraindf,type="response")
table(hrtraindf$is_promoted,hrgbmpredict>0.50)
(49923+1196)/(49923+217+3472+1196)#93.6%



