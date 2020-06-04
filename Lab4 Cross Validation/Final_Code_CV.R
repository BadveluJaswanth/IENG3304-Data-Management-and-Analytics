library(tidyverse) 
library(plyr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(scales)
library(readr)
require(GGally)
library(caTools)
library(corrplot)
library(e1071)
library(caret)
library(klaR)
library(randomForest)
library(Hmisc)
library(ROCR)
library(pROC)
library(party)
library(matrixStats)



data_num <- read_csv("C:/Users/Jaswanth/Desktop/DataManagement/Lab3 Classification/Data/data_num.csv")

data_num<- as.data.frame(data_num)

#view(data_num)


#Correlation
corrplot(cor(data_num),method = "color")

#As factor

data_num$Death <- as.factor(data_num$Death)
#Train Test
set.seed(101) 
split = sample.split(data_num$Death, SplitRatio = 0.70) 
train = subset(data_num, split == TRUE) 
test = subset(data_num, split == FALSE) 
train[-3] = scale(train[-3])
test[-3] = scale(test[-3])


model = rep(0, 7)
model[1] <- "Death~ Age_group"
model[2] <- "Death~ Age_group+Hospital_status"
model[3] <- "Death~ Age_group+Asymptomatic+Hospital_status"
model[4] <- "Death~ Age_group+Region+Hospital_status+Asymptomatic"
model[5] <- "Death~ Age_group+Tramssion+Hospital_status+Asymptomatic+Region"
model[6] <- "Death~ Age_group+Tramssion+Hospital_status+Asymptomatic+Occupation+Region"
model[7] <- "Death~ Age_group+Tramssion+Hospital_status+Asymptomatic+Gender+Occupation+Region"

#SVM
total_svm = rep(0,7)
j=1
for(j in 1:7){
svm_model <-svm(eval(parse(text=paste(model[j]))), 
                data = train, 
                type = 'C-classification', 
                kernel = 'linear')
y_pred_svm = predict(svm_model, test) 
cm=table(y_pred_svm, test$Death)
error_svm= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
total_svm[j]=total_svm[j]+error_svm
}
total_svm
total_rf=as.data.frame(total_svm)
#view(total_rf)
b6<-total_rf
a=c(1,2,3,4,5,6,7)
ds1<-data.frame(a,b6)
##view(ds1)
ggplot(ds1,aes(x=a, y=total_svm)) + 
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a)+labs(x="Models",
                                                        y="Error Rate",title ='SVM')

#SVM Cross Validation
folds = createFolds(train$Death, k = 5)
svm1 = lapply(folds, function(x) {
  totalError = rep(0, 7)
  avgError = rep(0,7)
  j=1
  for(j in 1:7){
    training_fold = train[-x,]
    test_fold = train[x,]
    random_forest <- svm(eval(parse(text=paste(model[j]))), data=training_fold,type = 'C-classification', 
                         kernel = 'linear')
    y_pred = predict(random_forest, newdata = test_fold[-3])
    cm = table(test_fold[,3], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    error_svm_cv= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    totalError[j]= totalError[j]+error_svm_cv
  }
  ##view(totalError)
  #plot(totalError,type="l")
  return(totalError)
})
##view(model[j])
#plot(as.numeric(cv),type="l")
svm1
svm1=as.data.frame(svm1)
##view(svm1)

b1=data.frame(b1=rowMeans(svm1[,-1]))
c<-svm1 %>%mutate(stDev = apply(.[(1:4)],1,sd))
c1<-c$stDev
c1
a1=c(1,2,3,4,5,6,7)
#data.frame(dt,stringsAsFactors = TRUE)
svm2<-data.frame(a1,b1,c1)
#plot(x=x,y=avg)
##view(cv)
##view(svm2)
ggplot(svm2,aes(x=a1, y=b1)) + 
  geom_errorbar(aes(ymin=b1-c1, ymax=b1+c1), width=0.5, position=position_dodge(0.5),color=a1,size=1) +
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a1)+labs(x="Models",
                                                        y="Average Error with 5 folds of Cv",title ='SVM')

#Logistic Regression
total_lr = rep(0,7)
j=1
for(j in 1:7){
logisticregression <- glm(eval(parse(text=paste(model[j]))), data=train,family=binomial)
preds <- predict(logisticregression,test)
cm= table(preds,as.factor(test$Death))
error_lr= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
total_lr[j]=total_lr[j]+error_lr
}
total_lr
total_lr=as.data.frame(total_lr)
##view(total_lr)
b7<-total_lr
a=c(1,2,3,4,5,6,7)
dl1<-data.frame(a,b7)
##view(dl1)
ggplot(dl1,aes(x=a, y=total_lr)) + 
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a)+labs(x="Models",
                                                        y="Error Rate",title ='Logistic Regression')

#Logistic Regression CV
folds = createFolds(train$Death, k = 5)
lr1 = lapply(folds, function(x) {
  totalError = rep(0, 7)
  avgError = rep(0,7)
  j=1
  for(j in 1:7){
    training_fold = train[-x,]
    test_fold = train[x,]
    logistic <- glm(eval(parse(text=paste(model[j]))), data=training_fold,family=binomial)
    y_pred = predict(logistic, newdata = test_fold[-3])
    cm = table(test_fold[,3], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    error_svm_cv= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    totalError[j]= totalError[j]+error_svm_cv
  }
  ##view(totalError)
  #plot(totalError,type="l")
  return(totalError)
})
lr1
lr1=as.data.frame(lr1)
##view(lr1)

b2=data.frame(b2=rowMeans(lr1[,-1]))
c<-lr1 %>%mutate(stDev = apply(.[(1:4)],1,sd))
c2<-c$stDev
c2
a2=c(1,2,3,4,5,6,7)
#data.frame(dt,stringsAsFactors = TRUE)
lr2<-data.frame(a2,b2,c2)
##view(lr2)
ggplot(lr2,aes(x=a2, y=b2)) + 
  geom_errorbar(aes(ymin=b2-c2, ymax=b2+c2), width=0.5, position=position_dodge(0.5),color=a2,size=1) +
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a2)+labs(x="Models",
                                                         y="Average Error with 5 folds of Cv",title ='Logistic Regression')

#Decision Tree with 3
total_dt = rep(0,7)
j=1
for(j in 1:7){
Decision_tree <- ctree(eval(parse(text=paste(model[j]))), data=train)
pred_dt <- predict(Decision_tree, test)
cm=table(pred_dt, test$Death)
error_dt= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
total_dt[j]=total_dt[j]+error_dt
}
total_dt
total_dt=as.data.frame(total_dt)
##view(total_lr)
b8<-total_dt
a=c(1,2,3,4,5,6,7)
dt1<-data.frame(a,b8)
##view(dl1)
ggplot(dt1,aes(x=a, y=total_dt)) + 
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a)+labs(x="Models",
                                                        y="Error Rate",title ='Decision Tree')

#Decision Tree CV

folds = createFolds(train$Death, k = 5)
dt = lapply(folds, function(x) {
  totalError = rep(0, 7)
  avgError = rep(0,7)
  j=1
  for(j in 1:7){
  training_fold = train[-x,]
  test_fold = train[x,]
  Decision_tree <- ctree(eval(parse(text=paste(model[j]))), data=training_fold)
  y_pred = predict(Decision_tree, newdata = test_fold[-3])
  cm = table(test_fold[,3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  error_dt_cv= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  totalError[j]= totalError[j]+error_dt_cv
  }
  ##view(totalError)
  #plot(totalError,type="l")
  return(totalError)
})
##view(model[j])
#plot(as.numeric(cv),type="l")
dt
dt=as.data.frame(dt)
#view(dt)

b=data.frame(b=rowMeans(dt[,-1]))
c<-dt %>%mutate(stDev = apply(.[(1:4)],1,sd))
c<-c$stDev
c
a=c(1,2,3,4,5,6,7)
#data.frame(dt,stringsAsFactors = TRUE)
df<-data.frame(a,b,c)
#plot(x=x,y=avg)
##view(cv)
##view(df)
ggplot(df,aes(x=a, y=b)) + 
  geom_errorbar(aes(ymin=b-c, ymax=b+c), width=0.5, position=position_dodge(0.5),color=a,size=1) +
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a)+labs(x="Models",
                                      y="Average Error with 5 folds of Cv",title ='Decision Tree with CV')


#Random Forest
total_rf = rep(0, 7)
j=1
for(j in 1:7){
  Random_Forest <- train(eval(parse(text=paste(model[j]))), data=train, method='rf')
  pred_rf <- predict(Random_Forest, test)
  #confusionMatrix(pred_rf, test$Death)
  cm= table(pred_rf,as.factor(test$Death))
  error_rf= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  total_rf[j]=total_rf[j]+error_rf
}
total_rf
total_rf=as.data.frame(total_rf)
##view(total_rf)
b5<-total_rf
a=c(1,2,3,4,5,6,7)
dr1<-data.frame(a,b5)
##view(dr1)
ggplot(dr1,aes(x=a, y=total_rf)) + 
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a)+labs(x="Models",
                                                         y="Error Rate",title ='Random Forest')

#Random Forest CV
folds = createFolds(train$Death, k = 5)
rf = lapply(folds, function(x) {
  totalError = rep(0, 7)
  avgError = rep(0,7)
  j=1
  for(j in 1:7){
    training_fold = train[-x,]
    test_fold = train[x,]
    random_forest <- train(eval(parse(text=paste(model[j]))), data=training_fold, method='rf')
    y_pred = predict(random_forest, newdata = test_fold[-3])
    cm = table(test_fold[,3], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    error_dt_cv= 1- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    totalError[j]= totalError[j]+error_dt_cv
  }
  ##view(totalError)
  #plot(totalError,type="l")
  return(totalError)
})
##view(model[j])
#plot(as.numeric(cv),type="l")
rf
rf=as.data.frame(rf)
##view(rf)

b3=data.frame(b3=rowMeans(rf[,-1]))
c<-rf %>%mutate(stDev = apply(.[(1:4)],1,sd))
c3<-c$stDev
c3
a3=c(1,2,3,4,5,6,7)
#data.frame(dt,stringsAsFactors = TRUE)
dr<-data.frame(a3,b3,c3)
#plot(x=x,y=avg)
##view(cv)
##view(dr)
ggplot(dr,aes(x=a3, y=b3)) + 
  geom_errorbar(aes(ymin=b3-c3, ymax=b3+c3), width=0.5, position=position_dodge(0.5),color=a3,size=1) +
  geom_line(position=position_dodge(0.5),size=1) +theme_minimal()+
  geom_point(position=position_dodge(0.5),color=a3)+labs(x="Models",
                                                        y="Average Error with 5 folds of Cv",title ='Random Forest with CV')







