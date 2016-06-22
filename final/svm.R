getwd()
setwd("D:/Users/skywayoo/Desktop/FDC/Executable/sensor and shapelet")
#19 variables
dat <- read.csv(file = "FDC_data.csv")
dat$label <- as.factor(dat$label)
library(cvTools)
set.seed(5487)
cvfold <- cvFolds(188,K = 5,type = "random")

#each fold
index_1 <- cvfold$subsets[cvfold$which==1,]
index_2 <- cvfold$subsets[cvfold$which==2,]
index_3 <- cvfold$subsets[cvfold$which==3,]
index_4 <- cvfold$subsets[cvfold$which==4,]
index_5 <- cvfold$subsets[cvfold$which==5,]
cv1 <- dat[index_1,] #38
cv2 <- dat[index_2,] #38
cv3 <- dat[index_3,] #38
cv4 <- dat[index_4,] #37
cv5 <- dat[index_5,] #37

#classify through 5 folds cross validation


#cv1 test    2~5 train
train <- rbind(cv2,cv3,cv4,cv5)
test <- rbind(cv1)

#svm
library(e1071)
#search beat gmma and cost
tuned <- tune.svm(label~.,data=train,gamma=10^(-2:2),cost=10^(-2:2))
model <- svm(label~ .,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
pred <- predict(model,test[,-19])
table <- table(pred=pred,true=train[,19])


TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:1  train:2:5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"svm result.txt",append = TRUE,col.names = F,row.names = F)



#cv2 test    1,3,4,5 train
train <- rbind(cv1,cv3,cv4,cv5)
test <- rbind(cv2)

#svm
library(e1071)
#search beat gmma and cost
tuned <- tune.svm(label~.,data=train,gamma=10^(-2:2),cost=10^(-2:2))
model <- svm(label~ .,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
pred <- predict(model,test[,-19])
table <- table(pred=pred,true=train[,19])

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:2  train:1,3,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"svm result.txt",append = TRUE,col.names = F,row.names = F)


#cv3 test    1,2,4,5 train
train <- rbind(cv1,cv2,cv4,cv5)
test <- rbind(cv3)

#svm
library(e1071)
#search beat gmma and cost
tuned <- tune.svm(label~.,data=train,gamma=10^(-2:2),cost=10^(-2:2))
model <- svm(label~ .,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
pred <- predict(model,test[,-19])
table <- table(pred=pred,true=train[,19])

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:3  train:1,2,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"svm result.txt",append = TRUE,col.names = F,row.names = F)


#cv4 test    1,2,3,5 train
train <- rbind(cv1,cv2,cv3,cv5)
test <- rbind(cv4)
#svm
library(e1071)
#search beat gmma and cost
tuned <- tune.svm(label~.,data=train,gamma=10^(-2:2),cost=10^(-2:2))
model <- svm(label~ .,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
pred <- predict(model,test[,-19])
table <- table(pred=pred,true=train[,19])

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:4  train:1,2,3,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"svm result.txt",append = TRUE,col.names = F,row.names = F)


#cv5 test    1,2,3,4 train
train <- rbind(cv1,cv2,cv3,cv4)
test <- rbind(cv5)
#svm
library(e1071)
#search beat gmma and cost
tuned <- tune.svm(label~.,data=train,gamma=10^(-2:2),cost=10^(-2:2))
model <- svm(label~ .,data=train,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
pred <- predict(model,test[,-19])
table <- table(pred=pred,true=test[,19])
length(pred)
TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:5  train:1:4","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"svm result.txt",append = TRUE,col.names = F,row.names = F)
