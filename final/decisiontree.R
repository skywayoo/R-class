getwd()
#72 variables
dat <- read.csv(file = "imdbbbb (1).csv")
dat$label <- as.factor(dat$label)
library(cvTools)
set.seed(5487)
cvfold <- cvFolds(722,K = 5,type = "random")

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
#decision tree
library(rpart)
tree_model <- rpart(label~.,train)
summary(tree_model)
plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# test data
tree_pred <- predict(tree_model,test,type="class")
tree_table.test <- table(test$label,tree_pred)

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

TP=FP=TN=FN=0
for(i in 1:length(tree_pred)){
  if(tree_pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(tree_pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(tree_pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(tree_pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:1  train:2:5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
            round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
            ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"desicion tree result.txt",append = TRUE,col.names = F,row.names = F)



#cv2 test    1,3,4,5 train
train <- rbind(cv1,cv3,cv4,cv5)
test <- rbind(cv2)
#decision tree
library(rpart)
tree_model <- rpart(label~.,train)
summary(tree_model)
plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# test data
tree_pred <- predict(tree_model,test,type="class")
tree_table.test <- table(test$label,tree_pred)

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

TP=FP=TN=FN=0
for(i in 1:length(tree_pred)){
  if(tree_pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(tree_pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(tree_pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(tree_pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:2  train:1,3,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"desicion tree result.txt",append = TRUE,col.names = F,row.names = F)



#cv3 test    1,2,4,5 train
train <- rbind(cv1,cv2,cv4,cv5)
test <- rbind(cv3)
#decision tree
library(rpart)
tree_model <- rpart(label~.,train)
summary(tree_model)
plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# test data
tree_pred <- predict(tree_model,test,type="class")
tree_table.test <- table(test$label,tree_pred)

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

TP=FP=TN=FN=0
for(i in 1:length(tree_pred)){
  if(tree_pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(tree_pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(tree_pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(tree_pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:3  train:1,2,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"desicion tree result.txt",append = TRUE,col.names = F,row.names = F)



#cv4 test    1,2,3,5 train
train <- rbind(cv1,cv2,cv3,cv5)
test <- rbind(cv4)
#decision tree
library(rpart)
tree_model <- rpart(label~.,train)
summary(tree_model)
plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# test data
tree_pred <- predict(tree_model,test,type="class")
tree_table.test <- table(test$label,tree_pred)

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

TP=FP=TN=FN=0
for(i in 1:length(tree_pred)){
  if(tree_pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(tree_pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(tree_pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(tree_pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:4  train:1,2,3,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"desicion tree result.txt",append = TRUE,col.names = F,row.names = F)


#cv5 test    1,2,3,4 train
train <- rbind(cv1,cv2,cv3,cv4)
test <- rbind(cv5)
#decision tree
library(rpart)
tree_model <- rpart(label~.,train)
summary(tree_model)
plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# test data
tree_pred <- predict(tree_model,test,type="class")
tree_table.test <- table(test$label,tree_pred)

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

TP=FP=TN=FN=0
for(i in 1:length(tree_pred)){
  if(tree_pred[i]=='0'&& test$label[i]=='0')TP=TP+1
  if(tree_pred[i]=='0'&& test$label[i]=='1')FP=FP+1
  if(tree_pred[i]=='1'&& test$label[i]=='0')FN=FN+1
  if(tree_pred[i]=='1'&& test$label[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:5  train:1,2,3,4","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"desicion tree result.txt",append = TRUE,col.names = F,row.names = F)


