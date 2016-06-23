getwd()
setwd("D:/Users/skywayoo/Desktop/FDC/Executable/sensor and shapelet")
#19 variables
dat <- read.csv(file = "FDC_data.csv")
dat$label <- as.numeric(dat$label)
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
#create model
library(mxnet)
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=50)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=100)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=2)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")
device=mx.cpu()

#cv1 test    2~5 train
train <- data.matrix(rbind(cv2,cv3,cv4,cv5))
test <- data.matrix(rbind(cv1))

train.x=train[,-19]
train.y=as.numeric(train[,19])-1
test.x=test[,-19]
test.y=as.numeric(test[,19])-1
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax,train.x,train.y,
                                     device, num.round=500, array.batch.size=50,
                                     learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy)
preds <- predict(model,test.x,device)
pred <- max.col(t(preds)) - 1

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test.y[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test.y[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test.y[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test.y[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:1  train:2:5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"nnet result.txt",append = TRUE,col.names = F,row.names = F)



#cv2 test    1,3,4,5 train
train <- data.matrix(rbind(cv1,cv3,cv4,cv5))
test <- data.matrix(rbind(cv2))

train.x=train[,-19]
train.y=as.numeric(train[,19])-1
test.x=test[,-19]
test.y=as.numeric(test[,19])-1
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax,train.x,train.y,
                                     device, num.round=500, array.batch.size=50,
                                     learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy)
preds <- predict(model,test.x,device)
pred <- max.col(t(preds)) - 1

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test.y[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test.y[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test.y[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test.y[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:2  train:1,3,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"nnet result.txt",append = TRUE,col.names = F,row.names = F)


#cv3 test    1,2,4,5 train
train <-  data.matrix(rbind(cv1,cv2,cv4,cv5))
test <-  data.matrix(rbind(cv3))

train.x=train[,-19]
train.y=as.numeric(train[,19])-1
test.x=test[,-19]
test.y=as.numeric(test[,19])-1
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax,train.x,train.y,
                                     device, num.round=500, array.batch.size=50,
                                     learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy)
preds <- predict(model,test.x,device)

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test.y[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test.y[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test.y[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test.y[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:3  train:1,2,4,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"nnet result.txt",append = TRUE,col.names = F,row.names = F)


#cv4 test    1,2,3,5 train
train <-  data.matrix(rbind(cv1,cv2,cv3,cv5))
test <-  data.matrix(rbind(cv4))
train.x=train[,-19]
train.y=as.numeric(train[,19])-1
test.x=test[,-19]
test.y=as.numeric(test[,19])-1
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax,train.x,train.y,
                                     device, num.round=500, array.batch.size=50,
                                     learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy)
preds <- predict(model,test.x,device)
pred <- max.col(t(preds)) - 1

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test.y[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test.y[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test.y[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test.y[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:4  train:1,2,3,5","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"nnet result.txt",append = TRUE,col.names = F,row.names = F)


#cv5 test    1,2,3,4 train
train <-  data.matrix(rbind(cv1,cv2,cv3,cv4))
test <-  data.matrix(rbind(cv5))

train.x=train[,-19]
train.y=as.numeric(train[,19])-1
test.x=test[,-19]
test.y=as.numeric(test[,19])-1
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax,train.x,train.y,
                                     device, num.round=500, array.batch.size=50,
                                     learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy)
preds <- predict(model,test.x,device)
pred <- max.col(t(preds)) - 1

TP=FP=TN=FN=0
for(i in 1:length(pred)){
  if(pred[i]=='0'&& test.y[i]=='0')TP=TP+1
  if(pred[i]=='0'&& test.y[i]=='1')FP=FP+1
  if(pred[i]=='1'&& test.y[i]=='0')FN=FN+1
  if(pred[i]=='1'&& test.y[i]=='1')TN=TN+1
}
#accuracy=(TP+TN)/(TP+TN+FP+FN)
#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 

write.table(paste("5- folds cross validation","\n","test:5  train:1:4","\n","accuracy:",round((TP+TN)/(TP+TN+FP+FN),3),"\n","precision:",
                  round(TP/(TP+FP),3),"\n","sensitivity:",round(TP/(TP + FN),3),"\n","specificity:",round(TN/(FP + TN),3),"\n"
                  ,"F-score:",round(2*TP/(2*TP+FP+FN),3),"\n"),"nnet result.txt",append = TRUE,col.names = F,row.names = F)
