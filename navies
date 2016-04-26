library(e1071)
setwd("C:/Users/user/Desktop/Week 10/")
getwd()
train=read.table("train_data.csv", header=T, sep=",")
head(train)
test=read.table("test_data.csv", header=T, sep=",")
ctrain=scan("ctrain.txt",what=character())
ctest=scan("ctest.txt",what=character())
train <- cbind(train,ctrain)
head(train)
test <- cbind(test,ctest)
model = naiveBayes(ctrain~.,data=train)
result <- predict(model,test[,-51])  
TP=FP=TN=FN=0
  
for(i in 1:length(result)){
  if(result[i]=='L'&& ctest[i]=='L')TP=TP+1
  if(result[i]=='L'&& ctest[i]=='M')FP=FP+1
  if(result[i]=='M'&& ctest[i]=='L')FN=FN+1
  if(result[i]=='M'&& ctest[i]=='M')TN=TN+1
}
accuracy <- (TP+TN)/length(result)
#=accuracy
(TP+TN)/(TP+TN+FP+FN)

#precision=TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
#specificity = TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 
k.sen =TP/(TP+FN)
k.sen

