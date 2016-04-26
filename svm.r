library(e1071)

set.seed(2)
n=0.1*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train=iris[-test.index,]
iris.test=iris[test.index,]
bestgc=tune.svm(iris.train[,1:4],iris.train[,5] , 
       gamma = 2^(-15:3),cost = 2^(-5:15),scale=F)#tune cost and gamma
bestg=bestgc[[1]][[1]] #best gamma
bestc=bestgc[[1]][[2]] #best cost
bbmodel=svm(iris.train[,1:4],iris.train[,5], 
     gamma=bestg,cost=bestc,scale=F) #build model scale=normalize 單位一樣可以不用 不一樣一定要用

svm.result=predict(bbmodel, iris.test[,1:4], scale=F) #predict
mean(svm.result==iris.test[,5]) 



TP=FP=TN=FN=0

for(i in 1:length(svm.result)){
  if(result[i]=='L'&& ctest[i]=='L')TP=TP+1
  if(result[i]=='L'&& ctest[i]=='M')FP=FP+1
  if(result[i]=='M'&& ctest[i]=='L')FN=FN+1
  if(result[i]=='M'&& ctest[i]=='M')TN=TN+1
}
accuracy <- (TP+TN)/length(svm.result)
#=accuracy
(TP+TN)/(TP+TN+FP+FN)

#precision=TP / (TP + FP) 
TP / (TP + FP) 
#sensitivity = TP / (TP + FN) 
TP / (TP + FN) 
#specificity = TN / (FP + TN)
TN / (FP + TN)
#F-score = 2*TP /(2*TP + FP + FN) 
2*TP /(2*TP + FP + FN) 

