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
