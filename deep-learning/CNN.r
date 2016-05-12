#install the package
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")


library(mxnet)
# Data preparation
train<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255)
test_org<-test
test<-test[,-1]
test<-t(test/255)
dim(test)
table(train.y)

# Convolutional NN
data <- mx.symbol.Variable('data')
# first conv
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
                             kernel=c(2,2), stride=c(2,2))
# second conv
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
                             kernel=c(2,2), stride=c(2,2))

# first fullc
flatten <- mx.symbol.Flatten(data=pool2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
# second fullc
fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test
dim(test.array) <- c(28, 28, 1, ncol(test))

mx.set.seed(0)

model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                       ctx=mx.cpu(), num.round=5, array.batch.size=100,
                                       learning.rate=0.05, momentum=0.9, wd=0.00001,
                                       eval.metric=mx.metric.accuracy,
                                       epoch.end.callback=mx.callback.log.train.metric(100))

#group some layers
layers <- mx.symbol.Group(c(conv1, pool1, tanh2,pool2,lenet))
executor <- mx.simple.bind(symbol=layers, data=dim(test.array), ctx=mx.cpu())
#get weight, bias, outputs
#update the parameters
mx.exec.update.arg.arrays(executor, model$arg.params, match.name=TRUE)
mx.exec.update.aux.arrays(executor, model$aux.params, match.name=TRUE)
#give the data
mx.exec.update.arg.arrays(executor, list(data=mx.nd.array(test.array)), match.name=TRUE)
#forward pass to get the paremeters
mx.exec.forward(executor,is.train = F)

as.array(executor$outputs$convolution0_output)[,,1,1]

#visualization

#model
print(graph.viz(lenet$as.json(), graph.title = "Computation graph",graph.title.font.name = "Helvetica",
                graph.title.font.size = 100,graph.width.px = 1000, graph.height.px = 2000))
                
#plot last image is 9
textcolor <- c("#c0c0c0","#a9a9a9","#808080","#696969","#000000")
image(test.array[,,1,1000][ncol(test.array[,,1,1000]):1,],col=textcolor,ylim = c(1:0),xlim = c(1:0))
#plot 0-9 each 10
par(mfrow=c(10,10),mar=c(0.1,0.1,0.1,0.1))
for(i in seq(1,1000,by = 10)){
textcolor <- c("#c0c0c0","#a9a9a9","#808080","#696969","#000000")
image(test.array[,,1,i][ncol(test.array[,,1,i]):1,],col=textcolor,ylim = c(1:0),xlim = c(1:0),axes=F)
}


#plot 0-9 each 10 after 1'st convolution
par(mfrow=c(10,10),mar=c(0.1,0.1,0.1,0.1))
for(i in seq(1,1000,by = 10)){
        textcolor <- c("#c0c0c0","#a9a9a9","#808080","#696969","#000000")
        image(as.array(executor$outputs$convolution0_output)[,,1,i][ncol(as.array(executor$outputs$convolution0_output)[,,1,i]):1,],col=textcolor,ylim = c(1:0),xlim = c(1:0),axes=F)
}

