# Group some output layers for visual analysis
out <- mx.symbol.Group(c(conv1, pool1, tanh2,pool2,lenet))
executor <- mx.simple.bind(symbol=out, data=dim(test.array), ctx=mx.cpu())
#output
names(executor$ref.outputs)
executor$arg.arrays$convolution0_weight
executor$arg.arrays$data
executor$aux.arrays
executor$grad.arrays
mx.exec.forward(executor,is.train = T)
mx.exec.update.arg.arrays(executor, model$arg.params, match.name=TRUE)
mx.exec.update.aux.arrays(executor, model$aux.params, match.name=TRUE)
mx.exec.forward(executor,is.train = T)
as.array(executor$outputs$convolution0_output)[,,6,1]
