test.train.split <-function(datatest){
  
  train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
  train.sample = train.sample[order(train.sample)]
  test.sample = 1:nrow(datatest)
  test.sample = test.sample[-c(train.sample)]
  
  data.train = datatest[train.sample,]
  data.test = datatest[test.sample,]
  data.train = as.data.frame(data.train)
  data.test = as.data.frame(data.test)
  
  datasplit <- list(data.train,data.test)
  return(datasplit)
  
}
test.train <- test.train.split(datatest)
data.train <- test.train[[1]]
data.test<- test.train[[2]]


train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(datatest)
test.sample = test.sample[-c(train.sample)]

data.train = datatest[train.sample,]
data.test = datatest[test.sample,]
data.train = as.data.frame(data.train)
data.test = as.data.frame(data.test)