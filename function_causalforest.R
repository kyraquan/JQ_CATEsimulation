function(data){
  
  require(grf)
  
  #splitting train and test 
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.6,0.4))
  data.train  <- data[sample, ]
  data.test   <- data[!sample, ]
  
  #create covariates matrix 
  X = as.matrix(data.train[,c(3:20)])
  W = data.train$trt 
  Y = data.train$yij
  clusters = data.train$schoolid 
  
  #Estimates of m(X) = E[Y|X]
  forest.Y <- regression_forest(X,Y)
  Y.hat <- predict(forest.W)$predictions
  
  #Estimates of the propensity score E[W|X]
  forest.W <- regression_forest(X,W,clusters=clusters)
  W.hat <- predict(forest.W)$predictions
  c.forest <- causal_forest(X,Y,W,Y.hat,W.hat)
  #Estimates the CATE E[Y(1)-Y(0)|X=x]
  tau.hat <- predict(c.forest)$predictions #the CATE
  

  
  #prediction on test 
  X.test = as.matrix(data.test[,c(3:20)])
  c.pred <- predict(c.forest,X.test)$prediction
  w.pred <- predict(forest.W,X.test)$prediction
  
  
  ### other results to save? 
  #save more than need to save 
  #'components to calculate the outcomes of the dissertation 
  #'extra things to calculate 
  #'ATE, SE, PS-score,
  #'dataset of each iteration, pred trt/crt/ps
  #'big list - keep the list in memory as r.data file until finish, name the r-data file with the name of conditions 
  #'write csv with append=TRUE, save the condition to know 
  #'not the raw data, but the cate for each person, trt in group, y_hat, summary of ate and se 
  #' SE for causal forest 
  #' in irt, the SE for each person, allowing to calculate the intervals 
  
  
}




######tests below 

library(grf)
# train a causal forest 
n=200; p =10
X= matrix(rnorm(n*p), n, p)
W = rbinom(n,1,0.5)
Y = pmax(X[,1],0)*W + X[,2]+pmin(X[,3],0)+rnorm(n)
causal.forest = causal_forest(X,Y,W)

#Estimate causal effects on new test data 
X.test = matrix(0,100,p)
X.test[,1] = seq(-2, 2, length.out=100)
predictions = predict(causal.forest, X.test)$predictions

class(X_cov)
X_cov = as.matrix(dataset1_full[,c(3:20)])
Y_cov = dataset1_full$yij11
W=dataset1_full$trt1
clusters = dataset1_full$schoolid
forest.Y <- causal_forest(X = X_cov, Y = Y_cov, W = W,clusters = clusters)
forest.y <- regression_forest(X = X_cov, Y = Y_cov,clusters = clusters)

#Estimate causal effects for the training data using out-of-bag prediction
oob.predictions =predict(causal.forest)$predictions

#Estimates of m(X) = E[Y|X]
forest.Y <- regression_forest(X,Y)
Y.hat <- predict(forest.W)$predictions
#Estimates of the propensity score E[W|X]
forest.W <- regression_forest(X,W)
W.hat <- predict(forest.W)$predictions

c.forest <- causal_forest(X,Y,W,Y.hat,W.hat)
#Estimates the CATE E[Y(1)-Y(0)|X=x]
tau.hat <- predict(c.forest)$predictions #the CATE

#E[Y|X,W = 0] conditional means
mu.hat.0 <- Y.hat - W.hat*tau.hat
#E[Y|X, W = 1]
mu.hat.1 <- Y.hat + (1-W.hat)*tau.hat