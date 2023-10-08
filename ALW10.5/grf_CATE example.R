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

predict <- predict(forest.W, estimate.variance=TRUE)
sqrt(predict$variance.estimates) #the se, investigate on the paper to see how it's calculated
predict$debiased.error
