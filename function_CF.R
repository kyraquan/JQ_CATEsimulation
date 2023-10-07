covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")

CF_est <- function(data.train, data.test, covariates){
  require(grf)
  
  #obtain PS first 
  cf.train.ps.forest <- regression_forest(X=as.matrix(data.train[,covariates]),
                                   Y=data.train$trt,
                                   clusters = data.train$School_id)
  cf.train.ps <- predict(cf.train.ps.forest)$predictions #save propensity score for train 
  
  #Estimates of m(X) = E[Y|X]
  cf.train.yhat.forest <- regression_forest(X=as.matrix(data.train[,covariates]),
                                     Y=data.train$yij,
                                     clusters = data.train$School_id)
  cf.train.yhat <- predict(cf.train.yhat.forest)$predictions
  
  #estimate CATE of train
  cf.train.cate.forest <- causal_forest(X=as.matrix(data.train[,covariates]),
                                 Y=data.train$yij,
                                 W=data.train$trt,
                                 Y.hat = cf.train.yhat,
                                 W.hat = cf.train.ps,
                                 clusters = data.train$School_id,
                                 honesty = TRUE) 
                                 
  #Estimates the CATE E[Y(1)-Y(0)|X=x]
  cf.train.cate <- predict(cf.train.cate.forest)$predictions #save trained CATE
  
  #?predict.causal_forest
  #X.test = as.matrix(data.test[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")])
 cf.test.ps <- predict(cf.train.ps.forest,newdata = data.test[,covariates],clusters=data.test$School_id)$predictions
 cf.test.cate<- predict(cf.train.cate.forest, newdata = data.test[,covariates],W.hat=cf.test.ps,clusters=data.test$School_id)$predictions
 
 
 #obtain results
 train.id <- data.train$Stu_id
 test.id <- data.test$Stu_id
 cf.train.ps <- cbind(train.id,cf.train.ps)
 cf.train.cate <- cbind(train.id,cf.train.cate)
 cf.test.ps <- cbind(test.id,cf.test.ps)
 cf.test.cate <- cbind(test.id,cf.test.cate)
  

  cf.result <- list(cf.train.cate,cf.test.cate,cf.train.ps,cf.test.ps)
  
  #use the same method as SL for test.data ATE 
  
  return(cf.result)
}

simulation.start = proc.time()
CFtest <- CF_est(data.train, data.test,covariates)
simulation.edu=proc.time()
total.time = simulation.edu-simulation.start 
for (i in 1:4) {
  print(CFtest[[i]][1:5,])
  
}
#user  system elapsed 
# 2.667   0.062   0.379 
#this is the T-learner 