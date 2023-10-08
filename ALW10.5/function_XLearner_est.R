#covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")










#first list, CATE for train; second list, CATE for test 
XLearner_est <- function(data.train, data.test, datatest,covariates,learner){
  require(SuperLearner) 
  
  #preparations
  learners <- c("SL.bartMachine", "SL.cforest")
  #covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")
  SL.tau.train <- matrix(NA,nrow(data.train),2)
  SL.tau.test <- matrix(NA,nrow(data.train),2)
  numberrow <- nrow(data.train)+nrow(data.test)
  pseudo_all <- matrix(NA,numberrow,2)
  ID_pseudo <- 1:numberrow
  pseudo_all <- cbind(pseudo_all,ID_pseudo)
  train.id <- data.train$Stu_id
  test.id <- data.test$Stu_id
  
  
  
  #train classification model to get the PS 
  data.train$trt <- as.numeric(data.train$trt)
  data.test$trt <- as.numeric(data.test$trt)
  
  train.ps <- SuperLearner(Y=data.train$trt,
                           X=data.train[,covariates],
                           SL.library = learner,
                           id=data.train$School_id,
                           verbose = FALSE, method = "method.NNLS", family = binomial())
  train.ps.hat <- train.ps$SL.predict
  train.ps.hat <- ifelse(train.ps.hat<0.025, 0.025, ifelse(train.ps.hat>.975,.975,train.ps.hat)) #set up the bound
  test.ps.hat <- predict.SuperLearner(train.ps, newdata = data.test[,covariates],id=data.test$schoolid)$pred
  
  pseudo_all[,2][train.id] <- train.ps.hat
  pseudo_all[,2][test.id] <- test.ps.hat
  
  
  #split training data into treatment and control observations 
  data.train <- as.data.frame(data.train)
  data.train_0 <- data.train[which(data.train$trt==0),] 
  data.train_1 <- data.train[which(data.train$trt==1),]
  
  #train regression model for the treatment observations 
  m1_mod <- SuperLearner(Y=data.train_1$yij,
                         X=data.train_1[,covariates],
                         SL.library = learner,
                         id=data.train_1$School_id,
                         verbose = FALSE, method = "method.NNLS")
  m1_hat.train_1 <- m1_mod$SL.predict
  m1_hat.test <-  predict.SuperLearner(m1_mod, newdata=data.test[,covariates],
                                                            id=data.test$School_id)$pred
  m1_hat.train_0 <- predict.SuperLearner(m1_mod, newdata=data.train_0[,covariates],
                                         id=data.train_0$School_id)$pred
  
  #train regression model for the control observations 
  m0_mod <- SuperLearner(Y=data.train_0$yij,
                         X=data.train_0[,covariates],
                         SL.library = learner,
                         id=data.train_0$School_id,
                         verbose = FALSE, method = "method.NNLS")
  m0_hat.train_0 <- m0_mod$SL.predict
  m0_hat.test <-  predict.SuperLearner(m0_mod, newdata=data.test[,covariates],
                                       id=data.test$School_id)$pred
  m0_hat.train_1 <- predict.SuperLearner(m0_mod, newdata=data.train_1[,covariates],
                                       id=data.train_1$School_id)$pred
  
  ##### X-learner 
  tau1.test <- data.test[which(data.test$trt==1),"yij"] - m0_hat.test[which(data.test$trt==1),]
  tau0.test <- m1_hat.test[which(data.test$trt==0),]-data.test[which(data.test$trt==0),"yij"]
  
  tau1.train <- data.train[which(data.train$trt==1),"yij"]- m0_hat.train_1
  tau0.train <- m0_hat.train_0 - data.train[which(data.train$trt==0),"yij"]
  
  ## collect all hats 
  pseudo_all[,1][(test.id[data.test$trt==1])] <- tau1.test
  pseudo_all[,1][(test.id[data.test$trt==0])] <- tau0.test
  
  pseudo_all[,1][(train.id[data.train$trt==1])] <- tau1.train
  pseudo_all[,1][(train.id[data.train$trt==0])] <- tau0.train
  
  # X-Learner final estimate 
  
  tau1_mod <- SuperLearner(Y=pseudo_all[,1][datatest$trt==1],
                           X=datatest[which(datatest$trt==1), covariates],
                           newX = datatest[,covariates],
                           id=datatest$School_id[datatest$trt==1],
                           verbose = FALSE, 
                           SL.library = learner, method = "method.NNLS")
  score_tau1 <- tau1_mod$SL.predict
  
  tau0_mod <- SuperLearner(Y=pseudo_all[,1][datatest$trt==0],
                           X=datatest[which(datatest$trt==0), covariates],
                           newX = datatest[,covariates],
                           id=datatest$School_id[datatest$trt==0],
                           verbose = FALSE, 
                           SL.library = learner, method = "method.NNLS")
  score_tau0 <- tau0_mod$SL.predict
  
  score_X <- pseudo_all[,2]*score_tau0+(1-pseudo_all[,2])*score_tau1 #The cate for both groups 
  

  
  
  SL.train.ps <- cbind(train.id,pseudo_all[,2][train.id])
  SL.train.CATE <- cbind(train.id, score_X[train.id])
  
  SL.test.ps <- cbind(test.id,pseudo_all[,2][test.id])
  SL.test.CATE <- cbind(test.id,pseudo_all[,2][test.id])
  
  SL.results <- list(SL.train.CATE,SL.test.CATE,SL.train.ps,SL.test.ps)
  
  return(SL.results)
  
}

#nrow(datatest2)

#simulation.start = proc.time()
#Xlearnertest1 <- XLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
#Xlearnertest2 <- XLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
#Xlearnertest3 <- XLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))

#simulation.edu=proc.time()
#total.time = simulation.edu-simulation.start 
#total.time
#user  system elapsed 
#242.605   2.915 232.782 

#calculate the total time of how to calculate the simulations 
  
#length(Slearnertest1)
  
  
