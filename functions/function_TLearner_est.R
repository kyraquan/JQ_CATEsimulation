TLearner_est <- function(data.test,data.train,datatest,covariates,learner){
  require(SuperLearner)
  
  data.train.0 <- data.train[which(data.train$trt==0),]
  data.train.1 <- data.train[which(data.train$trt==1),]
  
  #learner = "SL.cforest"
  #Train a regression model using the covariates and the treatment variable 
  m1_mod <- SuperLearner(Y=data.train.1$yij,
                        X=data.train.1[,covariates],
                        SL.library = learner,
                        id=data.train.1$School_id,
                        verbose = FALSE, method = "method.NNLS")
  train.m1 <- m1_mod$SL.predict
  test.m1 <- predict(m1_mod,newdata=data.test[,covariates],id=data.test$schoolid)$pred
  
  m0_mod <- SuperLearner(Y=data.train.0$yij,
                         X=data.train.0[,covariates],
                         SL.library = learner,
                         id=data.train.0$School_id,
                         verbose = FALSE, method = "method.NNLS")
  train.m0 <- m0_mod$SL.predict
  test.m0 <- predict(m0_mod,newdata=data.test[,covariates],id=data.test$schoolid)$pred
  
  #estimate CATE of test
  score_T <- matrix(0,nrow(datatest),1)
  tlearner.test.cate <- test.m1 - test.m0
  score_T[,1][data.test$Stu_id] <- tlearner.test.cate
  
  #estimate CATE of train 
  train.m1 <- predict(m1_mod,newdata=data.train[,covariates],id=data.train$schoolid)$pred
  train.m0 <- predict(m0_mod,newdata=data.train[,covariates],id=data.train$schoolid)$pred
  tlearner.train.cate <- train.m1 - train.m0
  score_T[,1][data.train$Stu_id] <- tlearner.train.cate
  
  
  tlearner.train.cate <- cbind(data.train$Stu_id,tlearner.train.cate)
  tlearner.test.cate <- cbind(data.test$Stu_id,tlearner.test.cate)
  
  tlearner.cate.list <- list(tlearner.train.cate,tlearner.test.cate,score_T) 
  
  return(tlearner.cate.list)
}


#learners <- c("SL.bartMachine", "SL.cforest") 

#simulation.start = proc.time()
#Tlearnertest1 <- TLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
#Tlearnertest2 <- TLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
#Tlearnertest3 <- TLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))

#simulation.edu=proc.time()
#total.time = simulation.edu-simulation.start 
#user  system elapsed 
#70.056   0.860  66.197 