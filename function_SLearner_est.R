

SLearner_est <- function(data.test,data.train,datatest,covariates,learner){
require(SuperLearner)

#first list, CATE for train; second list, CATE for test 
  
#Train a regression model using the covariates and the treatment variable 
m_mod <- SuperLearner(Y=data.train$yij,
                      X=data.train[,c(covariates,"trt")],
                      SL.library = learner,
                      id=data.train$School_id,
                      verbose = FALSE, method = "method.NNLS")

#estimate CATE of train
data.train.0 <- data.train[,c(covariates,"trt")]
data.train.0$trt <- 0
data.train.1 <- data.train[,c(covariates,"trt")]
data.train.1$trt <- 1
s.forest.train.cate <- predict(m_mod,data.train.1)$pred - predict(m_mod,data.train.0)$pred


#set treatment variable to 0 or 1 
data.test.0 <- data.test[,c(covariates,"trt")]
data.test.0$trt <- 0
data.test.1 <- data.test[,c(covariates,"trt")]
data.test.1$trt <- 1

score_S <- matrix(0,nrow(datatest),1)

s.forest.test.cate <- predict(m_mod,data.test.1)$pred - predict(m_mod,data.test.0)$pred

score_S[,1][data.train$Stu_id] <- s.forest.train.cate
score_S[,1][data.test$Stu_id] <- s.forest.test.cate

s.forest.train.cate <- cbind(data.train$Stu_id,s.forest.train.cate)
s.forest.test.cate <- cbind(data.test$Stu_id,s.forest.test.cate)

s.forest.cate.list <- list(s.forest.train.cate,s.forest.test.cate,score_S) 

return(s.forest.cate.list)
}

learners <- c("SL.bartMachine", "SL.cforest") 

for (i in 1:3){
  print(s.forest.cate.list[[i]][1:5,])
}


simulation.start = proc.time()
Slearnertest1 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
Slearnertest2 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
Slearnertest3 <- SLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))

simulation.edu=proc.time()
total.time = simulation.edu-simulation.start 
#user  system elapsed 
# 69.177   0.722  67.281
#this is the S-learner 


