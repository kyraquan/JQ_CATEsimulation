#data.train$School_id
#learner="SL.bartMachine"
#class(data.train)
#levels(data.train$School_id)
#levels(data.test$School_id)
#data.train$trt <- numeric(data.train$trt)
#class(data.train$trt)
#class(data.train.0$trt)

#SLearner2test <- SLearner_est(data.test,data.train,datatest,covariates,learner)


#data.train.0$trt <- as.integer(data.train.0$trt)
#data.train.1$trt <- as.integer(data.train.1$trt)

#learner = c("SL.bartMachine", "SL.cforest") 
#length(learner)
#m_mod$libraryNames

SLearner_est <- function(data.test,data.train,datatest,covariates,learner){
require(SuperLearner)

#Train a regression model using the covariates and the treatment variable 
m_mod <- SuperLearner(Y=data.train$yij,
                      X=data.train[,c(covariates,"trt")],
                      SL.library = learner,
                      id=data.train$School_id,
                      verbose = FALSE, method = "method.NNLS")


data.train.0 <- data.train[,c(covariates,"trt")]
data.train.0$trt <- 0
data.train.1 <- data.train[,c(covariates,"trt")]
data.train.1$trt <- 1
data.test.0 <- data.test[,c(covariates,"trt")]
data.test.0$trt <- 0
data.test.1 <- data.test[,c(covariates,"trt")]
data.test.1$trt <- 1


trt.train.class <- class(data.train$trt) 
trt.train.0.class <- class(data.train.0$trt)

if(trt.train.0.class!=trt.train.class){
  data.train.0$trt<- as(data.train.0$trt,trt.train.class)
  data.train.1$trt<- as(data.train.1$trt,trt.train.class)
  data.test.0$trt<- as(data.test.0$trt,trt.train.class)
  data.test.1$trt<- as(data.test.1$trt,trt.train.class)
  }


s.forest.train.cate <- predict(m_mod,data.train.1)$pred - predict(m_mod,data.train.0)$pred


#set treatment variable to 0 or 1 
score_S <- matrix(0,nrow(datatest),1)

s.forest.test.cate <- predict(m_mod,data.test.1)$pred - predict(m_mod,data.test.0)$pred

score_S[,1][data.train$Stu_id] <- s.forest.train.cate
score_S[,1][data.test$Stu_id] <- s.forest.test.cate

s.forest.train.cate <- cbind(data.train$Stu_id,s.forest.train.cate)
s.forest.test.cate <- cbind(data.test$Stu_id,s.forest.test.cate)


s.forest.cate.list <- list(s.forest.train.cate,s.forest.test.cate,score_S) 

return(s.forest.cate.list)
}

#learners <- c("SL.bartMachine", "SL.cforest") 

#simulation.start = proc.time()
#Slearnertest1 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
#Slearnertest2 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
#Slearnertest3 <- SLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))
#length(Slearnertest1)
#simulation.edu=proc.time()
#total.time = simulation.edu-simulation.start 
#user  system elapsed 
# 69.177   0.722  67.281
#this is the S-learner 

#Slearnertest3[[1]][1:5,]


#datatest <- PS_model_data(n_cluster=30, n_ind=10,tau_var = 0.47, ICC=0.3,
                          ps_model=2,treatment_model = 1,outcome_model = 3)
#testsplits <- test.train.split(datatest)
#data.train <- testsplits[[1]]
#data.test <- testsplits[[2]]

#sltest <- SLearner_est(data.test=data.test, data.train = data.train, datatest, covariates, learner = c("SL.cforest","SL.bartMachine"))
#Results_evaluation(sltest,datatest)

