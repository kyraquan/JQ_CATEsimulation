

#train classification model to get the PS 
#data.train$trt <- as.numeric(data.train$trt)
#data.test$trt <- as.numeric(data.test$trt)

#datatest <- PS_model_data(n_cluster = 60, n_ind=20, tau_var = 0.1, ICC=0.3,
#                          ps_model = 2, treatment_model = 2, outcome_model = 2)

#datasplit <- test.train.split(datatest)
#data.train <- datasplit[[1]]
#data.test <- datasplit[[2]]

#BARTXtest <- BART_Xlearner(data.train, data.test,datatest)
#BARTXtest[[1]][1:5,]
#BARTXtest[[2]][1:5,]
#BARTXtest[[3]][1:5,]
#BARTXtest[[4]][1:5,]


BART_Xlearner <- function(data.train,data.test,datatest) { 
  #library(bartCause)
  
  numberrow <- nrow(data.train)+nrow(data.test)
  pseudo_all <- matrix(NA,numberrow,2)
  ID_pseudo <- 1:numberrow
  pseudo_all <- cbind(pseudo_all,ID_pseudo)
  
  train.id <- data.train$Stu_id
  test.id <- data.test$Stu_id
  
  #obtain PS first 
  train.ps.bart <- bartc(response = yij,
                         treatment = trt,
                         confounders = x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id,
                         data = data.train,
                         keepTrees=TRUE,
                         method.trt = "bart",
                         method.rsp = "bart",
                         estimand = "ate"
                         )
  train.ps.hat <- train.ps.bart$p.score
  test.ps.hat <- predict(train.ps.bart, newdata = data.test,type = "p.score") #keep train ps
  test.ps.hat <- apply(test.ps.hat, 2, mean) #keep test ps 
  
  pseudo_all[,2][train.id] <- train.ps.hat 
  pseudo_all[,2][test.id] <- test.ps.hat
  

  
  ##### X-learner 

  
  
  #observed responses under the treatment together with predicted under the treated/control
  m1_hat.test <- fitted(train.ps.bart, newdata=data.test ,type = "y.1") #600 records
  m0_hat.test <- fitted(train.ps.bart, newdata=data.test ,type = "y.0") #600 records 
  
  m1_hat.train <- fitted(train.ps.bart, type = "y.1") #600 records
  m0_hat.train <- fitted(train.ps.bart, type = "y.0") #600 records 
  
  tau1.test <- data.test[which(data.test$trt==1),"yij"]-m0_hat.test[which(data.test$trt==1)]
  tau0.test <- m1_hat.test[which(data.test$trt==0)]-data.test[which(data.test$trt==0),"yij"]
  
  tau1.train <- data.train[which(data.train$trt==1),"yij"]- m0_hat.train[which(data.train$trt==1)]
  tau0.train <- m1_hat.train[which(data.train$trt==0)]-data.train[which(data.train$trt==0),"yij"]
  
  
  
  ## collect all hats 
  pseudo_all[,1][(test.id[data.test$trt==1])] <- tau1.test
  pseudo_all[,1][(test.id[data.test$trt==0])] <- tau0.test
  
  pseudo_all[,1][(train.id[data.train$trt==1])] <- tau1.train
  pseudo_all[,1][(train.id[data.train$trt==0])] <- tau0.train
  
  # X-Learner final estimate 
  
  datatest$pseudo_all <- pseudo_all[,1]
  
  tau_mod <- bartc(response = pseudo_all, treatment = trt,
                   confounders = x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id,
                   data = datatest,
                   keepTrees=TRUE,
                   method.trt = "bart",
                   method.rsp = "bart",
                   estimand = "ate") 
  score_tau1 <- fitted(tau_mod,type = "y.1")
  score_tau0 <- fitted(tau_mod,type = "y.0")
  
  score_X <- pseudo_all[,2]*score_tau0+(1-pseudo_all[,2])*score_tau1 #The cate for both groups 
  
  SL.train.ps <- cbind(train.id,pseudo_all[,2][train.id])
  SL.train.CATE <- cbind(train.id, score_X[train.id])
  
  SL.test.ps <- cbind(test.id,pseudo_all[,2][test.id])
  SL.test.CATE <- cbind(test.id,score_X[test.id])
  
  SL.results <- list(SL.train.CATE,SL.test.CATE,SL.train.ps,SL.test.ps)
  
  return(SL.results) }
