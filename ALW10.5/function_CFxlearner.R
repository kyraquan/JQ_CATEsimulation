#CFXtest <- CF_Xlearner(data.train, data.test,datatest)

#CFXtest[[1]][1:5,]
#CFXtest[[3]][1:5,]
#CFXtest[[2]][1:5,]
#CFXtest[[4]][1:5,]


CF_Xlearner <- function(data.train,data.test,datatest) { 
#  require(grf)
  
  numberrow <- nrow(data.train)+nrow(data.test)
  pseudo_all <- matrix(NA,numberrow,2)
  ID_pseudo <- 1:numberrow
  pseudo_all <- cbind(pseudo_all,ID_pseudo)
  covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")
  
  
  train.id <- data.train$Stu_id
  test.id <- data.test$Stu_id

#obtain PS first 
train.ps.forest <- regression_forest(X=as.matrix(data.train[,covariates]),
                                        Y=data.train$trt,
                                        clusters = data.train$School_id)

train.ps.hat <- predict(train.ps.forest)$predictions #save propensity score for train
test.ps.hat <- predict(train.ps.forest,newdata = data.test[,covariates],clusters=data.test$School_id)$predictions #300 records


pseudo_all[,2][train.id] <- train.ps.hat #saving the ps scores 
pseudo_all[,2][test.id] <- test.ps.hat #saving the ps scores 


#split training data into treatment and control observations 
data.train <- as.data.frame(data.train)
data.train_0 <- data.train[which(data.train$trt==0),] 
data.train_1 <- data.train[which(data.train$trt==1),]


#train regression model for the treatment observations 
m1_mod <- regression_forest(Y=data.train_1$yij,
                            X=data.train_1[,covariates],
                            clusters=data.train_1$School_id)
m1_hat.train_1 <- m1_mod$predictions
m1_hat.train_0 <- predict(m1_mod, newdata = data.train_0[,covariates], clusters=data.train_0$School_id)$predictions
m1_hat.test <- predict(m1_mod, newdata =data.test[,covariates], clusters=data.test$School_id)$predictions


#train regression model for the control observations 
m0_mod <- regression_forest(Y=data.train_0$yij,
                            X=data.train_0[,covariates],
                            clusters=data.train_0$School_id)
m0_hat.train_0 <- m0_mod$predictions
m0_hat.train_1 <- predict(m0_mod, newdata = data.train_1[,covariates], clusters=data.train_1$School_id)$predictions
m0_hat.test <- predict(m0_mod, newdata =data.test[,covariates], clusters=data.test$School_id)$predictions


##### X-learner 
tau1.test <- data.test[which(data.test$trt==1),"yij"] - m0_hat.test[which(data.test$trt==1)]
tau0.test <- m1_hat.test[which(data.test$trt==0)]-data.test[which(data.test$trt==0),"yij"]

tau1.train <- data.train[which(data.train$trt==1),"yij"]- m0_hat.train_1
tau0.train <- m0_hat.train_0 - data.train[which(data.train$trt==0),"yij"]

## collect all hats 

pseudo_all[,1][(test.id[data.test$trt==1])] <- tau1.test
pseudo_all[,1][(test.id[data.test$trt==0])] <- tau0.test

pseudo_all[,1][(train.id[data.train$trt==1])] <- tau1.train
pseudo_all[,1][(train.id[data.train$trt==0])] <- tau0.train


# X-Learner final estimate 
tau1_mod <- regression_forest(Y=pseudo_all[,1][datatest$trt==1],
                              X=datatest[which(datatest$trt==1), covariates],
                              # newX=datatest[,covariates],
                              clusters = datatest$School_id[datatest$trt==1])

#score_tau1 <- tau1_mod$predictions #PS score for all treated 
score_tau1 <- predict(tau1_mod,newdata = datatest[,covariates],clusters=datatest$School_id)$predictions


tau0_mod <- regression_forest(Y=pseudo_all[,1][datatest$trt==0],
                         X=datatest[which(datatest$trt==0), covariates],
                         #newX = datatest[,covariates],
                         cluster=datatest$School_id[datatest$trt==0])
                         
#score_tau0 <- tau0_mod$predictions
score_tau0 <- predict(tau0_mod,newdata = datatest[,covariates],clusters=datatest$School_id)$predictions



score_X <- pseudo_all[,2]*score_tau0+(1-pseudo_all[,2])*score_tau1 #The cate for both groups 



SL.train.ps <- cbind(train.id,pseudo_all[,2][train.id])
SL.train.CATE <- cbind(train.id, score_X[train.id])

SL.test.ps <- cbind(test.id,pseudo_all[,2][test.id])
SL.test.CATE <- cbind(test.id,score_X[test.id])


SL.results <- list(SL.train.CATE,SL.test.CATE,SL.train.ps,SL.test.ps)

return(SL.results) }

# CFX1 <- CF_Xlearner(data.train,data.test,datatest)


