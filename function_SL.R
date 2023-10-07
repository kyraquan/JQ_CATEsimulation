datatest2 <- PS_model_data(n_cluster=60,n_ind=10,ICC=0.3,ps_model = 1,treatment_model = 2,outcome_model = 2)
datatest2$trt
#split the dataset into training and test data

train.sample = sample(1:nrow(datatest2), nrow(datatest2)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(datatest2)
test.sample = test.sample[-c(train.sample)]


data.train = datatest2[train.sample,]
data.test = datatest2[test.sample,]
data.train = as.data.frame(data.train)
data.test = as.data.frame(data.test)


SL.est <- function(data.train, data.test){
 require(SuperLearner) 
  learners <- c("SL.bartMachine", "SL.cforest")
  #listWrappers()
  #x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id
  
  #train classification model to get the PS 
  data.train$trt <- as.numeric(data.train$trt)
  data.test$trt <- as.numeric(data.test$trt)
  train.ps <- SuperLearner(Y=data.train$trt,
                           X=data.train[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                           SL.library = learners,
                           id=data.train$School_id,
                           verbose = FALSE, method = "method.NNLS", family = binomial())
  SL.train.ps <- train.ps$SL.predict
  SL.train.ps <- ifelse(SL.train.ps<0.025, 0.025, ifelse(SL.train.ps>.975,.975,SL.train.ps))

  
  SL.test.ps <- predict(train.ps,newdata = data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                        id=data.test$schoolid)$pred 
  SL.test.ps <- ifelse(SL.test.ps<0.025, 0.025, ifelse(SL.test.ps>.975,.975,SL.test.ps))

  data.train$ps <- SL.train.ps
  
  
  #obtaining the ATE? 
  data.train <- as.data.frame(data.train)
  data.train_0 <- data.train[which(data.train$trt==0),] 
  data.train_1 <- data.train[which(data.train$trt==1),]
  #data.test_0 <- data.test[which(data.test$trt==0),] 
  #data.test_1 <- data.test[which(data.test$trt==1),]
  
  train.cate.0 <- SuperLearner(Y=data.train_0$yij,
                             X=data.train_0[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                             #newX = data.test[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")],
                             SL.library = learners,
                             id=data.train_0$School_id,
                             verbose = FALSE, method = "method.NNLS",
                             obsWeights = as.vector(data.train_0$SL.predict)) #the obs weight cannot be used 
  train.cate.0.hat <- train.cate.0$SL.predict
  test.cate.0.hat <- predict.SuperLearner(train.cate.0,newdata = data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                                          id=data.test$School_id)$pred
  train.cate.0.hat <- predict.SuperLearner(train.cate.0,newdata = data.train_1[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                                          id=data.train$School_id)$pred
  
  train.cate.1 <- SuperLearner(Y=data.train_1$yij,
                               X=data.train_1[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                               #newX = data.test[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")],
                               SL.library = learners,
                               id=data.train_1$School_id,
                               obsWeights = as.vector(data.train_1$SL.predict),
                               verbose = FALSE, method = "method.NNLS")
  train.cate.1.hat <- train.cate.1$SL.predict
  train.cate.1.hat <- predict.SuperLearner(train.cate.1, newdata=data.train_0[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                                           id=data.train_0$School_id)$pred
  test.cate.1.hat <- predict.SuperLearner(train.cate.1, newdata=data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                                          id=data.test$School_id)$pred
  #CATE 
  data.train$tau.pred <- NA
  train.cate.pred_1 <- data.train[which(data.train$trt==1),"yij"] - train.cate.0.hat #tau1 
  train.cate.pred_0 <- train.cate.1.hat - data.train_0$yij #tau0 
  data.train[which(data.train$trt==1),"tau.pred"] <- train.cate.pred_1
  data.train[which(data.train$trt==0),"tau.pred"] <- train.cate.pred_0
  train.cate <- data.train$tau.pred*
  
  
  
  test.cate.pred_1 <- data.test[which(data.test$trt==1),"yij"] - test.cate.0.hat[which(data.test$trt==0),]
  test.cate.pred_0 <- test.cate.1.hat[which(data.test$trt==0),] - data.test[which(data.test$trt==1),"yij"]
  data.test[which(data.test$trt==1),"tau.pred"] <- test.cate.pred_1
  data.test[which(data.test$trt==0),"tau.pred"] <- test.cate.pred_0
  
  
  
  length(test.cate.pred_0)+length(test.cate.pred_1)
  length(data.test[which(data.test$trt==1),"yij"])
  test.cate <- SL.test.ps*test.cate.pred_0 + (1-SL.test.ps)*test.cate.pred_1
  
  
  
  
  
  
  
  train.cate.pred <- train.cate.1.hat - train.cate.0.hat 
  test.cate.pred <- test.cate.1.hat - test.cate.0.hat 
  ate.test.pred <- mean(test.cate.pred) #potential outcomes
  
  SL.preds <- list(SL.train.ps,SL.test.ps, train.cate, test.cate)
  return(SL.preds)
  
  #estimating ATE? 
  # citation for ATE as the average of CATE - expected value between treatment and control 
  #Dr.Leite book page 100 
  #'look for in articles of TMLE van der Laan, M. J. (2010). Targeted maximum likelihood based causal inference: Part i. The International
  #' Journal of Biostatistics, 6(2)
  
}

SLtest <- SL.est(data.test = data.test, data.train = data.train)

data$ID <- c(1:nrow(data))

pseudo_all <- matrix(NA,nrow = (nrow(data.test)+nrow(data.train)),2)
ID_pseudo <- 1:(nrow(data.test)+nrow(data.train))
pseudo_all <- cbind(pseudo_all,ID_pseudo)
p_mod <- SuperLearner(Y=data.train$trt,
                      X=data.train[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                      newX = data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                      SL.library = learners,
                      id=data.train$School_id,
                      verbose = FALSE, method = "method.NNLS", family = binomial())
p_hat <- p_mod$SL.predict 
p_hat <- ifelse(p_hat<0.025, 0.025, ifelse(p_hat>.975,.975,p_hat)) #overlap bounding 
##collect PS 
pseudo_all[,2][data.test$Stu_id] <- p_hat

m1_mod <- SuperLearner(Y=data.train_1$yij,
                       X=data.train_1[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                       newX = data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                       SL.library = learners,
                       id=data.train_1$School_id,
                       #obsWeights = as.vector(data.train_1$SL.predict),
                       verbose = FALSE, method = "method.NNLS")
m0_mod <- SuperLearner(Y=data.train_0$yij,
                       X=data.train_0[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                       newX = data.test[,c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")],
                       SL.library = learners,
                       id=data.train_0$School_id,
                       #obsWeights = as.vector(data.train_1$SL.predict),
                       verbose = FALSE, method = "method.NNLS")
m1_hat <-m1_mod$SL.predict
m0_hat <-m0_mod$SL.predict

tau1 <- data.test[which(data.test$trt==1),"yij"] - test.cate.0.hat[which(data.test$trt==0),]
tau0 <- test.cate.1.hat[which(data.test$trt==0),] - data.test[which(data.test$trt==1),"yij"]

pseudo_all[,1][ (data.test$Stu_id[data.test$trt==1])] <- tau1
pseudo_all[,1][ (data.test$Stu_id[data.test$trt==0])] <- tau0

covariate <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")
tau1_mod <- SuperLearner(Y=pseudo_all[,1][datatest2$trt==0],
                         X=datatest2[which(datatest2$trt==0),covariate],
                         newX=datatest2[,covariate],
                         SL.library = learners,
                         id=data.train_0$School_id,
                         #obsWeights = as.vector(data.train_1$SL.predict),
                         verbose = FALSE, method = "method.NNLS")
                         





  SL.test.cate  <- train.cate.1.hat-train.cate.0.hat
  
  train.cate.0 <- SuperLearner(Y=data.train_0$yij,
                               X=data.train_0[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")],
                               #newX = data.test[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")],
                               SL.library = learners,
                               id=data.train_0$schoolid,
                               verbose = FALSE, method = "method.NNLS")
  predstest <- predict.SuperLearner(train.cate.0,newdata = data.test[,c("x1ij","x2ij","V1rep","V2rep","V3rep","V8","V9","V10","V11","V12")],
                       id=data.test$schoolid)
  
}
nrow(data.test)
summary(train.ps)

#### testing below 
install.packages("SuperLearner")
library(SuperLearner)
?SuperLearner
# jacob selected xgboost, nnet, and random forest 
# Wong2019 used lasso, decision tree, random forest, neural network, and svm
# use bart, random forest
listWrappers()
methodnames <- c("SL.cforest", "SL.bartMachine")
methodnames2 <- c("SL.cforest", "SL.bartMachine","SL.xgboost","SL.nnet")



#train a classification model to get the propensity score 
p_mod <- SuperLearner(Y = as.vector(dataset1_full_trt2[,2]),
                      X = as.data.frame(dataset1_full_trt2[,c(2:20)]),
                      SL.library = methodnames,
                      method = "method.NNLS",
                      id = as.vector(dataset1_full_trt2[,1]),
                      family = binomial())
#propensity score 
p_hat <- p_mod$SL.predict
var(p_hat)
#train a regression model for the treatment observation 
#split the training data into trt and cntr 
dataset1_full_trt2 <- as.data.frame(dataset1_full_trt2)
dataset1_full_trt2$ps_p <- p_hat
datatrain_1 <- dataset1_full_trt2[which(dataset1_full_trt2$trt2==1),]
datatrain_0 <- dataset1_full_trt2[which(dataset1_full_trt2$trt2==0),]
install.packages("xgboost")
m1_mod <- SuperLearner(Y = datatrain_1$yij1_trt2,
                       X = datatrain_1[,c(2:20)],
                       SL.library = methodnames,
                       method = "method.NNLS",
                       #obsWeights = dataset1_full_trt2$ps_p,
                       id = datatrain_1$schoolid)
                       
m1_hat <- m1_mod$SL.predict 
m0_mod <- SuperLearner(Y = as.vector(datatrain_0[,21]),
                       X = as.data.frame(datatrain_0[,c(3:20)]),
                       SL.library = methodnames,
                       method = "method.NNLS",
                       id = as.vector(datatrain_0[,1]),
                       family = binomial())
m0_hat <- m0_mod$SL.predict

install.packages("WeightIt")
library(WeightIt)
?weightit
w1 <- weightit(trt2 ~ x1ij + x2ij + V1rep + V2rep + V3rep + V8 + V9 + V10 + V11+V12+V13,
               data = dataset1_full_trt2, method = "super",estimand = "ATE",
               SL.library = c("SL.glm", "SL.stepAIC",
                              "SL.glm.interaction"))
summary(w1)


library("cobalt")
data("lalonde", package = "cobalt")
#Balancing covariates between treatment groups (binary)
(W1 <- weightit(treat ~ age + educ + married +
                  nodegree + re74, data = lalonde,
                method = "super", estimand = "ATT",
                SL.library = c("SL.glm", "SL.stepAIC",
                               "SL.glm.interaction")))




### example from Jacob 
##Train a classification model to get the propensity scores
p_mod <- SuperLearner(Y = df_aux$d, X = df_aux[,covariates], newX = df_main[,covariates], SL.library = learners,
                      verbose = FALSE, method = "method.NNLS", family = binomial(),cvControl = control)

p_hat <- p_mod$SL.predict
p_hat = ifelse(p_hat<0.025, 0.025, ifelse(p_hat>.975,.975, p_hat)) # Overlap bounding


## Collect propensity-score

pseudo_all[,2][df_main$ID] <- p_hat

# Split the training data into treatment and control observations
aux_1 <- df_aux[which(df_aux$d==1),]
aux_0 <- df_aux[which(df_aux$d==0),]

# Train a regression model for the treatment observations
m1_mod <- SuperLearner(Y = aux_1$y, X = aux_1[,covariates], newX = df_main[,covariates], SL.library = learners,
                       verbose = FALSE, method = "method.NNLS",cvControl = control)

m1_hat <- m1_mod$SL.predict

# Train a regression model for the control observations
m0_mod <- SuperLearner(Y = aux_0$y, X = aux_0[,covariates], newX = df_main[,covariates], SL.library = learners,
                       verbose = FALSE, method = "method.NNLS",cvControl = control)

m0_hat <- m0_mod$SL.predict






install.packages("party")
dataset1_full_trt2

