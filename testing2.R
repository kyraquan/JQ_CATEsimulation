#propensity score model 
Ni <- 30 #30,60 #individuals in a group
Nj <- 60 #200,500 #number of groups 
n <- Ni*Nj 
schoolid <- gl(Ni,k=Nj)
set.seed(89375)
#trt 2 of prob2 

yij1_trt2<- 1+tau1*trt2+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
### dataset 
dataset1_trt2 <- cbind(schoolid,trt2,x1ij,x2ij,V1rep,V2rep,V3rep)
dataset1_full_trt2 <- cbind(dataset1_trt2,distractor_distribution)
dataset1_full_trt2 <- cbind(dataset1_full_trt2,yij1_trt2)



#split the dataset into training and test data
train.sample = sample(1:nrow(dataset1_full_trt2), nrow(dataset1_full_trt2)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(dataset1_full_trt2)
test.sample = test.sample[-c(train.sample)]

#run training causal forest
data.train = dataset1_full_trt2[train.sample,]
data.test = dataset1_full_trt2[test.sample,]

data.train[,'yij1']

?causal_forest

#run training forest
train.forest = causal_forest(X=data.frame(data.train[,c(3:20)]),
                             Y = as.numeric(data.train[,'yij1_trt2']),
                             W = as.numeric(as.character(data.train[,'trt2'])),
                             W.hat = mean(as.numeric(as.character(data.train[,'trt2']))),
                             clusters = data.train[,'schoolid'], min.node.size = 50)

#Obtain estimates of the conditional average treatment effect (CATE)
tau.hat = predict(train.forest,X= data.frame(data.test[,c(3:20)]))$predictions
mean((tau.hat-dataset1_full_trt2[test.sample,'yij1_trt2'])^2)


bart = bartc(response=as.numeric(data.train[,'yij1_trt2']), treatment=as.numeric(as.character(data.train[,'trt2'])), 
             confounders=data.frame(data.train[,c(1,3:20)]), keepTrees = TRUE,
             method.rsp="bart", method.trt="none", estimand="ate")
conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(1,3:20)]), type="icate")
conditional.ind.effects = apply(conditional.ind.effects, 2, mean) #take mean of posterior predictions
mean((conditional.ind.effects-dataset1_full_trt2[test.sample,'yij1_trt2'])^2)



## another condition 
yij1_trt3<- 1+tau1*trt3+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
### dataset 
dataset1_trt3 <- cbind(schoolid,trt3,x1ij,x2ij,V1rep,V2rep,V3rep)
dataset1_full_trt3 <- cbind(dataset1_trt3,distractor_distribution)
dataset1_full_trt3 <- cbind(dataset1_full_trt3,yij1_trt3)

#split the dataset into training and test data
train.sample = sample(1:nrow(dataset1_full_trt3), nrow(dataset1_full_trt3)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(dataset1_full_trt3)
test.sample = test.sample[-c(train.sample)]

#run training causal forest
data.train = dataset1_full_trt3[train.sample,]
data.test = dataset1_full_trt3[test.sample,]



#run training forest
train.forest = causal_forest(X=data.frame(data.train[,c(3:20)]),
                             Y = as.numeric(data.train[,'yij1_trt3']),
                             W = as.numeric(as.character(data.train[,'trt3'])),
                             W.hat = mean(as.numeric(as.character(data.train[,'trt3']))),
                             clusters = data.train[,'schoolid'], min.node.size = 50)

#Obtain estimates of the conditional average treatment effect (CATE)
tau.hat = predict(train.forest,X= data.frame(data.test[,c(3:20)]))$predictions
mean((tau.hat-dataset1_full_trt3[test.sample,'yij1_trt3'])^2)

#run bart including teacher id to account for clustering effects
bart = bartc(response=as.numeric(data.train[,'yij1_trt3']), treatment=as.numeric(as.character(data.train[,'trt3'])), 
             confounders=data.frame(data.train[,c(1,3:20)]), keepTrees = TRUE,
             method.rsp="bart", method.trt="none", estimand="ate")
conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(1,3:20)]), type="icate")
conditional.ind.effects = apply(conditional.ind.effects, 2, mean) #take mean of posterior predictions
mean((conditional.ind.effects-dataset1_full_trt3[test.sample,'yij1_trt3'])^2)


###### tau 2 , yij1, rho=0.1, 
yij1 <- 1+tau2*trt1+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
var(yij1)
yij2 <- 1+tau2*trt1+0.4*x1ij-0.3*x2ij+x1ij*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
var(yij2)
yij3 <- 1+tau2*trt1+0.4*x1ij-0.3*x2ij+x1ij*V1rep+0.4*V1rep-0.4*V2rep+0.16*V2rep+ujrep+eij

### dataset 
dataset1 <- cbind(schoolid,trt3,x1ij,x2ij,V1rep,V2rep,V3rep)
dataset1_full <- cbind(dataset1,distractor_distribution,yij3)

#split the dataset into training and test data
train.sample = sample(1:nrow(dataset1_full), nrow(dataset1_full)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(dataset1_full)
test.sample = test.sample[-c(train.sample)]

#run training causal forest
data.train = dataset1_full[train.sample,]
data.test = dataset1_full[test.sample,]



#run training forest
train.forest = causal_forest(X=data.frame(data.train[,c(3:20)]),
                             Y = as.numeric(data.train[,'yij3']),
                             W = as.numeric(as.character(data.train[,'trt3'])),
                             W.hat = mean(as.numeric(as.character(data.train[,'trt3']))),
                             clusters = data.train[,'schoolid'], min.node.size = 50)

#Obtain estimates of the conditional average treatment effect (CATE)
tau.hat = predict(train.forest,X= data.frame(data.test[,c(3:20)]))$predictions
mean((tau.hat-dataset1_full[test.sample,'yij3'])^2)

#run bart including teacher id to account for clustering effects
bart = bartc(response=as.numeric(data.train[,'yij3']), treatment=as.numeric(as.character(data.train[,'trt3'])), 
             confounders=data.frame(data.train[,c(1,3:20)]), keepTrees = TRUE,
             method.rsp="bart", method.trt="none", estimand="ate")
conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(1,3:20)]), type="icate")
conditional.ind.effects = apply(conditional.ind.effects, 2, mean) #take mean of posterior predictions
mean((conditional.ind.effects-dataset1_full[test.sample,'yij3'])^2)


###### tau 2 , yij1, rho=0.1, 
yij11 <- 1+tau3*trt1+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
yij12 <- 1+tau3*trt2+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
yij13 <- 1+tau3*trt3+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij

var(yij1)
yij2 <- 1+tau3*trt1+0.4*x1ij-0.3*x2ij+x1ij*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
var(yij2)
yij3 <- 1+tau3*trt1+0.4*x1ij-0.3*x2ij+x1ij*V1rep+0.4*V1rep-0.4*V2rep+0.16*V2rep+ujrep+eij

### dataset 
dataset1 <- cbind(schoolid,trt1,x1ij,x2ij,V1rep,V2rep,V3rep)
dataset1_full <- cbind(dataset1,distractor_distribution,yij11)

#split the dataset into training and test data
train.sample = sample(1:nrow(dataset1_full), nrow(dataset1_full)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(dataset1_full)
test.sample = test.sample[-c(train.sample)]

#run training causal forest
data.train = dataset1_full[train.sample,]
data.test = dataset1_full[test.sample,]



#run training forest
train.forest = causal_forest(X=data.frame(data.train[,c(3:20)]),
                             Y = as.numeric(data.train[,'yij11']),
                             W = as.numeric(as.character(data.train[,'trt1'])),
                             W.hat = mean(as.numeric(as.character(data.train[,'trt1']))),
                             clusters = data.train[,'schoolid'], min.node.size = 50)

#Obtain estimates of the conditional average treatment effect (CATE)
tau.hat = predict(train.forest,X= data.frame(data.test[,c(3:20)]))$predictions
mean((tau.hat-dataset1_full[test.sample,'yij11'])^2)

#run bart including teacher id to account for clustering effects
bart = bartc(response=as.numeric(data.train[,'yij11']), treatment=as.numeric(as.character(data.train[,'trt1'])), 
             confounders=data.frame(data.train[,c(1,3:20)]), keepTrees = TRUE,
             method.rsp="bart", method.trt="none", estimand="ate")
conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(1,3:20)]), type="icate")
conditional.ind.effects = apply(conditional.ind.effects, 2, mean) #take mean of posterior predictions
mean((conditional.ind.effects-dataset1_full[test.sample,'yij11'])^2)



library(devtools)
install_github('susanathey/causalTree')
library(causalTree)
unloadNamespace('simstudy')
unloadNamespace('DescTools')


# Installs packages if not already installed, then loads packages 
list.of.packages <- c("glmnet", "rpart", "rpart.plot", "randomForest", "knitr", "dplyr", "purrr", "SuperLearner", "caret", "xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))
install.packages('ipred')




tree <- causalTree(y~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)

select <- dplyr::select
set.seed(1)
my_data <- readRDS('social_voting.rds')
#restrict the samle size 
n_obs <- 33000
my_data <- my_data[sample(nrow(my_data), n_obs),]
#split data into 3 samples 
folds = createFolds(1:nrow(my_data), k=3)


#' one or two weeks prior to 11/17 
#' October 31th ish to defense 
#' first submission for sepetmber 29th, just put all the sections for formatting 
#' degree application by september 15th ; ask brittany to submit this for me 
#' to one.uf.edu 
#' ask Brittany for help  
#' divide time; semulation to run at the beginning of september to have a month to write 
#' basically two month to write everything 
#' may take a while to run simulation 
#' submit to hyper-gator to schedule ask for people availability 
#' main conflicts of teaching/travel time 
#' sign-up for the graduation ceremony for the podium part 



#PS_model_data = function(
    n_cluster = 10000 #the number of cluster 
                         n_ind =1000 #the number of individual 
                         int=2.197225 #overall mean of ps model (intercept) to be treated; 0.8472979 - 30% treated
                         tau_var=0.1
                         ICC = 0.1 #for outcome model only,
                         ps_model # 1=constant, 2 = no interaction, 3=interactions
                         treatment_model,# 1=constant, 2 = no interaction, 3=interaction
                         outcome_model # 1=constant, 2 = no interaction, 3=interaction
                         

  # step 1: specifying the group random effect using ICC for outcome model only 
  require(MASS)
  
  require(simstudy)
  #targetICC <- c(0.1)
  var.lev1.res = 1
  setVars <- iccRE(ICC,dist = "normal",varWithin = var.lev1.res) #variance of level-1 residual in outcome model 
  var.lev2 <- setVars
  
  # end step 1
  #=======================================================================================# 
  
  #step 2: making level-1 and level-2 covariates 
  #simulate level-1 residual for the ps model and outcome 
  lev1.res = rnorm(n_cluster*n_ind,0,var.lev1.res) #residual for continuous outcome model
  #lev1.res.t = rlogis(n_cluster*n_ind) #ps model residual 
  
  #generating level-1 covariates 
  #n_ind=3
  #n_cluster=6
  n <- n_ind*n_cluster
  
  Stu_id = c(1:n)
  School_id=gl(n_cluster,k=n_ind)
  School_id = as.data.frame(gl(n_cluster,k=n_ind))
  names(School_id) <- c("School_id")
  
  x1ij <- rbinom(n,1,0.5) #level-1, binary
  x2ij <- rnorm(n,0,1) #level-1, continuous
  lev1.pred = cbind(Stu_id, School_id,x1ij,x2ij)
  
  lev1.pred = as.data.frame(cbind(x1ij,x2ij))
  lev1.pred=data.frame(Stu_id,lev1.pred)
  
  #generating level-2 covariates 
  v1j <- rbinom(n_cluster,1,0.3) #level-2, binary
  v2j <- runif(n_cluster,0,1) #level-2, uniform
  v3j <- rnorm(n_cluster,0,1) #level-2, continuous 
  v1rep <- rep(v1j,each=n_ind)
  v2rep <- rep(v2j,each=n_ind)
  v3rep <- rep(v3j,each=n_ind)
  
  lev2.pred = cbind(v1rep,v2rep,v3rep)
  
  lev2.pred=as.data.frame(cbind(School_id,v1rep,v2rep,v3rep))
  
  Stu.data = as.data.frame(cbind(lev1.pred, lev2.pred))
  
  
  #generating PS
  #proportion treated for int=2.197 (10% treated), if plan to manipulate it, reflecting the proportion treated
  ps.rij <- rlogis(n=n) #scale=1 means var=pi^2/3 
  -log(0.1/0.9) #10% treated 
  -log(0.3/0.7) #30% treated
  
  
  #higher portion treated log(0.2/0.8); log(0.4/0.6)
  
  if(ps_model == 1) {
    logit=rep(0,n)
    prob = 0.5
    break
  }
  if(ps_model == 2){ 
    logit <- int + 0.1*x1ij+0.03*x2ij+0.16*V1rep+0.08*V2rep+ps.rij
    prob <- 1/(1+exp(logit))
    break}
  if(ps_model == 3) {
    logit <- int +0.1*x1ij+0.03*x2ij+0.16*V1rep+0.08*V2rep+0.09*x2ij^2+0.25*V2rep^2+ps.rij
    prob <- 1/(1+exp(logit))
    break 
  }
  
  1/(1+exp(int))
  # the randomness has been added for rij  
  Stu.data$trt = 1*(prob>0.5) 
  Stu.data$trt = rbinom(n,size=1,prob=prob) #where to add the randomness 
  #another way remove 61's rij, line62 do prob>runif(1,0) instead 
  #logit > 0 for true/false = trt/ctrl
  #'test to see how it's the easiest 
  #'simulate one dataset with 1million then do assignments and fit the logistic regression, 0.1,0.03,0.08 probs
  Stu.data$ps = prob
 stu.trt <- ifelse(logit>0,1,0) #which students are treate 
 stu.prob.treated <- exp(stu.trt)/(1+exp(stu.trt))
  
  
  #generating CATE - tau 
  #treatment effect model 
  nij <- rnorm(n,0,tau_var)
  
  if(treatment_model == 1) {
    tau = 0.4
    break 
  }
  if(treatment_model == 2) {
    tau = 0.2*x1ij+0.6*x2ij+0.09*V1rep+0.16*V2rep+nij
    break
  }
  if(treatment_model == 3){
    tau = 0.2*x1ij+0.6*x2ij+0.09*V1rep+0.16*V2rep+0.25*x1ij*V1rep+0.25*V3rep+nij
    break
  }
  
  data$tau <- tau
  #generating outcome model 
  #outcome model 
  uj <- rnorm(n_cluster,0,var.lev2)
  ujrep <- rep(uj,each=n_ind)
  
  if(outcome_model==1) {
    yij <- 2.5+(0.3+tau)*trt+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
    break
  }
  if(outcome_model == 2) {
    yij <- 2.5+(0.3+tau)*trt + 0.4*x1ij-0.3*x2ij+x1ij*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
    break
  }
  if(outcome_model ==3) {
    yij <- 2.5+(0.3+tau)*trt + 0.4*x1ij-0.3*x2ij+x1ij*V1rep+0.4*V1rep-0.4*V2rep+0.16*V2rep+ujrep+eij
    break
  }
  
  data$yij = yij
  
  #generating distractors 
  #cov_mat_lev1 <- matrix(data=c(1,-0.1796,-0.04015,
    #                            -0.17962,1,0.46453,
     #                           -0.04015,0.4645,1), nrow=2,ncol=2)
  #cite from Dr.Leite's book chapter10
  cov_mat_lev1 <- matrix(data=c(1,-0.1796,
                              -0.1796,1),nrow=2,ncol=2)

  dist_lev1 <- as.data.frame(mvtnorm::rmvnorm(n=n,sigma=cov_mat_lev1))
  lev1.dist=data.frame(c(1:n),dist_lev1)
  names(lev1.dist1) <- c("Stu_id","dist11","dist12")
  #data <- cbind(data,dist_lev1)
  
  #cov_mat_lev2_1 <- matrix(data=c(0.9836,0.12494,-0.25794,
   #                               0.1249,0.88383,-0.05471,
    #                              -0.2579,-0.05471,0.17060), nrow=3,ncol=3)
  #cite from Dr.Leite's book chapter10 
  cov_mat_lev2 <- matrix(data=c(0.9836,0.12490,0.12490,0.9836),nrow=2,ncol=2)
  dist_lev2 <- as.data.frame(mvtnorm::rmvnorm(n=n_cluster,sigma=cov_mat_lev2))
  
  lev2.dist=data.frame(c(1:n_cluster),dist_lev2)
  names(lev2.dist) <- c("School_id","dist21","dist22")
  
  Stu.data = merge(Stu.data,lev1.dist1, by="Stu_id")
  data = merge(data, lev2.dist, by="School_id")
  
  
  return(Stu.data)
  
  
}



#' looking for papers to build on for super learner methods of multilevel ps 
#' in SL if the super learner to use different ones of framework, random forest, SVM do have constrains of time 
#' may only use easier SL for the sake of time 
#' do testing for the time used in each method 

