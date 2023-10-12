#vec.pac= c("foreign", "quantreg", "gbm", "glmnet",
#           "MASS", "rpart", "nnet", "matrixStats",
#           "xtable", "readstata13","grf","remotes",
#           "caret",  "multcomp","cowplot","SuperLearner",
#           "ranger","reshape2","gridExtra","bartCause","xgboost","bartMachine","nnet")
#install.packages(vec.pac)
#remotes::install_github("vdorie/bartCause")
# Get the current R version
current_version <- R.version$version.string

# Print the R version
cat("Current R Version:", current_version, "\n")
# options(repos = c(CRAN = "https://cran.r-project.org"))

# Only install simustudy because Mass is already installed per https://help.rc.ufl.edu/doc/R
# Additional packages need to be installed as personal library due to hyper gator permission issue
#install.packages("simstudy", dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/', type = "source")
# library("MASS", lib.loc=Sys.getenv("R_LIBS_USER"))
# library("simstudy", lib.loc=Sys.getenv("R_LIBS_USER"))
PS_model_data = function(n_cluster, #the number of cluster 
                         n_ind, #the number of individual 
                         int, #overall mean of ps model (intercept) to be treated; 0.8472979 - 30% treated
                         tau_var,
                         ICC, #for outcome model only,
                         ps_model, # 1=constant, 2 = no interaction, 3=interactions
                         treatment_model,# 1=constant, 2 = no interaction, 3=interaction
                         outcome_model # 1=constant, 2 = no interaction, 3=interaction
                         
) {
  # step 1: specifying the group random effect using ICC for outcome model only 

  #targetICC <- c(0.1)
  var.lev1.res = 1
  setVars <- iccRE(ICC,dist = "normal",varWithin = var.lev1.res) #variance of level-1 residual in outcome model 
  var.lev2 <- setVars
  
  # end step 1
  #=======================================================================================# 
  #n_cluster=3; n_ind=10
  #step 2: making level-1 and level-2 covariates 
  #simulate level-1 residual for the ps model and outcome 
  lev1.res = rnorm(n_cluster*n_ind,0,var.lev1.res) #residual for continuous outcome model
  #lev1.res.t = rlogis(n_cluster*n_ind) #ps model residual 
  
  #generating level-1 covariates 
  #n_ind=3
  #n_cluster=6
  n <- n_ind*n_cluster
  
  Stu_id = c(1:n)
  School_id = as.data.frame(gl(n_cluster,k=n_ind))
  names(School_id) <- c("School_id")
  data<- as.data.frame(School_id)
  
  data$Stu_id <- Stu_id
  
  x1ij <- rbinom(n,1,0.5) #level-1, binary
  x2ij <- rnorm(n,0,1) #level-1, continuous
  
  data$x1ij <- x1ij 
  data$x2ij <- x2ij
  #lev1.pred = as.data.frame(cbind(x1ij,x2ij))
  #lev1.pred=data.frame(Stu_id,lev1.pred)
  
  #generating level-2 covariates 
  v1j <- rbinom(n_cluster,1,0.3) #level-2, binary
  v2j <- runif(n_cluster,0,1) #level-2, uniform
  v3j <- rnorm(n_cluster,0,1) #level-2, continuous 
  v1rep <- rep(v1j,each=n_ind)
  v2rep <- rep(v2j,each=n_ind)
  v3rep <- rep(v3j,each=n_ind)
  
  data$v1rep <- v1rep 
  data$v2rep <- v2rep
  data$v3rep <- v3rep
  
  #lev2.pred=as.data.frame(cbind(School_id,v1rep,v2rep,v3rep))
  
  #Stu.data = as.data.frame(cbind(lev1.pred, lev2.pred))
  
  
  #generating PS
  #proportion treated for int=2.197 (10% treated), if plan to manipulate it, reflecting the proportion treated
  rij <- rlogis(n=n) #scale=1 means var=pi^2/3 
  #-log(0.1/0.9) #10% treated 
  #-log(0.3/0.7) #30% treated


  #higher portion treated log(0.2/0.8); log(0.4/0.6)
  
  if(ps_model == 1) {
    logit=rep(0,n)
    prob = 0.5
    trt = rbinom(n,1,prob=0.5)
    data$ps= 0.5 
    data$trt = trt
  }else if(ps_model == 2){ 
    logit <- int + 0.1*x1ij+0.03*x2ij+0.16*v1rep+0.08*v2rep+rij
    prob <- 1/(1+exp(logit)) 
    trt <- ifelse(logit>0,0,1)
    data$ps = prob
    data$trt = trt
    
    } else if(ps_model == 3) {
    logit <- int +0.1*x1ij+0.03*x2ij+0.16*v1rep+0.08*v2rep+0.09*x2ij^2+0.25*v2rep^2+rij
    prob <- 1/(1+exp(logit))
    trt <- ifelse(logit>0,0,1)
    data$ps = prob
    data$trt = trt
  }

  # the randomness has been added for rij  
  #Stu.data$trt = 1*(prob>0.5) 
  #Stu.data$trt = rbinom(n,size=1,prob=prob) #where to add the randomness 
  #another way remove 61's rij, line62 do prob>runif(1,0) instead 
  #logit > 0 for true/false = trt/ctrl
  #'test to see how it's the easiest 
  #'simulate one dataset with 1million then do assignments and fit the logistic regression, 0.1,0.03,0.08 probs
  #data$ps = prob
  #data$trt = ifelse(logit>0,0,1) #treated is 1, control is 0 
  #trt <- ifelse(logit>0,0,1)


  #generating CATE - tau 
  #treatment effect model 
  nij <- rnorm(n,0,tau_var)
  
  if(treatment_model == 1) {
    tau = 0.4
  }else if(treatment_model == 2) {
    tau = 0.2*x1ij+0.6*x2ij+0.09*v1rep+0.16*v2rep+nij
  }else if(treatment_model == 3){
    tau = 0.2*x1ij+0.6*x2ij+0.09*v1rep+0.16*v2rep+0.25*x1ij*v1rep+0.25*(v3rep^2)+nij
  }
  data$tau = tau
  data$treateff = tau+0.3 #the effect of the treated 
  #generating outcome model 
  #outcome model 
  uj <- rnorm(n_cluster,0,var.lev2)
  ujrep <- rep(uj,each=n_ind)
  eij <- rnorm(n,mean=0,sd=1)
  
  if(outcome_model==1) {
    yij <- 1+(0.3+tau)*trt+0.4*x1ij-0.3*x2ij+0.4*v1rep-0.4*v2rep+ujrep+eij
  }else if(outcome_model == 2) {
    yij <- 1+(0.3+tau)*trt + 0.4*x1ij-0.3*x2ij+x1ij*x2ij+0.4*v1rep-0.4*v2rep+ujrep+eij
  }else if(outcome_model ==3) {
    yij <- 1+(0.3+tau)*trt + 0.4*x1ij-0.3*x2ij+x1ij*v1rep+0.4*v1rep-0.4*v2rep+0.16*v2rep+ujrep+eij
  }
  
  data$yij = yij
  
  #generating distractors 
  cov_mat_lev1 <- matrix(data=c(1,-0.1796,
                                -0.1796,1),nrow=2,ncol=2)
  
  dist_lev1 <- as.data.frame(mvtnorm::rmvnorm(n=n,sigma=cov_mat_lev1))
  lev1.dist=data.frame(c(1:n),dist_lev1)
  names(lev1.dist) <- c("Stu_id","X3","X4")
  #cite from Dr.Leite's book chapter10
  
  cov_mat_lev2 <- matrix(data=c(0.9836,0.12490,0.12490,0.9836),nrow=2,ncol=2)
  dist_lev2 <- as.data.frame(mvtnorm::rmvnorm(n=n_cluster,sigma=cov_mat_lev2))
  
  lev2.dist=data.frame(c(1:n_cluster),dist_lev2)
  names(lev2.dist) <- c("School_id","V4","V5")
  
  data = merge(data,lev1.dist, by="Stu_id")
  data = merge(data, lev2.dist, by="School_id")
  
  
  return(data)
  
  
}

#names(data)[11:14] <- c("X3","X4","V4","V5")

#' looking for papers to build on for super learner methods of multilevel ps 
#' in SL if the super learner to use different ones of framework, random forest, SVM do have constrains of time 
#' may only use easier SL for the sake of time 
#' do testing for the time used in each method 

#datatest <- PS_model_data(n_cluster=60,n_ind=10,ICC=0.1,int=0.8472979,ps_model = 3,treatment_model = 1,outcome_model = 1)

#print(datatest)
#sum(datatest$trt)/40
#16/40


#BARTtest
