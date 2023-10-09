#install.packages("devtools")
#devtools::install_github("PolMine/RcppCWB")

#vec.pac= c("foreign", "quantreg", "gbm", "glmnet","MASS",
         #  "rpart", "nnet", "matrixStats",
     #      "xtable",
     #      "caret",  "multcomp","cowplot",
     #      "ranger","reshape2","gridExtra","xgboost",
    #       "tidyverse")

# separate installation [simstudy, readstata13, grf, remotes, SuperLearner, bartCause, bartMachine,googlesheets4, googledrive]
#install.packages(vec.pac)
install.packages(c("simstudy", "party", "readstata13", "grf",  "SuperLearner", "bartMachine", "AzureStor"), dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/')
remotes::install_github("vdorie/bartCause", dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"))

#install.packages("bartMachine")
#install.packages("rJava")
#install.packages("Rtools")
#install.packages("PKI", type="source")

#library(tidyverse)

source("simulation_function.R")
source("function_BART.R")
source("function_CF.R")
source("function_evaluations.R")
source("function_SLearner_est.R")
source("function_TLearner_est.R")
source("function_XLearner_est.R")

print("source loaded")

# Load the parallel package
library(parallel)
library(AzureStor)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
print(paste("Number of Cores", numCores))

access_sas <- Sys.getenv("access_sas")
bl_endp_key <- storage_endpoint("https://jiaquansimulation.blob.core.windows.net", sas=access_sas)

cont <- storage_container(bl_endp_key, "simulationresult")

#job related envs
job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
assemble_path <- function(job_id, task_id, file_name) {
  path <- file.path(job_id, task_id, file_name)
  return(path)
}

#file names 
BARTresults <- "BARTresults.csv"
CFresults <- "CFresults.csv"
SL1results <- "SL1results.csv"
SL2results <- "SL2results.csv"
SL3results <- "SL3results.csv"
TL1results <- "TL1results.csv"
TL2results <- "TL2results.csv"
TL3results <- "TL3results.csv"
XL1results <- "XL1results.csv"
XL2results <- "XL2results.csv"
XL3results <- "XL3results.csv"

# Assuming this file doesn't exist yet or you want to overwrite any previous content.
#if (file.exists(file_name)) {
#  file.remove(file_name)
#}


#initialize the file names with headers 
headers<-data.frame( METHOD = character(),
                     level2n=numeric(),level1n=numeric(),ICC=numeric(), PSmodel=numeric(),treatment_model=numeric(),Outcomemodel=numeric(),
                     Proportion=numeric(),tau_var=numeric(),
                     cate.PEHE_train=numeric(),cate.RBias_train=numeric(),catevar.Rbias_train=numeric(),
                     cate.PEHE_test=numeric(),cate.RBias_train=numeric(),catevar.Rbias_test=numeric(),
                     ps.MSE_train=numeric(),ps.corr_train=numeric(),ps.MSE_test=numeric(),ps.corr_test=numeric(),
                     train.ate=numeric(),test.ate=numeric(),train.ate.true=numeric(),test.ate.true=numeric(),
                     stringsAsFactors = FALSE)
write.table(headers, file = BARTresults, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = CFresults, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = SL1results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = SL2results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = SL3results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = TL1results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = TL2results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = TL3results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = XL1results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = XL2results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = XL3results, sep = ",", row.names = FALSE, quote = FALSE)



NumIter=1 #ideal 1000 
simulation.start = proc.time()

#committee to read the articles get dissertation to committee on 20th. 2 weeks and a half. 
covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")

# Get environment variable values as strings
level2n_str <- Sys.getenv("level2n")
level1n_str <- Sys.getenv("level1n")
ICC_str <- Sys.getenv("ICC")
PS_model_str <- Sys.getenv("PS_model")
treatment_model_str <- Sys.getenv("treatment_model")
Outcome_model_str <- Sys.getenv("Outcome_model")
tau_var_str <- Sys.getenv("tau_var")

# Convert to numeric if they are numeric values
level2n <- as.numeric(level2n_str)
level1n <- as.numeric(level1n_str)
ICC <- as.numeric(ICC_str)
PS_model <- as.numeric(PS_model_str)
treatment_model <- as.numeric(treatment_model_str)
Outcome_model <- as.numeric(Outcome_model_str)
tau_var <- as.numeric(tau_var_str)

#for (level2n in c(60,200,500)) { #number of groups 
#for (level1n in c(10,30,60)) { #number of individuals 
#   for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
#    for (PS_model in c(1,2,3)) {#propensity score model
#      for (treatment_model in c(1,2,3)) { #CATE model 
#        for (Outcome_model in c(1,2,3)) { #Outcome model 
#        for (Proportion in c(2.197225,0.8472979)) { #proportion treated 
#           for (tau_var in c(0.1,0.47)) {

Proportion=0.8472979
#results.cond = NULL  #storage of results for each condition.
#i = 1 #initiates the count of converged iterations
#Iters.run = 0 #initiates the count of iterations run.
#start.time = proc.time() #store starting computer time.


for (i in 1:NumIter) { 

datatest = PS_model_data(n_cluster=level2n, n_ind=level1n,ICC=ICC,int=Proportion,tau_var=tau_var,
                         ps_model = PS_model,treatment_model = treatment_model,outcome_model = Outcome_model)

#split the dataset into training and test data
train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(datatest)
test.sample = test.sample[-c(train.sample)]

data.train = datatest[train.sample,]
data.test = datatest[test.sample,]
data.train = as.data.frame(data.train)
data.test = as.data.frame(data.test)

conditions <- c(level2n,level1n,ICC, PS_model,treatment_model,Outcome_model,Proportion,tau_var)

#run each method 

clusterExport(cl, c("Results_evaluation", "conditions", "level2n", "level1n", "ICC", "PS_model","treatment_model","Outcome_model","Proportion","tau_var", "BARTresults",
                    "CFresults",
                    "SL1results",
                    "SL2results",
                    "SL3results",
                    "TL1results",
                    "TL2results",
                    "TL3results",
                    "XL1results",
                    "XL2results",
                    "XL3results",
                    "CF_est","SLearner_est","XLearner_est","TLearner_est","BART_est","PS_model_data","datatest",
                    "data.train", "data.test", "covariates"
))

tasks <- list(
  function() {
    ############################### BART analysis
    
    
    BARTresult <- try(BART_est(data.train,data.test),silent = F)
    if(class(BARTresult)[[1]]=="try_error") {#is this for list? 
      evalBART <- rep(-999,14) 
      evalBART <- c("BART",conditions,evalBART)
    }else{ evalBART <- Results_evaluation(BARTresult,datatest)
    evalBART <- c("BART",conditions,evalBART)}
    
    length(evalBART)
    evalBART[1]
    
    evalBART_df <- NULL
    # Create a dataframe for this result
    evalBART_df <- data.frame(
      METHOD = "BART",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSmodel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalBART[10],cate.RBias_train=evalBART[11],catevar.Rbias_train=evalBART[12],
      cate.PEHE_test=evalBART[13],cate.RBias_train=evalBART[14],catevar.Rbias_test=evalBART[15],
      ps.MSE_train=evalBART[16],ps.corr_train=evalBART[17],ps.MSE_test=evalBART[18],ps.corr_test=evalBART[19],
      train.ate=evalBART[20],test.ate=evalBART[21],train.ate.true=evalBART[22],test.ate.true=evalBART[23],
      stringsAsFactors = FALSE)
    
    # Append result to the CSV file
    write.table(evalBART_df, file = BARTresults, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #write.table(t(evalBART), file = BARTresults, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #BARTresults.cond = rbind(BARTresults.cond,append(c(evalBART)))
    print(evalBART_df)
    print("BART finished")
    ############################### BART finished##################
  },
  function() {
    CFresult <- try(CF_est(data.train,data.test,covariates),silent = F)
    if(class(CFresult)[[1]] =="try_error"){
      evalCF <-rep(-999,14) 
      evalCF <- c("CF",conditions,evalCF)
    }else{ evalCF <- Results_evaluation(CFresult,datatest)
    evalCF <- c("CF",conditions,evalCF)} 
    
    # Create a dataframe for this result
    evalCF_df <- data.frame(
      METHOD = "CF",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSmodel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalCF[10],cate.RBias_train=evalCF[11],catevar.Rbias_train=evalCF[12],
      cate.PEHE_test=evalCF[13],cate.RBias_train=evalCF[14],catevar.Rbias_test=evalCF[15],
      ps.MSE_train=evalCF[16],ps.corr_train=evalCF[17],ps.MSE_test=evalCF[18],ps.corr_test=evalCF[19],
      train.ate=evalCF[20],test.ate=evalCF[21],train.ate.true=evalCF[22],test.ate.true=evalCF[23],
      stringsAsFactors = FALSE)
    
    write.table(evalCF_df, file = CFresults, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    print(evalCF_df)
    print("CF finished")
    #CFresults.cond=rbind(CFresults.cond,append(conditions,evalCF))
    ########### CF finished 
  },
  function() {
    ######## SL1 anal
    SLearnerresult1 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
    if(class(SLearnerresult1)[[1]] =="try_error") {
      evalSL1 <-rep(-999,14)
      evalSL1 <- c("SL1",conditions,evalSL1)
    }else{evalSL1 <-Results_evaluation(SLearnerresult1,datatest)
    evalSL1 <- c("SL1",conditions,evalSL1)}
    
    # Create a dataframe for this result
    evalSL1_df <- data.frame(
      METHOD = "SL1",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalSL1[10],cate.RBias_train=evalSL1[11],catevar.Rbias_train=evalSL1[12],
      cate.PEHE_test=evalSL1[13],cate.RBias_train=evalSL1[14],catevar.Rbias_test=evalSL1[15],
      ps.MSE_train=evalSL1[16],ps.corr_train=evalSL1[17],ps.MSE_test=evalSL1[18],ps.corr_test=evalSL1[19],
      train.ate=evalSL1[20],test.ate=evalSL1[21],train.ate.true=evalSL1[22],test.ate.true=evalSL1[23],
      stringsAsFactors = FALSE)
    
    write.table(evalSL1_df, file = SL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    #write.table(t(evalSL1), file = SL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #SL1results.cond=rbind(SL1results.cond,append(conditions,evalSL1))
    
    
  },
  function() {
    
    SLearnerresult2 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
    if(class(SLearnerresult2)[[1]] =="try_error") {
      evalSL2 <-rep(-999,14)
      evalSL2 <- c("SL2",conditions,evalSL2)
    }else{ evalSL2 <- Results_evaluation(SLearnerresult2,datatest)
    evalSL2 <- c("SL2",conditions,evalSL2)}
    
    # Create a dataframe for this result
    evalSL2_df <- data.frame(
      METHOD = "SL2",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalSL2[10],cate.RBias_train=evalSL2[11],catevar.Rbias_train=evalSL2[12],
      cate.PEHE_test=evalSL2[13],cate.RBias_train=evalSL2[14],catevar.Rbias_test=evalSL2[15],
      ps.MSE_train=evalSL2[16],ps.corr_train=evalSL2[17],ps.MSE_test=evalSL2[18],ps.corr_test=evalSL2[19],
      train.ate=evalSL2[20],test.ate=evalSL2[21],train.ate.true=evalSL2[22],test.ate.true=evalSL2[23],
      stringsAsFactors = FALSE)
    
    write.table(evalSL2_df, file = SL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    #write.table(t(evalSL2), file = SL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    #SL2results.cond=rbind(SL2results.cond,append(conditions,evalSL2))
    
  },
  function() {
    SLearnerresult3 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
    if(class(SLearnerresult3)[[1]] =="try_error") {
      evalSL3 <-rep(-999,14)
      evalSL3 <- c("SL3",conditions,evalSL3)
    }else{evalSL3 <- Results_evaluation(SLearnerresult3,datatest)
    evalSL3 <- c("SL3",conditions,evalSL3) }
    #write.table(t(evalSL3), file = SL3results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    evalSL3_df <- data.frame(
      METHOD = "SL3",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalSL3[10],cate.RBias_train=evalSL3[11],catevar.Rbias_train=evalSL3[12],
      cate.PEHE_test=evalSL3[13],cate.RBias_train=evalSL3[14],catevar.Rbias_test=evalSL3[15],
      ps.MSE_train=evalSL3[16],ps.corr_train=evalSL3[17],ps.MSE_test=evalSL3[18],ps.corr_test=evalSL3[19],
      train.ate=evalSL3[20],test.ate=evalSL3[21],train.ate.true=evalSL3[22],test.ate.true=evalSL3[23],
      stringsAsFactors = FALSE)
    
    write.table(evalSL3_df, file = SL3results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    print("SL finished")
    #SL3results.cond=rbind(SL3results.cond,append(conditions,evalSL3))
  },
  function() {
    TLearnerresult1 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
    if(class(TLearnerresult1)[[1]] =="try_error") {
      evalTL1 <-rep(-999,14)
      evalTL1 <- c("TL1",conditions,evalTL1) 
    }else{evalTL1 <- Results_evaluation(TLearnerresult1,datatest)
    evalTL1 <- c("TL1",conditions,evalTL1) }
    #write.table(t(evalTL1), file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #TL1results.cond=rbind(TL1results.cond,append(conditions,evalTL1))
    # Create a dataframe for this result
    evalTL1_df <- data.frame(
      METHOD = "TL1",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalTL1[10],cate.RBias_train=evalTL1[11],catevar.Rbias_train=evalTL1[12],
      cate.PEHE_test=evalTL1[13],cate.RBias_train=evalTL1[14],catevar.Rbias_test=evalTL1[15],
      ps.MSE_train=evalTL1[16],ps.corr_train=evalTL1[17],ps.MSE_test=evalTL1[18],ps.corr_test=evalTL1[19],
      train.ate=evalTL1[20],test.ate=evalTL1[21],train.ate.true=evalTL1[22],test.ate.true=evalTL1[23],
      stringsAsFactors = FALSE)
    
    write.table(evalTL1_df, file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
  },
  function () {
    TLearnerresult2 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
    if(class(TLearnerresult2)[[1]] =="try_error") {
      evalTL2 <-rep(-999,14)
      evalTL2 <- c("TL2",conditions,evalTL2)
    }else{evalTL2 <- Results_evaluation(TLearnerresult2,datatest)
    evalTL2 <- c("TL2",conditions,evalTL2) }
    #write.table(t(evalTL2), file = TL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #TL2results.cond=rbind(TL2results.cond,append(conditions,evalTL2))
    evalTL2_df <- data.frame(
      METHOD = "TL2",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalTL2[10],cate.RBias_train=evalTL2[11],catevar.Rbias_train=evalTL2[12],
      cate.PEHE_test=evalTL2[13],cate.RBias_train=evalTL2[14],catevar.Rbias_test=evalTL2[15],
      ps.MSE_train=evalTL2[16],ps.corr_train=evalTL2[17],ps.MSE_test=evalTL2[18],ps.corr_test=evalTL2[19],
      train.ate=evalTL2[20],test.ate=evalTL2[21],train.ate.true=evalTL2[22],test.ate.true=evalTL2[23],
      stringsAsFactors = FALSE)
    
    write.table(evalTL2_df, file = TL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
  },
  function() {
    TLearnerresult3 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
    if(class(TLearnerresult3)[[1]] =="try_error") {
      evalTL3 <-rep(-999,14)
      evalTL3 <- c("TL3",conditions,evalTL3) 
    }else{evalTL3 <- Results_evaluation(TLearnerresult3,datatest)
    evalTL3 <- c("TL3",conditions,evalTL3) }
    #write.table(t(evalTL3), file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #TL3results.cond=rbind(TL3results.cond,append(conditions,evalTL3))
    evalTL3_df <- data.frame(
      METHOD = "TL3",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalTL3[10],cate.RBias_train=evalTL3[11],catevar.Rbias_train=evalTL3[12],
      cate.PEHE_test=evalTL3[13],cate.RBias_train=evalTL3[14],catevar.Rbias_test=evalTL3[15],
      ps.MSE_train=evalTL3[16],ps.corr_train=evalTL3[17],ps.MSE_test=evalTL3[18],ps.corr_test=evalTL3[19],
      train.ate=evalTL3[20],test.ate=evalTL3[21],train.ate.true=evalTL3[22],test.ate.true=evalTL3[23],
      stringsAsFactors = FALSE)
    
    write.table(evalTL3_df, file = TL3results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    print("TL finished")
  },
  function() {
    XLearnerresult1 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
    if(class(XLearnerresult1)[[1]] =="try_error") {  
      evalXL1 <-rep(-999,14)
      evalXL1 <- c("XL1",conditions,evalXL1)
    }else{evalXL1 <- Results_evaluation(XLearnerresult1,datatest)
    evalXL1 <- c("XL1",conditions,evalXL1) }
    #write.table(t(evalXL1), file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #XL1results.cond=rbind(XL1results.cond,append(conditions,evalXL1))
    evalXL1_df <- data.frame(
      METHOD = "XL1",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalXL1[10],cate.RBias_train=evalXL1[11],catevar.Rbias_train=evalXL1[12],
      cate.PEHE_test=evalXL1[13],cate.RBias_train=evalXL1[14],catevar.Rbias_test=evalXL1[15],
      ps.MSE_train=evalXL1[16],ps.corr_train=evalXL1[17],ps.MSE_test=evalXL1[18],ps.corr_test=evalXL1[19],
      train.ate=evalXL1[20],test.ate=evalXL1[21],train.ate.true=evalXL1[22],test.ate.true=evalXL1[23],
      stringsAsFactors = FALSE)
    
    write.table(evalXL1_df, file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    
    
  },
  function() {
    
    XLearnerresult2 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
    if(class(XLearnerresult2)[[1]] =="try_error") {
      evalXL2 <-rep(-999,14)
      evalXL2 <- c("XL2",conditions,evalXL2) 
    }else{evalXL2 <- Results_evaluation(XLearnerresult2,datatest)
    evalXL2 <- c("XL2",conditions,evalXL2) }
    #write.table(t(evalXL2), file = XL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #XL2results.cond=rbind(XL2results.cond,append(conditions,evalXL2))
    evalXL2_df <- data.frame(
      METHOD = "XL2",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalXL2[10],cate.RBias_train=evalXL2[11],catevar.Rbias_train=evalXL2[12],
      cate.PEHE_test=evalXL2[13],cate.RBias_train=evalXL2[14],catevar.Rbias_test=evalXL2[15],
      ps.MSE_train=evalXL2[16],ps.corr_train=evalXL2[17],ps.MSE_test=evalXL2[18],ps.corr_test=evalXL2[19],
      train.ate=evalXL2[20],test.ate=evalXL2[21],train.ate.true=evalXL2[22],test.ate.true=evalXL2[23],
      stringsAsFactors = FALSE)
    
    write.table(evalXL2_df, file = XL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    
    
  },
  function() {
    XLearnerresult3 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
    if(class(XLearnerresult3)[[1]] =="try_error") {
      evalXL3 <-rep(-999,14)
      evalXL3 <- c("XL3",conditions,evalXL3) 
    }else{evalXL3 <- Results_evaluation(XLearnerresult3,datatest)
    evalXL3 <- c("XL3",conditions,evalXL3) }
    #write.table(t(evalXL1), file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    #XL3results.cond=rbind(XL3results.cond,append(conditions,evalXL3))
    evalXL3_df <- data.frame(
      METHOD = "XL3",level2n=level2n,level1n=level1n,ICC=ICC, 
      PSModel=PS_model,treatment_model=treatment_model,Outcomemodel=Outcome_model,Proportion=Proportion,tau_var=tau_var,
      cate.PEHE_train=evalXL3[10],cate.RBias_train=evalXL3[11],catevar.Rbias_train=evalXL3[12],
      cate.PEHE_test=evalXL3[13],cate.RBias_train=evalXL3[14],catevar.Rbias_test=evalXL3[15],
      ps.MSE_train=evalXL3[16],ps.corr_train=evalXL3[17],ps.MSE_test=evalXL3[18],ps.corr_test=evalXL3[19],
      train.ate=evalXL3[20],test.ate=evalXL3[21],train.ate.true=evalXL3[22],test.ate.true=evalXL3[23],
      stringsAsFactors = FALSE)
    
    write.table(evalXL3_df, file = XL3results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
    
    
    
    print("XL finished")
  }
  
)
############ CF analysis 



#results.cond = rbind(results.cond,append(conditions,analysis))

#count number of iterations so far.
#count number of iterations so far.
results <- clusterApply(cl, tasks, fun = function(f) f())
print(paste("Finished iteration ", i))
} #close the while loop

storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, BARTresults))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, CFresults))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, SL1results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, SL2results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, SL3results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, TL1results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, TL2results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, TL3results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, XL1results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, XL2results))
storage_upload(cont, src=BARTresults, dest=assemble_path(job_id, task_id, XL3results))


#end.time = proc.time()
#condition.time = end.time - start.time
#print(c(condition.time[3],condition.time[3]/i,
#       level2n,level1n,ICC,Itau_va,PSmodel,CATEmodel,Outcomemodel,Proportion,
#        Iters.run))



# }


#run machinelearning methods

#put no column names or row names 
# write table

#write.csv(resultslist,paste0("Results", level2n,"-",level1n,"-",ICC,"-",tau_var,"-",PSmodel,"-",CATEmodel,"-",Outcomemodel,"-",Proportion)) #make sure  append=TRUE
# }}}}}}

simulation.end = proc.time()
total.time = simulation.end - simulation.start
stopCluster(cl)
