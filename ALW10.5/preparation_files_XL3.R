install.packages(c("party", "readstata13", "grf",  "SuperLearner", "bartMachine", "AzureStor"), dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/')
remotes::install_github("vdorie/bartCause", dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"))

source("function_evaluations.R")
source("function_XLearner_est.R")

print("source loaded")


library(AzureStor)

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

XL3results <- "XL3results.csv"



#initialize the file names with headers 
headers<-data.frame( METHOD = character(),
                     level2n=numeric(),level1n=numeric(),ICC=numeric(), PSmodel=numeric(),treatment_model=numeric(),Outcomemodel=numeric(),
                     Proportion=numeric(),tau_var=numeric(),
                     cate.PEHE_train=numeric(),cate.RBias_train=numeric(),catevar.Rbias_train=numeric(),
                     cate.PEHE_test=numeric(),cate.RBias_train=numeric(),catevar.Rbias_test=numeric(),
                     ps.MSE_train=numeric(),ps.corr_train=numeric(),ps.MSE_test=numeric(),ps.corr_test=numeric(),
                     train.ate=numeric(),test.ate=numeric(),train.ate.true=numeric(),test.ate.true=numeric(),
                     stringsAsFactors = FALSE)

write.table(headers, file = XL3results, sep = ",", row.names = FALSE, quote = FALSE)



NumIter=100 #ideal 1000 
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

Proportion=0.8472979

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
} #close the while loop


storage_upload(cont, src=XL3results, dest=assemble_path("non-parrelel-run", task_id, XL3results))


simulation.end = proc.time()
total.time = simulation.end - simulation.start
stopCluster(cl)
