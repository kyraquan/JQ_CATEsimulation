install.packages(c("simstudy", "party", "readstata13", "grf",  "SuperLearner", "bartMachine", "AzureStor"), dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/')
#remotes::install_github("vdorie/bartCause", dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"))

source("function_evaluations.R")
source("function_TLearner_est.R")

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
#BARTresults <- "BARTresults.csv"
#CFresults <- "CFresults.csv"
#SL1results <- "SL1results.csv"
#SL2results <- "SL2results.csv"
#SL3results <- "SL3results.csv"
#TL1results <- "TL1results.csv"
TL2results <- "TL2results.csv"
#TL3results <- "TL3results.csv"
#XL1results <- "XL1results.csv"
#XL2results <- "XL2results.csv"
#XL3results <- "XL3results.csv"



#initialize the file names with headers 
headers<-data.frame( METHOD = character(),
                     level2n=numeric(),level1n=numeric(),ICC=numeric(), PSmodel=numeric(),treatment_model=numeric(),Outcomemodel=numeric(),
                     Proportion=numeric(),tau_var=numeric(),
                     cate.PEHE_train=numeric(),cate.RBias_train=numeric(),catevar.Rbias_train=numeric(),
                     cate.PEHE_test=numeric(),cate.RBias_train=numeric(),catevar.Rbias_test=numeric(),
                     ps.MSE_train=numeric(),ps.corr_train=numeric(),ps.MSE_test=numeric(),ps.corr_test=numeric(),
                     train.ate=numeric(),test.ate=numeric(),train.ate.true=numeric(),test.ate.true=numeric(),
                     stringsAsFactors = FALSE)
#write.table(headers, file = BARTresults, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = CFresults, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = SL1results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = SL2results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = SL3results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = TL1results, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = TL2results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = TL3results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = XL1results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = XL2results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = XL3results, sep = ",", row.names = FALSE, quote = FALSE)



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

print(paste("Finished iteration ", i))
} #close the while loop


storage_upload(cont, src=TL2results, dest=assemble_path("non-parrelel-run", task_id, TL2results))


simulation.end = proc.time()
total.time = simulation.end - simulation.start
stopCluster(cl)
