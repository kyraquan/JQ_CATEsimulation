install.packages(c("party", "readstata13", "grf",  "SuperLearner", "bartMachine", "AzureStor"), dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/')

source("function_evaluations.R")
source("function_SLearner_est.R")

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
SL1results <- "SL1results.csv"
#SL2results <- "SL2results.csv"
#SL3results <- "SL3results.csv"
#TL1results <- "TL1results.csv"
#TL2results <- "TL2results.csv"
#TL3results <- "TL3results.csv"
#XL1results <- "XL1results.csv"
#XL2results <- "XL2results.csv"
#XL3results <- "XL3results.csv"

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
#write.table(headers, file = BARTresults, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = CFresults, sep = ",", row.names = FALSE, quote = FALSE)
write.table(headers, file = SL1results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = SL2results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = SL3results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = TL1results, sep = ",", row.names = FALSE, quote = FALSE)
#write.table(headers, file = TL2results, sep = ",", row.names = FALSE, quote = FALSE)
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

datatest = read.csv(paste0(i,".csv"))
datatest$School_id = factor(datatest$School_id)
#datatest = PS_model_data(n_cluster=level2n, n_ind=level1n,ICC=ICC,int=Proportion,tau_var=tau_var,
#                         ps_model = PS_model,treatment_model = treatment_model,outcome_model = Outcome_model)

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



print(paste("Finished iteration ", i))
} #close the while loop


storage_upload(cont, src=SL1results, dest=assemble_path("non-parrelel-run", task_id, SL1results))

simulation.end = proc.time()
total.time = simulation.end - simulation.start
stopCluster(cl)
