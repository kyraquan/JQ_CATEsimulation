list.of.packages <- c("foreign", "quantreg", "gbm", "glmnet","MASS",
                      "MASS", "rpart", "nnet", "matrixStats","simstudy",
                      "xtable", "readstata13","grf","remotes","party",
                      "caret",  "multcomp","cowplot","SuperLearner",
                      "ranger","reshape2","gridExtra","bartCause","xgboost","bartMachine",
                      "googlesheets4","googledrive","tidyverse","rJava",
                      "Rtools","PKI", "googlesheets4","googledrive","SuperLearner",
                      "grf")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

remotes::install_github("vdorie/bartCause") 
install.packages("SuperLearner",dependencies = TRUE)
install.packages("PKI", type="source")

source("function_BART.R") #can comment out
source("function_CF.R") #can comment out
source("function_evaluations.R")
source("function_SLearner_est.R")
source("function_TLearner_est.R")
source("function_XLearner_est.R")
source("simulation_function.R")