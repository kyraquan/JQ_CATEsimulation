vec.pac= c("foreign", "quantreg", "gbm", "glmnet","MASS",
           "MASS", "rpart", "nnet", "matrixStats","simstudy",
           "xtable", "readstata13","grf","remotes",
           "caret",  "multcomp","cowplot","SuperLearner",
           "ranger","reshape2","gridExtra","bartCause","xgboost","bartMachine","nnet")
install.packages(vec.pac)
remotes::install_github("vdorie/bartCause")


source("function_BART.R")
source("function_CF.R")
source("function_evaluations.R")
source("function_SLearner_est.R")
source("function_TLearner_est.R")
source("function_XLearner_est.R")
source("simulation_function.R")