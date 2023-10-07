install.packages("devtools")
devtools::install_github("PolMine/RcppCWB")

vec.pac= c("foreign", "quantreg", "gbm", "glmnet","MASS",
           "MASS", "rpart", "nnet", "matrixStats","simstudy",
           "xtable", "readstata13","grf","remotes",
           "caret",  "multcomp","cowplot","SuperLearner",
           "ranger","reshape2","gridExtra","bartCause","xgboost","bartMachine",
           "googlesheets4","googledrive","tidyverse")
install.packages(vec.pac)
remotes::install_github("vdorie/bartCause")

install.packages("party")
install.packages("bartMachine")
install.packages("rJava")
install.packages("Rtools")
install.packages("PKI", type="source")
install.packages("foreach", dependencies = TRUE)
install.packages("parallel", dependencies = TRUE)

library(foreach)
library(googlesheets4)
library(googledrive)
library(tidyverse)




source("function_BART.R")
source("function_CF.R")
source("function_evaluations.R")
source("function_SLearner_est.R")
source("function_TLearner_est.R")
source("function_XLearner_est.R")
source("simulation_function.R")