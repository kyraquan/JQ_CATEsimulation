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
install.packages(c("simstudy", "party", "readstata13", "grf",  "SuperLearner", "bartMachine", "parallel"), dependencies = TRUE, lib=Sys.getenv("R_LIBS_USER"), repos='http://cran.rstudio.com/')
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
