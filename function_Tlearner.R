#splitting train and test 
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.6,0.4))
data.train  <- data[sample, ]
data.test   <- data[!sample, ]

#applying R-learner 
ps.score <- ps.score 
#train the model with the estimated ps.score from various method 
response.model <- SuperLearner(Y=Y, X=X,newX=newX,
                               SL.library = SL.bartMachine,
                               method=method.NNLS) 
response.hat <- response.model$SL.predict 


y_tilde = Y - response.hat
w_tilde = W - ps.score 
pseudo_outcome = y_tilde/w_tilde 

weights = w_tilde^2 


#applying T-learner 
#splitting into trt and crt
data.train.0 <- data.train[which(data.train$trt==0),]
data.train.1 <- data.train[which(data.train$trt==1),]


ps.score.0 <- data.train.0$ps
ps.score.1 <- data.train.1$ps 

#Train a regression model
#' seperate the data generation, 
#' create list of conditions to be on, put on coe server, a list of conditions there and then find the next to run
#' finish the code completely, run like 2 iterations of all conditions, calculate the time, and then to estimate the time for entire simulation, 
#' then decide into multiple computers or hipergator 
#' 


