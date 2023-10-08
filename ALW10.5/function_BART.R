  #splitting train and test into 70% train 30% test
#  sample <- sample(c(TRUE, FALSE), nrow(datatest), replace=TRUE, prob=c(0.5,0.5))
#  data.train  <- datatest[sample, ]
#  data.test   <- datatest[!sample, ]
  
  #split the dataset into training and test data
  
#  train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
#  train.sample = train.sample[order(train.sample)]
#  test.sample = 1:nrow(datatest)
#  test.sample = test.sample[-c(train.sample)]
  
  
#  data.train = datatest[train.sample,]
#  data.test = datatest[test.sample,]
#  data.train = as.data.frame(data.train)
#  data.test = as.data.frame(data.test)

#BART_result = BART_est(data.train, data.test)
#datatest

BART_est <- function(data.train, data.test){
  require(bartCause)
  #PS score estimation 
  ps_bart_train <- bartc(yij, treatment =trt,
                    confounders=x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id,
                    #parametric = (1|schoolid),
                    data = data.train,
                    keepTrees=TRUE,
                    method.rsp="p.weight", 
                    method.trt="bart", 
                    estimand="ate")
  score_bart_train <- ps_bart_train$p.score #keep p.score for train
  #summary(fit_bart_train)
  #ps_bart_train$p.score
  score_bart_test <- predict(ps_bart_train, newdata = data.test,type = "p.score")
  score_bart_test <- apply(score_bart_test,2,mean) #keep p.score for test
  
  #ATE_train = summary(fit_bart_train, target="pate")$estimate
  #CATE_train = fit_bart$mu.hat.obs[1:nrow(data.train)] - fit_bart$mu.hat.cf [1:nrow(data.train)]
  #iCATE_bart_train = extract(fit_bart_train, type = "icate")
  #iCATE_bart_train = apply(iCATE_bart_train, 2, mean)
  
  cate_bart_train <- bartc(yij, treatment =trt,
                         confounders=x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id,
                         #adding schoolid as the fixed effect, explain what's doing and how this works in a tree
                          #parametric = (1|schoolid),
                          data = data.train,
                          keepTrees=TRUE,
                          method.rsp="bart", 
                          method.trt="bart",
                         weights = score_bart_train,
                          estimand="ate")
  #unique(data.train$schoolid) 
  #class
  #score_bart_test <- predict(fit_bart_train, newdata=data.test,type="p.score")
  #score_bart_test <- apply(score_bart_test,2,mean) #keep p.score for test
  
  iCATE_bart_train = extract(cate_bart_train, type = "icate")
  iCATE_bart_train = apply(iCATE_bart_train, 2, mean)
  
  iCATE_bart_test = predict(cate_bart_train, newdata=data.test,type="icate")
  #length(iCATE_bart_test)
  iCATE_bart_test = apply(iCATE_bart_test, 2, mean)
  
  train.id <- data.train$Stu_id
  test.id <- data.test$Stu_id
  
  BART.train.ps <- cbind(train.id,score_bart_train)
  BART.train.CATE <- cbind(train.id, iCATE_bart_train)
  
  BART.test.ps <- cbind(test.id,score_bart_test)
  BART.test.CATE <- cbind(test.id,iCATE_bart_test)

  
  #save results
  BART_result <- list(BART.train.CATE,BART.test.CATE,BART.train.ps,BART.test.ps)
  
  return(BART_result)
}

#simulation.start = proc.time()
#BARTtest <- BART_est(data.train, data.test)
#simulation.edu=proc.time()
#total.time = simulation.edu-simulation.start
#user  system elapsed 
#17.806   0.438   6.681 

#BARTresult[[3]][1:5,]

#for (i in 1:4) {
#  print(BARTtest[[i]][1:5,])
  
#}


#BARTresult <- try(BART_est(data.test),silent = F)
#if(class(BARTresult)[[1]]=="try_error") {#is this for list? 
#  evalBART <- rep(-999,14) 
#}else{ evalBART <- Results_evaluation(BARTresult,datatest)}
#restul1 <- Results_evaluation(Slearnertest1,datatest)
#result2 <- Results_evaluation(BARTresult,datatest)
#analysis <- rbind(restul1,result2)  
#rep(conditions,2)
#results.cond<-append(results.cond,analysis)
#BARTresults.cond=NULL

#BARTresults.cond = rbind(BARTresults.cond,append(conditions,evalBART))
