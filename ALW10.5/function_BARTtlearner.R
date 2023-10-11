BART_Tlearner <- function(data.train,data.test,datatest) { 

  #train the model with data train 
  m1_mod <- bartc(response = yij,
                         treatment = trt,
                         confounders = x1ij+x2ij+v1rep+v2rep+v3rep+V4+V5+X3+X4+School_id,
                         data = data.train,
                         keepTrees=TRUE,
                         method.trt = "bart",
                         method.rsp = "bart",
                         estimand = "ate" ) 
  
  train.m1 <- fitted(m1_mod, type = "y.1")
  train.m0 <- fitted(m1_mod, type = "y.0")
  
  
  
  test.m1 <- predict(m1_mod,newdata = data.test,type = "y.1")
  test.m1 <- apply( test.m1, 2, mean) #keep test ps 
  test.m0 <- predict(m1_mod,newdata = data.test,type = "y.0")
  test.m0 <- apply( test.m0, 2, mean) #keep test ps 
  
  score_T <- matrix(0,nrow(datatest),1)
  
  #estimate CATE of test
  tlearner.test.cate <- test.m1 - test.m0
  score_T[,1][data.test$Stu_id] <- tlearner.test.cate
  
  
  #estimate CATE of train 
  tlearner.train.cate <- train.m1 - train.m0
  score_T[,1][data.train$Stu_id] <- tlearner.train.cate

  tlearner.train.cate <- cbind(data.train$Stu_id,tlearner.train.cate)
  tlearner.test.cate <- cbind(data.test$Stu_id,tlearner.test.cate)

  
  tlearner.cate.list <- list(tlearner.train.cate,tlearner.test.cate,score_T) 
  
  return(tlearner.cate.list)
}

#bartttet <-BART_Tlearner(data.train, data.test, datatest)  
  
  
  
  