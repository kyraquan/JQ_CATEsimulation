
#datalist is the estimates,datatest is the true values
Results_evaluation <- function(datalist,datatest) {
  if(length(datalist)==1){
    results <- rep(-999,14)
    
  }else{

  #datalist <- BARTresult
  
  #length(datalist)
  #require(psych)
  train.cate <- as.data.frame(datalist[[1]])
  test.cate <- as.data.frame(datalist[[2]])

  
  train.cate.true <- datatest[train.cate[,1],"tau"]
  test.cate.true <- datatest[test.cate[,1],"tau"]
  train.ps.true <- datatest[train.cate[,1],"ps"]
  test.ps.true <- datatest[test.cate[,1],"ps"]
  
  #Evaluate the CATEs
  cate.PEHE_train <- mean((train.cate.true - train.cate[,2])^2)
  cate.PEHE_test <- mean((test.cate.true - test.cate[,2])^2)
  
  cate.RBias_test <- mean((test.cate.true - test.cate[,2])/test.cate[,2]) 
  cate.RBias_train <- mean((train.cate.true - train.cate[,2])/train.cate[,2]) #newly updated the name
  
  #catevar.RBias_train <- (var(train.cate[,2])-var(train.cate.true))/var(train.cate.true) #bias in the variance of cate 
  #catevar.RBias_test <- (var(test.cate[,2])-var(test.cate.true))/var(test.cate.true)
  
  if (var(train.cate.true) == 0){
    catevar.Rbias_train <- NA
    catevar.Rbias_test <- NA
  }else{
    catevar.Rbias_train <- (var(train.cate[,2])-var(train.cate.true))/var(train.cate.true)# Relative bias in the variance of cate 
    catevar.Rbias_test <- (var(test.cate[,2])-var(test.cate.true))/var(test.cate.true)
    }
  
 
  #Evaluate the ATE
  
  #ate.Bias_train <- (train.ate-mean(train.cate.true)) #bias = mean estimation and population parameter -- correct the estimate
  #  no need for bias 
  #ate.RB_train <- (train.ate-mean(train.cate.true))/(mean(train.cate.true)) 
  #relative bias = bias/population parameter -- correct the estimate
  #interpreting criteria https://ufl.instructure.com/courses/414257/files/51997723?module_item_id=8158074
  train.ate <- mean(train.cate[,2])             
  test.ate <- mean(test.cate[,2])
  test.ate.true <- mean(train.cate.true) ###- switched the name, should be test, make update later
  train.ate.true<- mean(test.cate.true) ###- switched the name, should be train, make update later
  
  
  #ate.Bias_test <- (test.ate-mean(test.cate.true))
  #ate.RB_test <- (test.ate-mean(test.cate.true))/(mean(test.cate.true))
  
  if(length(datalist) == 4) {
    train.ps <- as.data.frame(datalist[[3]]) 
    test.ps <- as.data.frame(datalist[[4]]) 
    
    #Evaluate the PS
    #the MSE is used to measure efficiency according to the article
    ps.MSE_train <- mean((train.ps.true - train.ps[,2])^2) #RMSE is combine for relative bias and variance, better 
    ps.MSE_test <- mean((test.ps.true - test.ps[,2])^2)
    if(var(train.ps.true) == 0) { 
      ps.corr_train <- -888
      ps.corr_test <- -888
      } else{
    ps.corr_train <- cor(train.ps.true,train.ps[,2])
    ps.corr_test <- cor(test.ps.true,test.ps[,2])}
    #look for efficiency, maybe do relative bias and standard deviation 
    
    #don't need the kappa 
    #train.trt.true=ifelse(train.ps.true>=0.5,1,0)
    #train.trt.est = ifelse(train.ps[,2]>=0.5,1,0)
    #test.trt.true=ifelse(test.ps.true>=0.5,1,0)
    #test.trt.est = ifelse(test.ps[,2]>=0.5,1,0)
    #kappa <- cohen.kappa(x=cbind(train.trt.true,train.trt.est))
    #ps.Kappa_train <- kappa$kappa
    #kappa <- cohen.kappa(x=cbind(test.trt.true,test.trt.est))
    #ps.Kappa_test <- kappa$kappa
    
  } else{
    ps.corr_train <- NA
    ps.corr_test <- NA
    #ps.Kappa_test <- NA
    #ps.Kappa_train <- NA
    ps.MSE_train <-NA 
    ps.MSE_test <- NA
    
  }
  
  results<- c(cate.PEHE_train,cate.RBias_train,catevar.Rbias_train,
              cate.PEHE_test,cate.RBias_test,catevar.Rbias_test,
              ps.MSE_train,ps.corr_train,ps.MSE_test,ps.corr_test,
              train.ate,test.ate,train.ate.true,test.ate.true)
  
  #length(results)
  #names(results) <- c("cate.PEHE_train","cate.RBias_train","catevar.Rbias_train",
  #                    "cate.PEHE_test","cate.RBias_train","catevar.Rbias_test",
  #                    "ps.MSE_train","ps.corr_train","ps.MSE_test","ps.corr_test",
  #                    "train.ate","test.ate","train.ate.true","test.ate.true")
  }
  
  return(results)
  
  
  
}
#resulttest <- Results_evaluation(datalist=BARTresult,datatest)
#Results_evaluation(datalist=c(1),datatest)
#round(resulttest,4)

#c(cate.PEHE_train,cate.Bias_train,catevar.Bias_train,catevar.Rbias_train,
#cate.PEHE_test,cate.Bias_test,catevar.Bias_train,catevar.Rbias_test,
#ps.MSE_train,ps.Kappa_train,ps.MSE_test,ps.Kappa_test,
#ate.Bias_train,ate.RB_train,ate.Bias_test,ate.RB_test)


