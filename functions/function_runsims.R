createbootstrappedData <- function(df_boot) {
  
  smpl_0 <- sample((1:nrow(df_boot))[df_boot$trt == 0],
                   replace = TRUE,
                   size = sum(1 - df_boot$d))
  smpl_1 <- sample((1:nrow(df_boot))[df_boot$trt == 1],
                   replace = TRUE,
                   size = sum(df_boot$d))
  smpl <- sample(c(smpl_0, smpl_1))
  
  return(df_boot[smpl,])
}

data_aux <- createbootstrappedData(data.train)



#splitting train and test into 70% train 30% test
sample <- sample(c(TRUE, FALSE), nrow(datatest), replace=TRUE, prob=c(0.5,0.5))
data.train  <- datatest[sample, ]
data.test   <- datatest[!sample, ]

#split the dataset into training and test data

train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(datatest)
test.sample = test.sample[-c(train.sample)]


data.train = datatest[train.sample,]
data.test = datatest[test.sample,]
data.train = as.data.frame(data.train)
data.test = as.data.frame(data.test)

covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")


Slearnertest1 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
#Slearnertest2 <- SLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
#Slearnertest3 <- SLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))

Tlearnertest1 <- TLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
#Tlearnertest2 <- TLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
#Tlearnertest3 <- TLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))

Xlearnertest1 <- XLearner_est(data.train, data.test,datatest,covariates,learner = "SL.cforest")
Xlearnertest2 <- XLearner_est(data.train, data.test,datatest,covariates,learner = "SL.bartMachine")
Xlearnertest3 <- XLearner_est(data.train, data.test,datatest,covariates,learner = c("SL.bartMachine", "SL.cforest"))


## Print to file ##
write.csv(Xlearnertest1, paste0("Results D", primary, "Psi", Psi, ".csv"))

# Display in console
print(results.summary)

NumIter=2 #ideal 100
simulation.start = proc.time()

#committee to read the articles get dissertation to committee on 20th. 2 weeks and a half. 
2*2*2*27*2
2*2*2*3*3*1*3*2
log(0.3/0.7)
log(0.1/0.9)
#start research on hypergator - ready to start 
#' make computers in rem lab work together 
#' a list to check and have multiple computer to check in REM lab 
#' send 5 conditions to hipergator, 5 conditions to REM lab
#' bench marking of 2 iterations for slowest conditions 
#' 1 iteration of all conditions for time in both locations 
#' go by anova use eta-square, create tables 
#' eg, et-square for group size is 0.0001, rhen no need table 
#' if icc*level1n is 0.2, create a table by sample size and icc 
#' one thing higher level interactions replace lower level
#' the two-way interaction between icc*level2n is 0.22
#' the three-way interaction is 0.13, then no need for the two-way 
#' everytime if two-way contained in three-way, then just need only three-way tables of 
#' if none interactions is bigger than 0.1, then just use 
#' combinations of two-may,three-way, then interpret from there 
#' anova is telling us how to create the tables
#' situation where nothing mattered, decided to be linear to make the effect size become 0.05
#' if dont do effect size then 
#' eta squared, or generalized eta square, interpret only results bigger than the 


#edited 10.5
for (level2n in c(60,200)) { #number of groups --cut to 2
  for (level1n in c(10,30)) { #number of individuals -- cut to 2
    for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
      for (PSmodel in c(1,2,3)) {#propensity score model
        for (CATEmodel in c(1,2,3)) { #CATE model 
          for (Outcomemodel in c(1,2,3)) { #Outcome model 
            for (Proportion in c(0.8472979)) { #proportion treated - keep just log(0.3/0.7) -- delete in the loop 
              for (tau_var in c(0.1,0.47)) {
              
              #results.cond = NULL #storage of results for each condition 
              i=1 #initiates the count of converged iterations 
              Iters.run = 0 #initiates the count of iterations run.
              start.time = proc.time() #store starting computer time 
              
              BARTresults.cond=NULL
              CFresults.cond=NULL
              SL1results.cond=NULL
              SL2results.cond=NULL
              SL3results.cond=NULL
              TL1results.cond=NULL
              TL2results.cond=NULL
              TL3results.cond=NULL
              XL1results.cond=NULL
              XL2results.cond=NULL
              XL3results.cond=NULL
              
              
              while(i <= NumIter) { 
                
                datatest = PS_model_data(n_cluster=level2n, n_ind=level1n,ICC=ICC,int=Proportion,tau_var=tau_var)
                #split the dataset into training and test data
                train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
                train.sample = train.sample[order(train.sample)]
                test.sample = 1:nrow(datatest)
                test.sample = test.sample[-c(train.sample)]
                
                data.train = datatest[train.sample,]
                data.test = datatest[test.sample,]
                data.train = as.data.frame(data.train)
                data.test = as.data.frame(data.test)
                
                covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")
                conditions <- c(level2n,level1n,ICC, PSmodel,CATEmodel,Outcomemodel,Proportion,tau_var)
                
                #run each method 

                BARTresult <- try(BART_est(data.train,data.test),silent = F)
                if(class(BARTresult)[[1]]=="try_error") {#is this for list? 
                  evalBART <- rep(-999,14) 
                  evalBART <- c("BART",conditions,evalBART)
                }else{ evalBART <- Results_evaluation(BARTresult,datatest)
                evalBART <- c("BART",conditions,evalBART)}
                BARTresults.cond = rbind(BARTresults.cond,append(conditions,evalBART))
                
                CFresult <- try(CF_est(data.train,data.test,covariates),silent = F)
                if(class(CFresult)[[1]] =="try_error"){
                  evalCF <-rep(-999,14) 
                  evalCF <- c("CF",conditions,evalCF)
                }else{ evalCF <- Results_evaluation(CFresult,datatest)
                evalCF <- c("CF",conditions,evalCF)} 
                CFresults.cond=rbind(CFresults.cond,append(conditions,evalCF))
                
                
                SLearnerresult1 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(SLearnerresult1)[[1]] =="try_error") {
                  evalSL1 <-rep(-999,14)
                  evalSL1 <- c("SL1",conditions,evalSL1)
                }else{evalSL1 <- Results_evaluation(SLearnerresult1,datatest)
                evalSL1 <- c("SL1",conditions,evalSL1)}
                SL1results.cond=rbind(SL1results.cond,append(conditions,evalSL1))

                
                SLearnerresult2 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(SLearnerresult1=2)[[1]] =="try_error") {
                  evalSL2 <-rep(-999,14)
                  evalSL2 <- c("SL2",conditions,evalSL2)
                }else{ evalSL2 <- Results_evaluation(SLearnerresult1,datatest)
                evalSL2 <- c("SL2",conditions,evalSL2)}
                SL2results.cond=rbind(SL2results.cond,append(conditions,evalSL2))

                                       
                SLearnerresult3 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(SLearnerresult3)[[1]] =="try_error") {
                  evalSL3 <-rep(-999,14)
                  evalSL3 <- c("SL3",conditions,evalSL3)
                }else{evalSL3 <- Results_evaluation(SLearnerresult3,datatest)
                evalSL3 <- c("SL3",conditions,evalSL3) }
                SL3results.cond=rbind(SL3results.cond,append(conditions,evalSL3))
             
                
                TLearnerresult1 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(TLearnerresult1)[[1]] =="try_error") {
                  evalTL1 <-rep(-999,14)
                  evalTL1 <- c("TL1",conditions,evalTL1) 
                }else{evalTL1 <- Results_evaluation(TLearnerresult1,datatest)
                evalTL1 <- c("TL1",conditions,evalTL1) }
                TL1results.cond=rbind(TL1results.cond,append(conditions,evalTL1))
            
                
                TLearnerresult2 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(TLearnerresult2)[[1]] =="try_error") {
                  evalTL2 <-rep(-999,14)
                  evalTL2 <- c("TL2",conditions,evalTL2)
                }else{evalTL2 <- Results_evaluation(TLearnerresult2,datatest)
                evalTL2 <- c("TL2",conditions,evalTL2) }
                TL2results.cond=rbind(TL2results.cond,append(conditions,evalTL2))
              
                
                TLearnerresult3 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(TLearnerresult3)[[1]] =="try_error") {
                  evalTL3 <-rep(-999,14)
                  evalTL3 <- c("TL3",conditions,evalTL3) 
                }else{evalTL3 <- Results_evaluation(TLearnerresult3,datatest)
                evalTL3 <- c("TL3",conditions,evalTL3) }
                TL3results.cond=rbind(TL3results.cond,append(conditions,evalTL3))
           
                
                XLearnerresult1 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(XLearnerresult1)[[1]] =="try_error") {  
                  evalXL1 <-rep(-999,14)
                  evalXL1 <- c("XL1",conditions,evalXL1)
                }else{evalXL1 <- Results_evaluation(XLearnerresult1,datatest)
                evalXL1 <- c("XL1",conditions,evalXL1) }
                XL1results.cond=rbind(XL1results.cond,append(conditions,evalXL1))
      
                
                XLearnerresult2 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(XLearnerresult2)[[1]] =="try_error") {
                  evalXL2 <-rep(-999,14)
                  evalXL2 <- c("XL2",conditions,evalXL2) 
                }else{evalXL2 <- Results_evaluation(XLearnerresult2,datatest)
                evalXL2 <- c("XL2",conditions,evalXL2) }
                XL2results.cond=rbind(XL2results.cond,append(conditions,evalXL2))

                
                XLearnerresult3 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(XLearnerresult3)[[1]] =="try_error") {
                  evalXL3 <-rep(-999,14)
                  evalXL3 <- c("XL1",evalXL3) 
                }else{evalXL3 <- Results_evaluation(XLearnerresult3,datatest)
                evalXL3 <- c("XL1",evalXL3) }
                XL3results.cond=rbind(XL3results.cond,append(conditions,evalXL3))
                
                
                #results.cond = rbind(results.cond,append(conditions,analysis))
                
                #count number of iterations so far.
                Iters.run = Iters.run + 1
                print(i)}
              
              #save partial results.
              write.table(BARTresults.cond,file="BARTresults.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(CFresults.cond,file="CFresults.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(SL1results.cond,file="SL1results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(SL2results.cond,file="SL2results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(SL3results.cond,file="SL3results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(TL1results.cond,file="TL1results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(TL2results.cond,file="TL2results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(TL3results.cond,file="TL3results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(XL1results.cond,file="XL1results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(XL2results.cond,file="XL2results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              write.table(XL3results.cond,file="XL3results.csv",append=T,sep=",",
                          col.names=F,row.names=F,quote=F)
              #give feedback.
              end.time = proc.time()
              condition.time = end.time - start.time
              print(c(condition.time[3],condition.time[3]/Iters.run,
                      level2n,level1n,ICC,Itau_va,PSmodel,CATEmodel,Outcomemodel,Proportion,
                      Iters.run))

                
              }
              
              
              
              
              #run machinelearning methods
              
              #put no column names or row names 
              # write table
        
              #write.csv(resultslist,paste0("Results", level2n,"-",level1n,"-",ICC,"-",tau_var,"-",PSmodel,"-",CATEmodel,"-",Outcomemodel,"-",Proportion)) #make sure  append=TRUE
            }}}}}}}

simulation.end = proc.time()
total.time = simulation.end - simulation.start



cross.entropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    x <- x + (p[i] * log(phat[i]))
  }
  return(-x)
}

cross.entropy(train.ps.true,train.ps[,2]) #of ps score 
-sum(trt.true*log(train.ps[,2]))+(1-trt.true)*log(1-train.ps[,2])
cross.entropy(test.ps.true,test.ps[,2]) 
cross.entropy(c(0,1,0,1,1,1,1),
              c(1,1,1,0,0,0,0)) #cannot calculate that when there's no trt assigned. 

trt.true=ifelse(train.ps.true>=0.5,1,0)
trt.est = ifelse(train.ps[,2]>=0.5,1,0)

trt.est =ifelse(datalist[[4]][,2]>=0.5,1,0)

BART.train.cate <- as.data.frame(BARTtest[[1]])
mean((datatest[BART.train.cate[,1],"tau"] - BART.train.cate[,2])^2)
datatest$tau
-sum(train.ps.true*log2(train.ps[,2]))
-sum(c(0.1,0.4,0.5)*log2(c(0.8,0.15,0.05)))
-sum(trt.true*log(train.ps))
-sum(trt.true*log(trt.est))
library(psych)
install.packages("psych")
kappa <- cohen.kappa(x=cbind(trt.true,trt.est))
kappa$weighted.kappa
cohen.kappa(x=cbind(c(1,1,1,0,0),c(1,1,1,0,1)))
rater1 = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0)
rater2 = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0)
cohen.kappa(x=cbind(rater1,rater2))