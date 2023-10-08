
BARTresults <- "BARTresults.csv"
CFresults <- "CFresults.csv"
SL1results <- "SL1results.csv"
SL2results <- "SL2results.csv"
SL3results <- "SL3results.csv"
TL1results <- "TL1results.csv"
TL2results <- "TL2results.csv"
TL3results <- "TL3results.csv"
XL1results <- "XL1results.csv"
XL2results <- "XL2results.csv"
XL3results <- "XL3results.csv"

# Assuming this file doesn't exist yet or you want to overwrite any previous content.
#if (file.exists(file_name)) {
#  file.remove(file_name)
#}



headers<-c( "METHOD",
            "level2n","level1n,ICC", "PSmodel","CATEmodel","Outcomemodel","Proportion","tau_var",
           "cate.PEHE_train","cate.RBias_train","catevar.Rbias_train",
           "cate.PEHE_test","cate.RBias_train","catevar.Rbias_test",
           "ps.MSE_train","ps.corr_train","ps.MSE_test","ps.corr_test",
             "train.ate","test.ate","train.ate.true","test.ate.true")
write.table(t(headers), file = BARTresults, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = CFresults, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = SL1results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = SL2results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = SL3results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = TL2results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = TL3results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = XL2results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(t(headers), file = XL3results, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)



NumIter=2 #ideal 1000 
simulation.start = proc.time()

#committee to read the articles get dissertation to committee on 20th. 2 weeks and a half. 
covariates <- c("x1ij","x2ij","v1rep","v2rep","v3rep","V4","V5","X3","X4")
for (level2n in c(60,200,500)) { #number of groups 
  for (level1n in c(10,30,60)) { #number of individuals 
    for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
      for (PS_model in c(1,2,3)) {#propensity score model
        for (treatment_model in c(1,2,3)) { #CATE model 
          for (Outcome_model in c(1,2,3)) { #Outcome model 
            for (Proportion in c(2.197225,0.8472979)) { #proportion treated 
              for (tau_var in c(0.1,0.47)) {
                
                
                results.cond = NULL  #storage of results for each condition.
                i = 1 #initiates the count of converged iterations
                Iters.run = 0 #initiates the count of iterations run.
                start.time = proc.time() #store starting computer time.
              
              
              while(i <= NumIter) { 
                
                datatest = PS_model_data(n_cluster=level2n, n_ind=level1n,ICC=ICC,int=Proportion,tau_var=tau_var,
                                         ps_model = PS_model,treatment_model = treatment_model,outcome_model = Outcome_model)
                #split the dataset into training and test data
                train.sample = sample(1:nrow(datatest), nrow(datatest)/2)
                train.sample = train.sample[order(train.sample)]
                test.sample = 1:nrow(datatest)
                test.sample = test.sample[-c(train.sample)]
                
                data.train = datatest[train.sample,]
                data.test = datatest[test.sample,]
                data.train = as.data.frame(data.train)
                data.test = as.data.frame(data.test)
              
                conditions <- c(level2n,level1n,ICC, PSmodel,CATEmodel,Outcomemodel,Proportion,tau_var)
                
                #run each method 

                BARTresult <- try(BART_est(data.train,data.test),silent = F)
                if(class(BARTresult)[[1]]=="try_error") {#is this for list? 
                  evalBART <- rep(-999,14) 
                  evalBART <- c("BART",conditions,evalBART)
                }else{ evalBART <- Results_evaluation(BARTresult,datatest)
                evalBART <- c("BART",conditions,evalBART)}
                
                write.table(t(evalBART), file = BARTresults, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #BARTresults.cond = rbind(BARTresults.cond,append(c(evalBART)))
                print("BART finished")
                
                CFresult <- try(CF_est(data.train,data.test,covariates),silent = F)
                if(class(CFresult)[[1]] =="try_error"){
                  evalCF <-rep(-999,14) 
                  evalCF <- c("CF",conditions,evalCF)
                }else{ evalCF <- Results_evaluation(CFresult,datatest)
                evalCF <- c("CF",conditions,evalCF)} 
                
                write.table(t(evalCF), file = CFresults, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                
                print("CF finished")
                #CFresults.cond=rbind(CFresults.cond,append(conditions,evalCF))
                
                
                SLearnerresult1 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(SLearnerresult1)[[1]] =="try_error") {
                  evalSL1 <-rep(-999,14)
                  evalSL1 <- c("SL1",conditions,evalSL1)
                }else{evalSL1 <-Results_evaluation(SLearnerresult1,datatest)
                evalSL1 <- c("SL1",conditions,evalSL1)}
                write.table(t(evalSL1), file = SL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #SL1results.cond=rbind(SL1results.cond,append(conditions,evalSL1))

                
                SLearnerresult2 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(SLearnerresult2)[[1]] =="try_error") {
                  evalSL2 <-rep(-999,14)
                  evalSL2 <- c("SL2",conditions,evalSL2)
                }else{ evalSL2 <- Results_evaluation(SLearnerresult2,datatest)
                evalSL2 <- c("SL2",conditions,evalSL2)}
                
                write.table(t(evalSL2), file = SL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                
                #SL2results.cond=rbind(SL2results.cond,append(conditions,evalSL2))

                                       
                SLearnerresult3 <- try(SLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(SLearnerresult3)[[1]] =="try_error") {
                  evalSL3 <-rep(-999,14)
                  evalSL3 <- c("SL3",conditions,evalSL3)
                }else{evalSL3 <- Results_evaluation(SLearnerresult3,datatest)
                evalSL3 <- c("SL3",conditions,evalSL3) }
                write.table(t(evalSL3), file = SL3results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                
                print("SL finished")
                #SL3results.cond=rbind(SL3results.cond,append(conditions,evalSL3))
             
                
                TLearnerresult1 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(TLearnerresult1)[[1]] =="try_error") {
                  evalTL1 <-rep(-999,14)
                  evalTL1 <- c("TL1",conditions,evalTL1) 
                }else{evalTL1 <- Results_evaluation(TLearnerresult1,datatest)
                evalTL1 <- c("TL1",conditions,evalTL1) }
                write.table(t(evalTL1), file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #TL1results.cond=rbind(TL1results.cond,append(conditions,evalTL1))
                
            
                
                TLearnerresult2 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(TLearnerresult2)[[1]] =="try_error") {
                  evalTL2 <-rep(-999,14)
                  evalTL2 <- c("TL2",conditions,evalTL2)
                }else{evalTL2 <- Results_evaluation(TLearnerresult2,datatest)
                evalTL2 <- c("TL2",conditions,evalTL2) }
                write.table(t(evalTL2), file = TL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #TL2results.cond=rbind(TL2results.cond,append(conditions,evalTL2))
              
                
                TLearnerresult3 <- try(TLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(TLearnerresult3)[[1]] =="try_error") {
                  evalTL3 <-rep(-999,14)
                  evalTL3 <- c("TL3",conditions,evalTL3) 
                }else{evalTL3 <- Results_evaluation(TLearnerresult3,datatest)
                evalTL3 <- c("TL3",conditions,evalTL3) }
                write.table(t(evalTL3), file = TL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #TL3results.cond=rbind(TL3results.cond,append(conditions,evalTL3))
                print("TL finished")
           
                
                XLearnerresult1 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.cforest"),silent = F)
                if(class(XLearnerresult1)[[1]] =="try_error") {  
                  evalXL1 <-rep(-999,14)
                  evalXL1 <- c("XL1",conditions,evalXL1)
                }else{evalXL1 <- Results_evaluation(XLearnerresult1,datatest)
                evalXL1 <- c("XL1",conditions,evalXL1) }
                write.table(t(evalXL1), file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #XL1results.cond=rbind(XL1results.cond,append(conditions,evalXL1))
      
                
                XLearnerresult2 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner="SL.bartMachine"),silent = F)
                if(class(XLearnerresult2)[[1]] =="try_error") {
                  evalXL2 <-rep(-999,14)
                  evalXL2 <- c("XL2",conditions,evalXL2) 
                }else{evalXL2 <- Results_evaluation(XLearnerresult2,datatest)
                evalXL2 <- c("XL2",conditions,evalXL2) }
                write.table(t(evalXL2), file = XL2results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #XL2results.cond=rbind(XL2results.cond,append(conditions,evalXL2))

                
                XLearnerresult3 <- try(XLearner_est(data.test,data.train,datatest,covariates,learner=c("SL.cforest","SL.bartMachine")),silent = F)
                if(class(XLearnerresult3)[[1]] =="try_error") {
                  evalXL3 <-rep(-999,14)
                  evalXL3 <- c("XL3",conditions,evalXL3) 
                }else{evalXL3 <- Results_evaluation(XLearnerresult3,datatest)
                evalXL3 <- c("XL3",conditions,evalXL3) }
                write.table(t(evalXL1), file = XL1results, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
                #XL3results.cond=rbind(XL3results.cond,append(conditions,evalXL3))
                print("XL finished")
                
                #results.cond = rbind(results.cond,append(conditions,analysis))
                
                #count number of iterations so far.
                #count number of iterations so far.
                Iters.run = Iters.run + 1
                print(i)
               } #close the while loop
              
              end.time = proc.time()
              condition.time = end.time - start.time
              print(c(condition.time[3],condition.time[3]/i,
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


