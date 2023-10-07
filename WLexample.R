#Required Package
require(MASS)
require(lme4)
require(MatchIt)
require(Matching)
require(survey)
require(optmatch)
require(twang)
require(Rcpp)
require(magicfor)
require(data.table)

source("Test_function_to_simulate_CCREM_data.r") #function to simulate data
source("Test_function_to_simulate_CCREM_ps_estimation.r") #function to PS estimation
source("Test_function_to_simulate_CCREM_Matching&ATT.r") #function to get matched data and ATT
load(file="Population_CorCov Matrix.Rdata") #Covariance matrix of confounders

NumIter = 1000
simulation.start = proc.time() #store starting computer time.

for (level2n in c(30,50)){ #number of groups (30-30) or (50-50)
  for (rescorr_lev2 in c(0.0,0.4)){  #correlation between level 2 random effects (0.0 or 0.4)
    for (NS in c(20,40)) { # the mean of the group size
      for (IUCC_S2 in c(0.05,0.15)) { # IUCC S2 (0.05,0.15)
        for (IUCC_S4 in c(0.05,0.15)) { #IUCC S4 (0.05,0.15)
          for (P_R2 in c(1,2,3)) { #Pseudo R-square (level-1 and level-2) : (1: 0.05-0.15), (2: 0.10-0.10), (3:0.15-0.05)
            for (feeder in c(2,4)){ #feeder
              for (int in c(log(0.05/0.95))){ #conditional intercept (treated proportion)
                for (Method in c(1,2)) { #Ps methods (1: LR, 2: L-CCREM)
                  for (cov in c(1,2)) { #1: PS models with just leve 1 covariates, 2: ps models with both level 1 and level 2}
                    for(Matching in c(5)){
                      
                      
                      results.cond = NULL  #storage of results for each condition.
                      i = 1 #initiates the count of converged iterations
                      Iters.run = 0 #initiates the count of iterations run.
                      start.time = proc.time() #store starting computer time.
                      
                      
                      
                      #loop that only finishes when the desired number of converged iterations is obtained.
                      while (i <= NumIter) {
                        
                        data = CCREM.data(level2n,
                                          rescorr_lev2,
                                          NS,
                                          IUCC_S2,
                                          IUCC_S4,
                                          P_R2,
                                          feeder,
                                          int,
                                          
                                          NC1=3,  # Number of covariates in level- 1(fixed)
                                          NC21=3,  # Number of covariates in level -2 related to S2 school (fixed)
                                          NC22=3  # Number of covariates in level -2 related to S4 school (fixed)
                        )
                        
                        
                        #run propensity score estimation, matching and effect estimation.
                        data = try(CCREM.ps(data,Method,cov),silent=F)
                        if (class(data)[[1]]=="try-error") { #check for errors.
                          analysis = rep(-999,14)} else {
                            
                            #analyze the data
                            
                            analysis = try(CCREM.ATT(data,Matching),silent=F)
                            if (class(analysis)=="try-error") {analysis = rep(-888,14)
                            } else {
                              if (min(analysis)>(-111)) {i = i + 1}
                              
                              conditions = c(level2n,rescorr_lev2,NS,IUCC_S2,IUCC_S4,P_R2,feeder,
                                             int,Method,cov,Matching)
                              
                            } #close else.
                          }
                        
                        results.cond = rbind(results.cond,append(conditions,analysis))
                        
                        #count number of iterations so far.
                        Iters.run = Iters.run + 1
                        print(i)
                        
                      } #close while loop
                      
                      #save partial results.
                      write.table(results.cond,file="Results.csv",append=T,sep=",",
                                  col.names=F,row.names=F,quote=F)
                      
                      #give feedback.
                      end.time = proc.time()
                      condition.time = end.time - start.time
                      print(c(condition.time[3],condition.time[3]/Iters.run,level2n,rescorr_lev2,NS,IUCC_S2,IUCC_S4,P_R2,feeder,int,
                              Method,cov,Matching,Iters.run))
                      
                      
                    }#close if clause that checks whether the condition has been completed.
                  }
                }
              }    
            }      
          }        
        }
      }
    }
  }
}

simulation.end = proc.time()
total.time = simulation.end - simulation.start