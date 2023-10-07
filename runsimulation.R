#running simulation 


setwd("~/Destop/Dissertation/simulation/functions")
files.sources = list.files()
sapply(files.sources, source)

NumIter = 100 
simulation.start = proc.time() #store starting computer time 
for (level2n in c(60,200,500)) { #number of groups 
  for (level1n in c(10,30,60)) { #number of individuals 
    for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
      for (PSmodel in c(1,2,3)) {#propensity score model
        for (CATEmodel in c(1,2,3)) { #CATE model 
          for (Outcomemodel in c(1,2,3)) { #Outcome model 
            for (Proportion in c(0.2,0.5)) { #proportion treated 
              
              
              results.cond = NULL #storage of results for each condition 
              i = 1 #initiates the count of converged iterations 
              Iters.run = 0 #initiates the count of iterations run 
              start.time = proc.time() #store starting computer time 
              
              #loop that only finishes when the desired number of converged iterations is obtained 
              while (i <= NumIter) {
                data = #datagenfunction
                
              }
              #run propensity score estimations and CATE estimations 
              data= try(function_BART(), silent = F)
              if(class(data)[[1]]=="try-error"){
                analysis = rep(-999,14)} else {
                  #analyze the data 
                  analysis = try(xxx(data,function), silent=F)
                  if(class(analysis) =="try-error") {
                    
                  }
                }
              }
              
              
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  for (variable in vector) {
    
  }
  
}