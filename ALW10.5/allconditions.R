#edited 10.5
#datatest = PS_model_data(n_cluster=level2n, n_ind=level1n,ICC=ICC,int=Proportion,tau_var=tau_var)

combinations_df <- data.frame(level2n=integer(),
                              level1n=integer(),
                              ICC=numeric(),
                              PS_model=integer(),
                              treatment_model=integer(),
                              Outcome_model=integer(),
                              tau_var=numeric(),
                              stringsAsFactors = FALSE)
#conditions
for (level2n in c(60,200)) { #number of groups --cut to 2
  for (level1n in c(10,30)) { #number of individuals -- cut to 2
    for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
      for (PS_model in c(1,2,3)) {#propensity score model
        for (treatment_model in c(1,2,3)) { #CATE model 
          for (Outcome_model in c(1,2,3)) { #Outcome model 
            #for (Proportion in c(0.8472979)) { #proportion treated - keep just log(0.3/0.7) -- delete in the loop 
              for (tau_var in c(0.1,0.47)) {
                
                combination <- data.frame(level2n, level1n,ICC,PS_model,treatment_model,Outcome_model,tau_var)
                combinations_df <- rbind(combinations_df,combination)
              }
          }
        }
      }
    }
  }
}
colnames(combinations_df)
nrow(combinations_df)
write.csv(combinations_df,file="ALLCONDITIONS.csv",row.names = FALSE,col.names = TRUE)
