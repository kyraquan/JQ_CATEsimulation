
googledrive::drive_auth()

gs4_find()
conditions.completed <- gs4_create(name="conditions.completed")
#open filt in browser 
gs4_browse(conditions.completed)
?gs4_find()
url <-c(" https://docs.google.com/spreadsheets/d/1KRh7AbdYh89EggmsyX7XslkpMQnHZTGmu1_ImOWu-H0/edit?usp=sharing")
read <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1KRh7AbdYh89EggmsyX7XslkpMQnHZTGmu1_ImOWu-H0")

read %>% 
  sheet_append(c(60,10,0.1,1,1,1,0.47),sheet='Sheet1')
sheet_write(c(60,10,0.1,1,1,1,0.47))
conditions <- matrix(data=c(60,10,0.1,1,1,1,0.47),nrow = 1)
sheet_delete(ss="https://docs.google.com/spreadsheets/d/1KRh7AbdYh89EggmsyX7XslkpMQnHZTGmu1_ImOWu-H0",
             "test_sheet")


conditions <- as.data.frame(conditions)
names(conditions) <- c("level2n","level1n","ICC","PS_model","treatment_model","Outcome_model","tau_var")
gs4_create("test_sheet",sheets=list(data=conditions))
test_sheet <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1jR9Ok2fG0c7weZ5t7_P-ztNlbpLPhkThdqTXg9Cdzl0")
conditions2 <- as.data.frame(matrix(data=c(60,10,0.1,1,1,1,0.1),nrow = 1))
sheet_append(conditions2,test_sheet,sheet=1)
?sheet_append

# see 5 Sheets, prioritized by creation time

#set up the google sheet 
conditions <- as.data.frame(matrix(data=rep(-0.99,7),nrow = 1)) 
names(conditions) <- c("level2n","level1n","ICC","PS_model","treatment_model","Outcome_model","tau_var")
gs4_create("conditions.working",sheets=list(data=conditions))
ss="https://docs.google.com/spreadsheets/d/1CM8B8itM66PhS6aUf0SmKvNWvFSUrpgGNTDOs3DsQPE"



#find ss
#x <- gs4_find(order_by = "createdTime desc", n_max = 5)
# ss = x$id[1]
#library(googlesheets4)
#library(googledrive)
#library(tidyverse)
#clean a certain range if needed 
#range_clear(ss=ss,range = "A3:G8",reformat = FALSE)

for (level2n in c(60,200)) { #number of groups 
  for (level1n in c(10,30)) { #number of individuals 
    for (ICC in c(0.1,0.3)) { #ICC, 0.1, 0.3 
      for (PS_model in c(1,2,3)) {#propensity score model
        for (treatment_model in c(1,2,3)) { #CATE model 
          for (Outcome_model in c(1,2,3)) { #Outcome model 
            #for (Proportion in c(2.197225,0.8472979)) { #proportion treated, removed, only keeping log(0.3/0.7)
            for (tau_var in c(0.1,0.47)) {
              #--------------------------------------------------------------------
              #read conditions that have already been claimed
              #ss="https://docs.google.com/spreadsheets/d/1jR9Ok2fG0c7weZ5t7_P-ztNlbpLPhkThdqTXg9Cdzl0"
              conditions.completed = read_sheet(ss=ss)
              current.condition = matrix(rep(c(level2n,level1n,ICC,PS_model,treatment_model,Outcome_model,tau_var),
                                             each=nrow(conditions.completed)),nrow(conditions.completed),7)
              if (max(apply((current.condition==conditions.completed),1,min))==0) {
                
                #save to the server the description of the conditions picked
                #so another computer cannot pick the same.
                sheet_append(ss=ss, as.data.frame(matrix(current.condition[1,],1,)),sheet=1)}#close the if loop for condition
            }#close for tau_var
          }#closte for outcome model 
        }#close for treatment model
      }#close for ps model
    }#close for icc 
  }#close for level1n 
}#close for level2n

range_clear(ss=ss,sheet=1,range=NULL)
              
              
              
              
              
              
              
              