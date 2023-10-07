#code to run the simulation
#setwd("D:/walter/research/Treatment_effects/Comparison_of_propensity_score_methods_for_multiple_treatment_doses/R_code")
source("function_to_simulate_data2.R")
source("function_to_analyze_data.R")

#define population parameters
gamma = rep(0.1,10) #gamma = effects of covariates on outcome
pred.cov = diag(10) #pred.cov = covariance matrix of predictors
#coef.treat = matrix of effects of predictors on multiple treatments (except the baseline treatment)
#where each row is a treatment, the first column is of interecepts and the other columns are the 
#effects of each covariate.
#first define which effects would give a pseudo-R2 of 0.25
#assuming all covariates have the same effect on treatment effect and are all uncorrelated
#also assuming that the variances of the covariates is 1
treat.eff0 = c(0,0,0) #no effect for the calculation of Type I error rates
treat.eff1=c(0.1,0.2,0.3) #treatment effects as diferences from baseline category                   
treat.eff2 = c(0.4,0.5,0.6)
#define models
outcome.model = formula("y~treat")
selection.model = paste("x",1:10,collapse="+",sep="")
selection.model = paste("treat~", selection.model,sep="")

n.cov = 10 #number of covariates
iterations = 1000
simulation.start = proc.time() #store starting computer time.
#start loop through conditions
for (r.squared in  c(0.1, 0.25, 0.4)) { #proportion of the variance of the outcome explained by predictors
for (effect in list(treat.eff0,treat.eff1,treat.eff2)) {
for (prop.b in c(0.25, 0.55, 0.75) ) { #vary proportion in the baseline
for (pseudo.r2 in c(0.1, 0.25, 0.4)) {
for (sample.size in c(200,400,800,1600)) { 

#--------------------------------------------------------------------
#read conditions that have already been claimed
conditions.completed = read.csv("simulation_control.csv")
#'start from a empty with all column = 0 
#'the first computer pick the codition based on the csv 

#conditions.completed = as.data.frame(t(c(0.1,0.5,0.25,0.1,200)))
#nrow(conditions.completed)
#conditions.completed[2,]=c(0.25,0.5,0.25,0.1,200)

#current.condition = matrix(rep(c(0.25,0.5,0.25,0.1,200),each=nrow(conditions.completed)),nrow(conditions.completed),5)
#(max(apply((current.condition==conditions.completed),1,min))==0)
#define current condition.
current.condition = matrix(rep(c(r.squared, mean(effect), prop.b, pseudo.r2, sample.size),
                               each=nrow(conditions.completed)),nrow(conditions.completed),5) 
#conditions in a single row first


#'only run if the condition has not been picked already
#'below gives true/false converted to 0/1, if the min is 0 then the current condition doesn't in the list completed
#'write that condition in the log, then the code can proceded with the condition, 
#'if the condition is completed, then the computer look for the next 
#'closes on the check condition, if the if is true, then it goes to the next and proceed to the next loop, 
#'some computer may work faster 
#'to implement this, a place that all computer can access through the coe-vfs server places
#'put list on github, upload simulation_control to pull it from there 
#'read on how to committ changes from rstudio, so that list is updating 
#'couldn't use dropbox, you can do comitts all from rstudio directly 
#'do it one iteration of each condition to allow you to estimat the time 
#'if it's even then you can split 
#'logging and multiple checking with rem lab computers 
#'timing-out on hypergator, and it will run a few hours 
#'each analysis will take about 20 minutes that it was timing out, but couldn't figure out, 
#'chunk in other ways to make it work 
#'codetoanalyze simulated data 2 has anaova
#'about computers, put email names 
#'added as a user, R.4.3
#'computer names the first one is good, faster 
#'
#'
if (max(apply((current.condition==conditions.completed),1,min))==0) {
  
  #save to the server the description of the conditions picked
  #so another computer cannot pick the same.
  write.table(matrix(current.condition[1,],1,),file="simulation_control.csv",
              append=T,sep=",", col.names=F,row.names=F,quote=F)
  
  
#---------------------------------------------------------------------  
datasets = data.frame()

#calculate the intercept for each proportion of the baseline
int = log((1/prop.b - 1)/3)


#obtain the effect of covariates on treatment assignment for a given pseudo-R2
b = sqrt(pseudo.r2*(pi^2/3)/(n.cov*(1 - pseudo.r2)))
coef.treat = cbind(intercepts = rep(int, 3), matrix(rep(b,n.cov*3),3,n.cov))

start.time = proc.time() #store starting computer time.
for (iter in 1:iterations) {
  
#simulate data
data = simulate.data(sample.size, pred.cov = pred.cov, 
                     gamma = gamma, r.squared = r.squared,coef.treat = coef.treat,
                     treat.eff=effect)



#Estimate the propensity scores by the baseline category logistic regression model
require(nnet)

ps.model = multinom(selection.model,data)
ps = data.frame(fitted(ps.model))

#obtain propensity score analyses
results = data.frame(rbind(cbind(estimate(data, outcome.model, ps ,method="baseline"),0),
            cbind(estimate(data, outcome.model, ps ,method="IPTW"),1),
            cbind(estimate(data, outcome.model, ps ,method="strata"), 2),
            cbind(estimate(data, outcome.model, ps ,method="ofpsm"), 3),
            cbind(estimate(data, outcome.model, ps ,method="mmw.strata"), 4),
            cbind(estimate(data, outcome.model, ps ,method="mmw.ofpsm"), 5)))
names(results)[4:5] = c("pvalue","ps.method")
results$coef.name = rep(c("int","C1","C2","C3"), times = (nrow(results)/4)) 
results$ps.method = factor(results$ps.method,levels = 0:5,
                      labels = c("baseline", "IPTW", "strata", "ofpsm",
                                 "mmw.strata","mmw.ofpsm"))
results$effect = rep(c(int,effect), times = (nrow(results)/4))
results$dataset = iter #add iteration number

#put results togeter
datasets = rbind(datasets, results)

#delete previous results and clean memory
rm(data,results,ps.model,ps) ; gc()

} # close loop through iterations
#Add condition ids.
datasets$sample.size = sample.size
datasets$r.squared = r.squared
datasets$prop.b = prop.b
datasets$pseudo.r2 = pseudo.r2

write.table(datasets, file="all_results.csv", sep=",", append=T,
            col.names = F, row.names = F)

#give feedback.
end.time = proc.time()
condition.time = end.time - start.time
  print(paste("Time:",condition.time[3], "r.squared:",r.squared, 
              "effect:",mean(effect), "prop.b:",prop.b, 
              "pseudo.r2:",pseudo.r2, "sample.size:",sample.size))
  
} #close if to check condition completion  
} #close loop through sample sizes
} #close loop through pseudo R2 
} #close loop through proportions in baseline
} #close loop through effects
} #close loop through R squared

simulation.end = proc.time()
total.time = simulation.end - simulation.start
total.time

