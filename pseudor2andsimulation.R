#pseudo- r2 experiment 

require(MASS)

IUCC_S2 = 0.05
IUCC_S4 = 0.1
rescorr_lev2 = 0.3
# In outcome model
var.lev1.res = 1  #variance of level-1 residual in outcome model
var.S4_res = IUCC_S4*((var.lev1.res*(IUCC_S2+IUCC_S4)/(-IUCC_S4-IUCC_S2+1))+var.lev1.res) #Variance of S4 residual using IUCC in outcome model
var.S2_res = IUCC_S2*((var.lev1.res*(IUCC_S2+IUCC_S4)/(-IUCC_S4-IUCC_S2+1))+var.lev1.res)   #Variance of S2 residual in outcome model 
cov_S2S4 = rescorr_lev2*sqrt(var.S2_res)*sqrt(var.S4_res) #getting variance-covariance between S2 and S4
# In propensity score model
var.lev1.res = 1
var.lev1.res.t = pi^2/3
var.S4_res.t = IUCC_S4*((var.lev1.res*(IUCC_S2+IUCC_S4)/(-IUCC_S4-IUCC_S2+1))+var.lev1.res.t)
var.S2_res.t = IUCC_S2*((var.lev1.res*(IUCC_S2+IUCC_S4)/(-IUCC_S4-IUCC_S2+1))+var.lev1.res.t) 
cov_S2S4.t = rescorr_lev2*sqrt(var.S2_res.t)*sqrt(var.S4_res.t) #getting variance-covariance between S2 and S4
fincross_corr_t = mvrnorm(level2n, mu=c(0,0), Sigma=matrix(c(var.S2_res.t,cov_S2S4.t,cov_S2S4.t,var.S4_res.t),2,2))


#pseudo R2
coef_lev1.t = runif(3,0.1,0.6)
coef_lev21.t = runif(3,0.1,0.6)
coef_lev22.t = runif(3,0.1,0.6)

varexp_1 = matrix(coef_lev1.t,1,3) %*% cov_mat_lev1 %*% matrix(coef_lev1.t,3,1)
varexp_21 = matrix(coef_lev21.t,1,3) %*% cov_mat_lev2_1 %*% matrix(coef_lev21.t,3,1)
varexp_22 = matrix(coef_lev22.t,1,3) %*% cov_mat_lev2_2 %*% matrix(coef_lev22.t,3,1)

P_R2_lev1 = (varexp_1) / (varexp_1 + varexp_21 + varexp_22 + var.S2_res.t + var.S4_res.t + var.lev1.res.t)
P_R2_lev2 = (varexp_21 + varexp_22) / (varexp_1 + varexp_21 + varexp_22 + var.S2_res.t + var.S4_res.t + var.lev1.res.t)

269/2

install.packages("performance")
library(performance)
r2_mckelvey(r.glm)
install.packages("MuMIn")
predict(r.glm, type="response")



mz_rsquare = function(y_pred){
  rsq = var(y_pred) / (var(y_pred) + (pi^2)/3)
  return(rsq)}
mz_rsquare(y_pred = predict(r.glm, type="response"))


##### 
# simulating mixed effects 
install.packages("tidyverse")
install.packages("GGally")
install.packages("faux")
library(tidyverse)
library(lmerTest)
library(GGally)
library(faux)
version
options("scipen"=10, "digits"=4) # control scientific notation
set.seed(8675309) # Jenny, I've got your number

# subjects 
sub_n  <- 200 # number of subjects in this simulation
sub_sd <- 100 # SD for the subjects' random intercept

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i  = rnorm(sub_n, 0, sub_sd), # random intercept
  sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
)

#stimuli 
stim_n  <- 50 # number of stimuli in this simulation
stim_sd <- 50 # SD for the stimuli's random intercept


stim <- tibble(
  stim_id = 1:stim_n,
  stim_i = rnorm(stim_n, 0, stim_sd) # random intercept
)
#trails 
trials <- crossing(
  sub_id = sub$sub_id, # get subject IDs from the sub data table
  stim_id = stim$stim_id, # get stimulus IDs from the stim data table
  stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
) %>%
  left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
  left_join(stim, by = "stim_id")   # includes the intercept for each stimulus


# set variables to use in calculations below
grand_i          <- 400 # overall mean DV
sub_cond_eff     <- 50  # mean difference between conditions: hard - easy
stim_version_eff <- 50  # mean difference between versions: incongruent - congruent
cond_version_ixn <-  0  # interaction between version and condition
error_sd         <- 25  # residual (error) SD


dat <- trials %>%
  mutate(
    # effect-code subject condition and stimulus version
    sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
    stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = +0.5),
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, error_sd),
    # calculate DV from intercepts, effects, and error
    dv = grand_i + sub_i + stim_i + err +
      (sub_cond.e * sub_cond_eff) + 
      (stim_version.e * stim_version_eff) + 
      (sub_cond.e * stim_version.e * cond_version_ixn) # in this example, this is always 0 and could be omitted
  )


ggplot(dat, aes(sub_cond, dv, color = stim_version)) +
  geom_hline(yintercept = grand_i) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9))



#### 
set.seed(37)
n = 10000
x1 = rnorm(n,mean=0,sd=1)
x2 = rnorm(n, mean = 0, sd=1)
z = pi^2/3 + 2*x1 + 3*x2
p = 1/ (1+exp(-z))
summary(p)
sort(p)
y = rbinom(size=1,n=n,prob = p)

exp(0)
testdata= as.data.frame(cbind(y,x1,x2))

lr = glm(y~x1+x2, data=testdata,family=binomial)
summary(lr)
y_pred = predict(lr, type = "response")
psudor2 = mz_rsquare(y_pred=y_pred)
psudor2


