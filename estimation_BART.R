#estimating PS with BART 
# Probit BART with cluster indicators : BART model with probit link with pbart function in BART package for computational efficiency 
#called BART-FE 
install.packages("BART")
library(BART)
?pbart
#include group-id as a fixed effect to estimate PS
data("ACTG175")
ex <- is.na(ACTG175$cd496)
table(ex)
ACTG175$cd40 <- min(500, max(250, ACTG175$cd40))
#relative CD4 decline 
y <- ((ACTG175$cd496-ACTG175$cd40)/ACTG175$cd40)[!ex]
summary(y)
# 0 = failure, 1 = success 
y <- 1*(y>-0.5)

library(bartCause)
?bartc
install.packages("stan4bart")
library(stan4bart)
n <- 100L
beta.z <- c(.75, -0.5, 0.25)
beta.y <- c(.5, 1, -1.5)
sigma <- 2
 
set.seed(725)
x <- matrix(rnorm(3*n),n,3)
tau <- rgamma(1L, 0.25*16*rgamma(1L, 1*32, 32), 16)

p.score <- pnorm(x %*% beta.z)
z <- rbinom(n, 1, p.score)

mu.0 <- x %*% beta.y
mu.1 <- x %*% beta.y + tau

y <- mu.0 *(1-z) + mu.1*z + rnorm(n,0,sigma)

fit <- bartc(y, z, x, n.samples = 100L, n.burn =15L, n.chains = 2L)
fit$p.score
fit2 <- bartc(y, z, x, n.samples = 100L, n.burn =15L, n.chains = 2L,p.scoreAsCovariate = TRUE)
summary(fit)
summary(fit2)



covariateNames <- c(#student-level covariates:
  "X1RTHETK1", #X1 READING IRT THETA SCORE-K1 DATA FILE
  "X1MTHETK1", #X1 MATH IRT THETA SCORE--K1 DATA FILE
  "X1TCHAPP", #X1 TEACHER REPORT APPROACHES TO LEARNING
  "X1TCHCON", #X1 TEACHER REPORT SELF-CONTROL
  "X1TCHPER", #X1 TEACHER REPORT INTERPERSONAL
  "X1TCHEXT", #X1 TEACHER REPORT EXTERN PROB BEHAVIORS
  "X1TCHINT", #X1 TEACHER REPORT INTERN PROB BEHAVIORS
  "X1ATTNFS", #X1 TEACHER REPORT ATTENTIONAL FOCUS
  "X1INBCNT", #X1 TEACHER REPORT INHIBITORY CONTROL
  "X_CHSEX_R",#Sex
  "X_RACETH_R",#Race
  "X2DISABL2", #Disability
  "P1HSCALE", #health
  "C1ENGHM", #English Language Learner
  "X12MOMAR", #Parents married
  "X1NUMSIB", # of siblings
  "P1OLDMOM", #age of mom
  "P1CHLDBK", #books at home
  "P2DISTHM", #distance from school
  "P1NUMPLA", #places child lives
  "T2PARIN", #parental involvement
  "X12PAR1ED_I", #mother education
  "X12PAR2ED_I", #father education
  "X2INCCAT_I", #income
  "X1PAR1EMP", #PARENT 1 EMPLOYMENT STATUS
  #school level covariates:
  "S2LUNCH", #P2 Percent Eligible Free or Reduced Lunch
  "X2KRCETH",#X2 Percent NonWhite Students in School
  "S2NGHBOR", #S2 Percent From Neightborhood
  "S2OUTSID", #S2 PErcent Sent with Special Need
  "S2USDABR", #S2 join USDA's breakfast program
  "S2PUBSOC", #attending the school under public school choice
  "X1LOCALE", #LOCATION TYPE OF SCHOOL (city, suburt, town, rural)
  "prop.missing") #proportion of missing values

#convert school id to a factor
data$S1_ID <- factor(data$S1_ID)
data$W1_2P0PSU <- factor(data$W1_2P0PSU)

#ESTIMATE Propensity scores with fixed effects models 
#create a formula that excludes the school variables
#the school id and cluster id are included as predictors 
#they have to be factors, so R automatically dummy codes them in the model
ps.formula2 <- paste(covariateNames[c(1:25,33)], collapse="+") 
ps.formula2 <- formula(paste("treated~",ps.formula2,"+ S1_ID + W1_2P0PSU")) 

#'treated ~ X1RTHETK1 + X1MTHETK1 + X1TCHAPP + X1TCHCON + X1TCHPER + 
#'X1TCHEXT + X1TCHINT + X1ATTNFS + X1INBCNT + X_CHSEX_R + X_RACETH_R + 
#'X2DISABL2 + P1HSCALE + C1ENGHM + X12MOMAR + X1NUMSIB + P1OLDMOM + 
#'P1CHLDBK + P2DISTHM + P1NUMPLA + T2PARIN + X12PAR1ED_I + 
#'X12PAR2ED_I + X2INCCAT_I + X1PAR1EMP + prop.missing + S1_ID + 
#'W1_2P0PSU


require(bartCause)
#split the dataset into training and test data
train.sample = sample(1:nrow(dataset1_full), nrow(dataset1_full)/2)
train.sample = train.sample[order(train.sample)]
test.sample = 1:nrow(dataset1_full)
test.sample = test.sample[-c(train.sample)]

#run training causal forest
data.train = dataset1_full[train.sample,]
data.test = dataset1_full[test.sample,]
data.train = as.data.frame(data.train)
data.test = as.data.frame(data.test)

#run bart including teacher id to account for clustering effects
bart = bartc(response=as.numeric(data.train$yij11), treatment=as.numeric(data.train$trt1), 
             confounders=data.frame(data.train[,c(3:20)]), 
             keepTrees = TRUE,
             method.rsp="p.weight", method.trt="bart", estimand="ate")

conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(1,3:20)]), type="icate")
conditional.ind.effects = apply(conditional.ind.effects, 2, mean) #take mean of posterior predictions

bart$p.score
bart
class(data.train$schoolid)
data.train$schoolid = as.numeric(data.train$schoolid)

bart2 = bartc(response=yij11, treatment=trt1, 
             confounders= x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10+(1|schoolid),
             #parametric = (1|schoolid), 
             data= data.train,
             keepTrees = TRUE,
             method.rsp="p.weight", 
             method.trt="bart", 
             use.ranef = TRUE,
             group.effects = schoolid,
             estimand="ate")
bart2$p.score
bart2$estimand
bart2$est
extract(bart2,type = "icate")
predict(bart2, newdata=data.test,type = "y")

?predict.bartcFit
mean(bart2$p.score)
ATE = summary(bart2, target="pate")
?bartc
bart3 = bartc()
?stan4bart
library(survey)
install.packages("survey")
library(dbarts)

formula
?stan4bart
summary(bart)
summary(bart2)
class(bart)
bart2$p.score
summary(bart)

library(sandwich)
dataset1_full <- as.data.frame(dataset1_full)
vcovCL(bart,cluster = dataset1_full$schoolid)
coeftest(bart,vcovCL, cluster=dataset1_full$schoolid)
?vcovCL

#detect interactions
library("bartMachine")
set_bart_machine_num_cores(8)
bart_machine <- bartMachine(X=data.test[,c(3:20)], 
                            y=conditional.ind.effects, 
                            serialize = TRUE, mem_cache_for_speed=F)
bart_machine

var_importance = investigate_var_importance(bart_machine, num_replicates_for_avg = 25)
var_importance

interactions = interaction_investigator(bart_machine, num_replicates_for_avg = 25,
                                        num_var_plot = 6, bottom_margin = 5)
interactions

library(stan4bart)
library(bartMachine)


predict(fit_bart,newdata = data.test,type = "p.score")


fit_bart_test <- bartc(yij, treatment =trt,
                       confounders=x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10,
                       data = data.train,
                       keepTrees=TRUE)
score_bart_test <- predict(fit_bart_test, newdata=data.test,type="p.score")
score_bart_test <- apply(score_bart_test,2,mean) #keep p.score for test

iCATE_bart_test <- predict(fit_bart_test, newdata=data.test,type="icate")
iCATE_bart_test = apply(iCATE_bart_test, 2, mean)



names(dataset1_full_trt2)[2] <- "trt"
names(dataset1_full_trt2)[21] <- "yij"

#splitting train and test into 70% train 30% test
sample <- sample(c(TRUE, FALSE), nrow(dataset1_full_trt2), replace=TRUE, prob=c(0.5,0.5))
data.train  <- dataset1_full_trt2[sample, ]
data.test   <- dataset1_full_trt2[!sample, ]


test_bart <- BART_est(data.train = data.train, data.test = data.test)


fit_bart
?bartc
fit_bart$mu.hat.obs[1:900] #the first row, individual responses under the observed treatment regime 
fit_bart$mu.hat.cf [1:900] #the first row, individual responses under the counterfactual treatment
fit_bart$mu.hat.cf
fit_bart$trt
fit_bart$sd.obs

fit_bart_test <- bartc(yij11, treatment =trt1,
                       confounders=x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10,
                       data = data.train,
                       keepTrees=TRUE)
score_bart_test <- predict(fit_bart_test, newdata=data.test,type="p.score")
score_bart_test <- apply(score_bart_test,2,mean) #keep p.score for test




#obtain the PS score run bart including teacher id to account for clustering effects
bart = bartc(response=yij, 
             treatment=trt, 
             confounders=x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10+(1|schoolid), 
             #parametric = (1|schoolid),#to indicate for the group effect, having trouble
             data = data.train,
             keepTrees = TRUE,
             method.rsp="p.weight", 
             method.trt="bart", 
             estimand="ate")
predict.pscore = bart$p.score
predict.trt = bart$trt

dataset1_full_trt2
conditional.ind.effects = predict(bart, newdata = data.frame(data.test[,c(3:20)]), type="icate")

#standard error estimation -- under construction 



}







fit_bart_ps <- bartc(response=trt1, 
                     treatment=trt1, 
                     confounders=x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10+(1|schoolid), 
                     #parametric = (1|schoolid),#to indicate for the group effect, having trouble
                     data = data.train,
                     keepTrees = TRUE)
#method.rsp="bart", 
#method.trt="bart", 
#estimand="ate")
score_bart <- predict(fit_bart_ps,newdata = data.test,type="p.score")
score_bart
data.t


