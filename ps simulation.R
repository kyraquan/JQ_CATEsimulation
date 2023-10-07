library(lme4)
library(lmtest)
library(simstudy)
library(MASS)

#propensity score model 
Ni <- 30 #30,60 #individuals in a group
Nj <- 60 #200,500 #number of groups 
n <- Ni*Nj 
schoolid <- gl(Ni,k=Nj)
?gl
set.seed(89375)
ps1 <- rbinom(n,1,0.5)

x1ij <- rbinom(n,1,0.5) #level-1 
x2ij <- rnorm(n,0,1)
v1j <- rbinom(Nj,1,0.3) #level-2
v2j <- runif(Nj,0,1)
v3j <- rnorm(Nj,0,1)
rij <- rlogis(n,0,pi^2/3) #error/residual for level 1 
pi

V1rep <- rep(v1j,each=Ni)
V2rep <- rep(v2j,each=Ni)
V3rep <- rep(v3j,each=Ni)

# selection probability
prob1 <- 0.1*x1ij+0.03*x2ij+0.16*V1rep+0.08*V2rep+rij -1.69
prob1 <- 1/(1+exp(prob1))

1/(1+exp(pi^2/3))

prob2 <- 0.1*x1ij+0.03*x2ij+0.16*V1rep+0.08*V2rep+0.09*x2ij^2+0.25*V2rep^2+rij-1.6
prob2 <- 1/(1+exp(prob2))

# different treatment selection conditions
trt1 <- rbinom(n,size=1,prob=0.5)
trt2 <- rbinom(n,size=1,prob = prob1)
trt3 <- rbinom(n,size=1,prob = prob2) 

1/(1+exp(-2.1))


## test 
r.glm <- glmer(trt2~x1ij+x2ij+V1rep+V2rep+(1|schoolid),data = trt1dat,family=binomial,control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
prob2.glm <- glm(trt2~x1ij+x2ij+V1rep+V2rep+(1|schoolid),data = trt1dat,family=binomial,control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10)
summary(r.glm)
class(r.glm)

r.glm2 <- glm(trt2~x1ij+x2ij+V1rep+V2rep,data = trt1dat)
class(r.glm2)

summary(r.glm2)

library(DescTools)
PseudoR2(r.glm,c("McFadden", "Nagel"))
PseudoR2(r.glm2, c("McKelveyZavoina"))

fit <- glmer(trt2 ~(1|schoolid),family = binomial("logit"),data=trt1dat)
PseudoR2(fit) #NA ?

trt1dat <-cbind(schoolid,x1ij,x2ij,V1rep,V2rep,rij,trt2)
trt1dat <- as.data.frame(trt1dat)

?PseudoR2

## pseudoR2 by hand using MckelveyZavoina 
## generalized multilevel modeling book for the formula to calculate 
#'Snijders, T. A. B., & Bosker, R. J. (2012). 
#'Multilevel analysis: An introduction to basic and advanced multilevel modeling (2nd ed.). Sage Publications.
#'coeff * varcovar - phi-lambfa_1/varexp+tau^2, have the group variance 



#treatment effect model 
nij <- rnorm(n,0,0.1)

tau1 <- rep(2,n)
tau2 <- 0.2*x1ij+0.6*x2ij+0.09*V1rep+0.16*V2rep+nij
var(tau2) #0.3887582 
tau3 <- 0.2*x1ij+0.6*x2ij+0.09*V1rep+0.16*V2rep+0.25*x1ij*V1rep+0.25*V3rep+nij
var(tau3) #0.4469932





### outcome model 

library(simstudy)
targetICC <- c(0.1,0.3)
setVars <- iccRE(targetICC,dist = "normal",varWithin = 1)
round(setVars,4) #obtain the variance between groups, the site-level random effect
# 0.1111 0.4286 
#individual-level (within) variance of 1 
set.seed(745)
#testing: specify between site variation 
d <- defData(varname = "a", formula = 0, variance = 0.2105, id = "school")
d <- defData(d, varname = "intercept", dist="nonrandom",formula=1,id="idnum")
?defData

# write the simulation conditions as comment in the code for documentation 
#simulate the large dataset to see if the results for ICC to see if it is closer to the simulation
#use glm before fitting model , if close then have it's validation 

#out comemodel 
uj <- rnorm(Nj,0,0.111111)
ujrep <- rep(uj,each=Ni)
eij <- rnorm(n,0,1)

yij1 <- 1+tau1*trt1+0.4*x1ij-0.3*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
var(yij1)
yij2 <- 1+tau1*trt1+0.4*x1ij-0.3*x2ij+x1ij*x2ij+0.4*V1rep-0.4*V2rep+ujrep+eij
var(yij2)
yij3 <- 1+tau1*trt1+0.4*x1ij-0.3*x2ij+x1ij*V1rep+0.4*V1rep-0.4*V2rep+0.16*V2rep+ujrep+eij
var(yij3)

#### distractors 
# load library MASS
library(MASS)

# set seed and create data vectors; obtained online 
# need improvement and revision for more accurate distractors 
#' www.practicalpropensityscore.com 
#' all datasets are there with multiple variables 
#' except for chapter 6 
#' 


set.seed(98989)
sample_size <- 600							
sample_meanvector <- c(0.10, .5, .7, .9, 2)								
sample_covariance_matrix <- matrix(c(5, 4, 3, 2, 1, 4, 5, 4, 3, 2,
                                     3, 4, 5, 4, 3, 2, 3, 4, 5, 4, 1,
                                     2, 3, 4, 5), ncol = 5)

# create multivariate normal distribution
sample_distribution <- mvrnorm(n = sample_size,
                               mu = sample_meanvector,
                               Sigma = sample_covariance_matrix)

# print top of distribution
head(sample_distribution)

#

### dataset 
dataset1 <- cbind(schoolid,trt1,x1ij,x2ij,V1rep,V2rep,V3rep)
dataset1_full <- cbind(dataset1,yij1,distractor_distribution)
#' create functions in seperate files 
#' function to simulate
#' function to analysis 
#' have open science site 
#' cross-classified data code : https://osf.io/7eqmc/ 
#' PS analysis with missing data paper put in the actual open scicence framework 



### distractor test 
TRUTH= 0.8 
R <- as.matrix(data.frame(c(1, TRUTH), c(TRUTH, 1)))
S = c(sqrt(1), sqrt(1))

cor2cov_1 <- function(R,S){
  diag(S) %*% R %*% diag(S)
}

cor2cov_1(R,S)

outer(S,S) * R 

smat = as.matrix(S)
R * smat %*% t(smat)

cor2cov <- function(R, S) {
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}
cor2cov(R,S)

S= c(1,1,1,1,1,1,0.5,0.56,1,0.39,1,1,0.5)
R <- as.matrix(data.frame(c(1,.234,.311,.341,.192,-.101,.067,.043,.289,.147,.079,-.108,.124), #1
                          c(.234,1,.219,.152,.130,-0.054,.026,.031,.179,.134,.065,-.05,.059),#2
                          c(.311, .219, 1, .174, .124, -0.086,0.058, .052, .228, .061, .049, -.066, .073),#3
                          c(.341, 1.52, .174,1, .192,-.101,0.067, .043, .289, .147, .079, -.108, .124),#4
                          c(.192,.130,.124,.177,1, -.279,.163, .024, .182, .233, .342, -.177, .067),#5
                          c(-.101, -.054, -.086, -.246, -.279,1 , -.058, -.016, -.127,-.032, -.113, .087, -.02),#6
                          c(.067, .026, .058,.058,.163,-.058,1, .045, .063, .02, .076, -.093, .027), #7
                          c(.043, .031, .052, .043, .024, -.016, .045,1,.101, .056, .002, -.021, .025), #8
                          c(.289, .179, .228, .140, .182, -.127, .063, .101,1, .257, .126, -.252, .177), #9 
                          c(.147, .134, .061, .004, .233, -.032, .020, .056, .257, 1, .221, -.12,.061), #10 
                          c(.079, .065, .049, .047, .342, -.113, .076, .002, .126, .221, 1, -.289, .031), #11
                          c(-.108, -.050, -.066, -.059, -.177, .087, -.093, -.021, -.252, -.12, -.289,1, -.075),#12
                          c(.124, .059, .073, .003, .067, -.02, .027, .025, .177, .061, .031, -.075, 1) #13
                          ))
comatrix <- cor2cov_1(R,S)
comatrix <- cor2cov(R,S)

sample_meanvector <- c(0,0,0,0,0,0,0.5,3.22,0,0.81,0,0,0.5)								
sample_covariance_matrix <- matrix(c(5, 4, 3, 2, 1, 4, 5, 4, 3, 2,
                                     3, 4, 5, 4, 3, 2, 3, 4, 5, 4, 1,
                                     2, 3, 4, 5), ncol = 5)
# https://repository.usfca.edu/cgi/viewcontent.cgi?article=1502&context=diss 
#source of covariance matrix 
# create multivariate normal distribution
distractor_distribution <- mvrnorm(n = 1800,
                               mu = sample_meanvector,
                               Sigma = comatrix)

#data set full 
dataset1_full <- cbind(dataset1,distractor_distribution)










set.seed(1234) #for reproduction
nG <- 20 #number of groups
nJ <- 30 #cluster size
W1 <- 2 #level 2 coeff
X1 <- 3 #level 1 coeff

tmp2 <- rnorm(nG) #generate 20 random numbers, m = 0, sd = 1
l2 <- rep(tmp2, each = nJ) #all units in l2 have the same value
var(l2)
group <- gl(nG, k = nJ) #creating cluster variable
tmp2 <- rnorm(nG) #error term for level 2
err2 <- rep(tmp2, each = nJ) #all units in l2 have the same value
var(err2)

l1 <- rnorm(nG * nJ) #total sample size is nG * nJ
err1 <- rnorm(nG * nJ) #level 1 

#putting it all together
y <- W1 * l2 + X1 * l1 + err2 + err1
dat <- data.frame(y, group, l2, err2,l1, err1) 


library(lme4) #to run multilevel models
library(jtools) #to get nicer output
mlm0 <- lmer(y ~ (1|group), data = dat) #unconditional
summ(mlm0) #shows the ICC, close
library(lmtest)

mlm01 <- lmer(resp ~ (x+1|ID), data = dat1) #unconditional
summ(mlm01) #shows the ICC, close
summary(mlm01)
1.26/(1.26+9.486)
?simulate

(0.16+0.16+1 )/ (0.32+2+0.16+0.09) #0.51 
var(rij)
pi^2/3

1/(1+exp(pi^2/3))
#target ICC = 0.1, 0.3 
library(simstudy)
targetICC <- c(.1,.3)
setVars <- iccRE(ICC = targetICC, dist = "normal",varWithin = 1)
setVars #obtaining the variance between group 
# 0.1111111 0.4285714
install.packages(lme4)
library(lme4)
install.packages("dbarts")


b0_grid = seq(from = -5, to = 5, by = 0.10)
b0.dfs <- data.frame( grid = b0_grid, prev.actual = prev.actual )

prev.actual=NULL
treatment_prevalence = NULL
which.min(abs(prev.actual-treatment_prevalence))

1/(1+exp(x))=0.44
1/0.44 = 1+exp(x)
1/0.44-1=exp(x)
exp(x) = 1.273
x=log(1.273)
x
exp(0.2414)
1/(1+exp(0.2414))

log(0.8)/log(2)
exp(-0.32)
