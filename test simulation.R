set.seed(1233)
nG <- 60 #number of groups 
nJ <- 10 #cluster size 

#treatment effect model 
# tau = 2
# tau = 0.2X1ij + 0.6X2ij+0.09V1j +0.16V2j + etaij
# tau = 0.2X1ij + 0.6X2ij+0.09V1j +0.16V2j + 0.25X1ij*V1j + 0.09V3_j^2 +etaij

V1 <- 0.09 #level 2 coeff 

X1 <- 3 #level 1 coeff 

tmp2 <- rnorm(nG) #20 random numbers, m=0,sd=1
l2 <- rep(tmp2,each = nJ) #all units in l2 have the same value 
groups <- gl(nG, k=nJ) #creating cluster variable
tmp2err <- rnorm(nG) #error term for level 2 
err2 <- rep(tmp2err, each = nJ) #all units in l2 have the same value

l1 <- rnorm(nG*nJ) #total sample size is nG*nJ
err1 <- rnorm(nG*nJ) #level 1 

#putting it all together 
y <- W1*l2 +X1*l1 +err2 + err1
dat <- data.frame(y, groups, l2, err2, l1,err1)


library(lme4) #to run multilevel models
install.packages("jtools")
library(jtools) #to get nicer output
mlm0 <- lmer(y ~ (1|groups), data = dat) #unconditional
summ(mlm0) #shows the ICC, close


mlm1 <- lmer(y ~ l2 + l1  + (1|groups), data = dat)
ols1 <- lm(y ~ l2 + l1, data = dat)

summ(ols1, cluster = 'group', robust = T)






