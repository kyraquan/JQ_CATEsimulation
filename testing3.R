set.seed(1234) #for reproducability
nG <- 20 #number of groups
nJ <- 30 #cluster size
W1 <- 2 #level 2 coeff
X1 <- 3 #level 1 coeff

tmp2 <- rnorm(nG) #generate 20 random numbers, m = 0, sd = 1
l2 <- rep(tmp2, each = nJ) #all units in l2 have the same value
group <- gl(nG, k = nJ) #creating cluster variable
tmp2 <- rnorm(nG) #error term for level 2
err2 <- rep(tmp2, each = nJ) #all units in l2 have the same value

l1 <- rnorm(nG * nJ) #total sample size is nG * nJ
err1 <- rnorm(nG * nJ) #level 1 

#putting it all together
y <- W1 * l2 + X1 * l1 + err2 + err1
dat <- data.frame(y, group, l2, err2,l1, err1)

dat$group <- as.integer(dat$group)
between <- dat[,mean(y),keyby=group][,var(V1)]
between <- dCn05[, mean(y1), keyby = grp][, var(V1)]

mean(with(dat, tapply(y, group, var))) #variance across a factor of group - between group
#10.44742 
dat$id <- rep(1:30,20)
mean(with(dat, tapply(y, id, var))) #variance across a factor of id - within group 
#14.44158
10.44742 /(10.44742 +14.44158)

fit = lm(y~1|group,dat)
anova(fit)
0.0836/(14.3652+0.0836)

# https://stackoverflow.com/questions/1401894/calculate-within-and-between-variances-and-confidence-intervals-in-r
mean(c(0.005833333, 0.310000000, 0.610000000, 0.043333333)) #within group variance "residual, mean sq"

mean(c(0.48562500, 0.88729167, 0.05583333))#between-group variance "group, mean sq") 


ICC(fit)
library(simstudy)
targetICC <- c(.05,.075,.1)
setVars <- iccRE(ICC = targetICC, dist = "normal",varWithin = 4)
setVars #obtaining the variance between group 
0.4/0.9
set.seed(73632)
d <- defData(varname = "a", formula = 0,variance = 0.2105,id="grp")
d <- defData(d, varname = "size",formula = 1000, dist = "nonrandom")
?defData
?distributions
a <- defDataAdd(varname = "y1", formula = "30+a",variance = 4, dist = "normal")
dT <- genData(1000,d)
dCn05 <- genCluster(dtClust = dT, cLevelVar = "grp",numIndsVar = "size",level1ID = "id")
dCn05 <- addColumns(a,dCn05)
var(dCn05$a)


# ICC, rho = between/(between + within)

model0_fit <- lmer(formula = y ~ 1+(1|group),data = dat)
VarCorr(model0_fit)
randomeffects <- as.data.frame(VarCorr(model0_fit))
randomeffects
randomeffects[1,4]/(randomeffects[1,4]+randomeffects[2,4])  



library(GenericML)
?BLP
