library(grf)
library(rpart)
library(glmnet)
library(splines)
library(lmtest)
library(MASS)
library(sandwich)
library(ggplot2)
library(reshape2)
library(stringr) 

#simulating data 
n <- 500
x <- runif(n, -4, 4)
mu <- ifelse(x<0, cos(2*x), 1-sin(x))
y <- mu + 1*rnorm(n)
mu

data<- data.frame(x=x, y=y)
outcome <- "y"
covariates <- c("x")

plot(x,y, col="black", ylim=c(-4,4), pch=21, bg="red", ylab="Outcome y", las=1)
lines(x[order(x)], mu[order(x)], col="black", lwd=3, type="l")
legend("bottomright", legend=c("Ground truth E[Y|X=x]", "Data"), cex = .8, 
       lty = c(1, NA), col="black", pch=c(NA,21), pt.bg=c(NA, "red"))

#overfitting 
subset <- 1:30
fmla <- formula(paste0(outcome, "~poly(", covariates[1], ",10)"))
fmla

ols <- lm(fmla, data= data, subset = subset)
summary(ols)
#compute a grid of x1 values we'll use for prediction 
x <- data[,covariates[1]]
x.grid <- seq(min(x), max(x), length.out=1000)
x
x.grid
new.data <- data.frame(x.grid)
colnames(new.data) <- covariates[1]
#predict 
y.hat <- predict(ols, newdata = new.data)
#plotting observations (in red) and model predictions (in green)
plot(data[subset, covariates[1]], data[subset, outcome], pch=21, bg="red",
     xlab = covariates[1], ylim = c(-3,3), ylab = "Outcome y", las=1)
lines(x.grid, y.hat, col="green", lwd=2)
legend("bottomright", legend = c("Estimate", "Data"), col=c("green","black"), pch=c(NA,21),
       pt.bg = c(NA,"red"), lty=c(1,NA), lwd=c(2,NA), cex=.8)

#underfitting
subset <- 1:25 
fmla <- formula(paste0(outcome, "~", covariates[1]))
covariates[1]
ols <- lm(fmla, data[subset,])
x <- data[,covariates[1]]
x.grid<- seq(min(x), max(x),length.out=1000)
new.data <- data.frame(x.grid)
colnames(new.data) <- covariates[1]
y.hat <- predict(ols, newdata = new.data)
plot(data[subset,covariates[1]], data[subset,outcome], pch=21, bg="red", xlab=covariates[1], ylab="Outcome y", las=1)
lines(x.grid,y.hat, col="green",lwd=2)
legend("bottomright", legend = c("Estimate", "Data"), col = c("green","black"),pch =c(NA, 21), pt.bg = c(NA,"red"), lty=c(1,NA),lwd =c(2,NA), cex=.8)


# bias-variance trade-off : training and testing 
poly.degree <- seq(3,20)
train <- sample(1:n, 0.5*n)
mse.estimates <- lapply(poly.degree, function(q) {
  fmla <-formula(paste0(outcome,"~poly(",covariates[1],",",q,")"))
  ols <- lm(fmla, data=data[train,])
  
  #prediction on the training subset
  y.hat.train <- predict(ols)
  y.train <- data[train,outcome]
  #prediction on the validation subset 
  y.hat.test <- predict(ols, newdata = data[-train,])
  y.test <- data[-train,outcome]
  
  #compute MSE 
  data.frame(
    mse.train=mean((y.hat.train-y.train)^2),
    mse.test=mean((y.hat.test-y.test)^2))
  
})
mse.estimates <- do.call(rbind, mse.estimates)

matplot(poly.degree, mse.estimates, type = "l", ylab="MSE estimate", xlab="Polynomial degree", las=1)
text(poly.degree[2], .9*max(mse.estimates), pos=4, "<-----\nHigh bias\nLow variance")
text(max(poly.degree),.9*max(mse.estimates), pos=2, "----->\nLow bias\nHigh variance")
legend("top", legend = c("Training", "Validation"), bty="n", lty=1:2,col=1:2,cex = .7)


#diving into k folds , k-fold cross-validation 
n.folds <- 5 
poly.degree <- seq(4,20)
#list of indices that will be left out at each step 
indices <- split(seq(n), sort(seq(n) %% n.folds))
?split
?sapply

mse.estimates <- sapply(poly.degree, function(q){
  fmla <- formula(paste0(outcome, "~poly(", covariates[1],",",q,")"))
  y.hat <- lapply(indices, function(fold.idx){
    #fit on k-1 folds, leaving out observations in fold.idx
    ols<- lm(fmla, data=data[-fold.idx, ])
    predict(ols, newdata = data[fold.idx,])
  })
  y.hat <- unname(unlist(y.hat))
  
  mean((y.hat-data[,outcome])^2)
  
  
})

plot(poly.degree, mse.estimates, ylab = "MSE estimate", xlab = "Polynomial degree", type="l", lty=2, col=2, las=1)
legend("top", legend = c("Cross-validated MSE"), bty = "n", lty=2,col = 2,cex=.7)

#fitting using more data 
subset <- 1:n 
fmla <- formula(paste0(outcome,"~ poly(", covariates[1], ",15)"))
ols <- lm(fmla,data,subset = subset)
#compute a grid of x1 values we'll use for prediction 
x <- data[,covariates[1]]
x.grid <- seq(min(x),max(x),length.out=1000)
new.data <- data.frame(x.grid)
colnames(new.data) <- covariates[1]
y.hat <- predict(ols, newdata = new.data)
plot(data[subset, covariates[1]], data[subset,outcome], pch=21, bg="red", xlab=covariates[1],ylab = "Outcome",las=1)
lines(x[order(x)], mu[order(x)],lwd=2, col="black")
lines(x.grid, y.hat, col="green", lwd=2)
legend("bottomright", lwd = 2, lty=c(1,1), col = c("black","green"), legend = c("Ground truth", "Estimate"))


# load dataset
data <- read.csv("https://docs.google.com/uc?id=1kNahFWMGUEB3Qz83s6rMf1l684MSqc_3&export=download")

# outcome variable name
outcome <- "LOGVALUE"
# covariates
true.covariates <- c('LOT','UNITSF','BUILT','BATHS','BEDRMS','DINING','METRO','CRACKS','REGION','METRO3','PHONE','KITCHEN','MOBILTYP','WINTEROVEN','WINTERKESP','WINTERELSP','WINTERWOOD','WINTERNONE','NEWC','DISH','WASH','DRY','NUNIT2','BURNER','COOK','OVEN','REFR','DENS','FAMRM','HALFB','KITCH','LIVING','OTHFN','RECRM','CLIMB','ELEV','DIRAC','PORCH','AIRSYS','WELL','WELDUS','STEAM','OARSYS')
p.true <- length(true.covariates)
# noise covariates added for didactic reasons
p.noise <- 20
noise.covariates <- paste0('noise', seq(p.noise))
covariates <- c(true.covariates, noise.covariates)
X.noise <- matrix(rnorm(n=nrow(data)*p.noise), nrow(data), p.noise)
colnames(X.noise) <- noise.covariates
data <- cbind(data, X.noise)
#sample size 
n <- nrow(data)
p <- length(covariates) #total number of covariates 
round(cor(data[, covariates[1:8]]), 3)

fmla <- formula(paste("~0+", paste0(covariates, collapse = "+")))
#selects covariates according to the formula above and expands the covariates accordingly
XX <- model.matrix(fmla, data)
Y <- data[, outcome]
# fit a lasso model 
lasso <-cv.glmnet(
  x=XX, y=Y,
  family = "gaussian", #use binomial for logistic regression 
  alpha =.1, #alpha=0 for ridge, or alpha in (0,1) for elastic net
)
#plot average estimated MSE for each lambda 
par(oma=c(0,0,3,0))
plot(lasso, las=1)
mtext('Number of Non-Zero Coefficients', side =3, line =3)
#'Figure 2.7 plots the average estimated MSE for each lambda. 
#'The red dots are the averages across all folds, and the error bars are 
#'based on the variability of mse estimates across folds. The vertical dashed
#' lines show the (log) lambda with smallest estimated MSE (left) and 
#' the one whose mse is at most one standard error from the first (right). 
#' The top x-axis indicates the number of nonzero coefficient estimates.
#' 
#' 
# Estimated coefficients at the lambda value that minimized cross-validated MSE
coef(lasso, s = "lambda.min")[1:5,]  # showing only first coefficients
print(paste("Number of nonzero coefficients at optimal lambda:", lasso$nzero[which.min(lasso$cvm)], "out of", length(coef(lasso))))

# Retrieve predictions at best lambda regularization parameter
y.hat <- predict(lasso, newx=XX, s="lambda.min", type="response")

# Get k-fold cross validation
mse.glmnet <- lasso$cvm[lasso$lambda == lasso$lambda.min]
print(paste("glmnet MSE estimate (k-fold cross-validation):", mse.glmnet))
#plots the estimated coefficients as a function of the regularization parameter lambda
par(oma=c(0,0,3,0))
plot(lasso$glmnet.fit, xvar="lambda")
mtext('Number of Non-Zero Coefficients', side=3, line = 3)

# Generating some data 
# y = 1 + 2*x1 + 3*x2 + noise, where corr(x1, x2) = .5
# note the sample size is very large -- this isn't solved by big data!
x <- mvrnorm(100000, mu=c(0,0), Sigma=diag(c(.5,.5)) + 1)
y <- 1 + 2*x[,1] + 3*x[,2] + rnorm(100000)
data.sim <- data.frame(x=x, y=y)

print("Correct model")
lm(y ~ x.1 + x.2, data.sim)
print("Model with omitted variable bias")
lm(y ~ x.1, data.sim)

#we observe the path of the estimated coefficient on the number of 
#bathroooms (BATHS) as we increase  λ

# prepare data
fmla <- formula(paste0(outcome, "~", paste0(covariates, collapse="+")))
XX <- model.matrix(fmla, data)[,-1]  # [,-1] drops the intercept
Y <- data[,outcome]

# fit ols, lasso and ridge models
ols <- lm(fmla, data)
lasso <- cv.glmnet(x=XX, y=Y, alpha=1.)  # alpha = 1 for lasso
ridge <- cv.glmnet(x=XX, y=Y, alpha=0.)  # alpha = 0 for ridge

# retrieve ols, lasso and ridge coefficients
lambda.grid <- c(0, sort(lasso$lambda))
ols.coefs <- coef(ols)
lasso.coefs <- as.matrix(coef(lasso, s=lambda.grid))
ridge.coefs <- as.matrix(coef(ridge, s=lambda.grid))

# loop over lasso coefficients and re-fit OLS to get post-lasso coefficients
plasso.coefs <- apply(lasso.coefs, 2, function(beta) {
  
  # which slopes are non-zero
  non.zero <- which(beta[-1] != 0)  # [-1] excludes intercept
  
  # if there are any non zero coefficients, estimate OLS
  fmla <- formula(paste0(outcome, "~", paste0(c("1", covariates[non.zero]), collapse="+")))
  beta <- rep(0, ncol(XX) + 1)
  
  # populate post-lasso coefficients
  beta[c(1, non.zero + 1)] <- coef(lm(fmla, data))
  
  beta
})


selected <- 'BATHS'
k <- which(rownames(lasso.coefs) == selected) # index of coefficient to plot
coefs <- cbind(postlasso=plasso.coefs[k,],  lasso=lasso.coefs[k,], ridge=ridge.coefs[k,], ols=ols.coefs[k])
matplot(lambda.grid, coefs, col=1:4, type="b", pch=20, lwd=2, las=1, xlab="Lambda", ylab="Coefficient estimate")
abline(h = 0, lty="dashed", col="gray")

legend("bottomleft",
       legend = colnames(coefs),
       bty="n", col=1:4,   inset=c(.05, .05), lwd=2)

covs <- which(covariates %in% c('UNITSF', 'BEDRMS',  'DINING'))
matplot(lambda.grid, t(lasso.coefs[covs+1,]), type="l", lwd=2, las=1, xlab="Lambda", ylab="Coefficient estimate")
legend("topright", legend = covariates[covs], bty="n", col=1:p,  lty=1:p, inset=c(.05, .05), lwd=2, cex=.6)

# Fixing lambda. This choice is not very important; the same occurs any intermediate lambda value.
selected.lambda <- lasso$lambda.min
n.folds <- 10
foldid <- (seq(n) %% n.folds) + 1
coefs <- sapply(seq(n.folds), function(k) {
  lasso.fold <- glmnet(XX[foldid == k,], Y[foldid == k])
  as.matrix(coef(lasso.fold, s=selected.lambda))
})
heatmap(1*(coefs != 0), Rowv = NA, Colv = NA, cexCol = 1, scale="none", col=gray(c(1,0)), margins = c(3, 1), xlab="Fold", labRow=c("Intercept", covariates), main="Non-zero coefficient estimates")

# Number of data-driven subgroups.
num.groups <- 4

# Fold indices
n.folds <- 5
foldid <- (seq(n) %% n.folds) + 1

fmla <- formula(paste(" ~ 0 + ", paste0("bs(", covariates, ", df=3)", collapse=" + ")))

# Function model.matrix selects the covariates according to the formula
# above and expands the covariates accordingly. In addition, if any column
# is a factor, then this creates dummies (one-hot encoding) as well.
XX <- model.matrix(fmla, data)
Y <- data[, outcome]

# Fit a lasso model.
# Passing foldid argument so we know which observations are in each fold.
lasso <- cv.glmnet(x=XX, y=Y, foldid = foldid, keep=TRUE)

y.hat <- predict(lasso, newx = XX, s = "lambda.min")

# Ranking observations.
ranking <- lapply(seq(n.folds), function(i) {
  
  # Extract cross-validated predictions for remaining fold.
  y.hat.cross.val <- y.hat[foldid == i]
  
  # Find the relevant subgroup break points
  qs <- quantile(y.hat.cross.val, probs = seq(0, 1, length.out=num.groups + 1))
  
  # Rank observations into subgroups depending on their predictions
  cut(y.hat.cross.val, breaks = qs, labels = seq(num.groups))
})
ranking <- factor(do.call(c, ranking))

# Estimate expected covariate per subgroup
avg.covariate.per.ranking <- mapply(function(x.col) {
  fmla <- formula(paste0(x.col, "~ 0 + ranking"))
  ols <- lm(fmla, data=transform(data, ranking=ranking))
  t(lmtest::coeftest(ols, vcov=vcovHC(ols, "HC2"))[, 1:2])
}, covariates, SIMPLIFY = FALSE)

avg.covariate.per.ranking[1:2]
