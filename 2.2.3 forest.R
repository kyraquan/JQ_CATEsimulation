#forests
#continued afrer ML causal tutorial.R, 2.2.2 trees.R

X <- data[,covariates]
Y <- data[,outcome]

# Fitting the forest
# We'll use few trees for speed here. 
# In a practical application please use a higher number of trees.
forest <- regression_forest(X=X, Y=Y, num.trees=200)  

# There usually isn't a lot of benefit in tuning forest parameters, but the next code does so automatically (expect longer training times)
# forest <- regression_forest(X=X, Y=Y, tune.parameters="all")

# Retrieving forest predictions
y.hat <- predict(forest)$predictions

# Evaluation (out-of-bag mse)
mse.oob <- mean(predict(forest)$debiased.error)
print(paste("Forest MSE (out-of-bag):", mse.oob))

var.imp <- variable_importance(forest)
names(var.imp) <- covariates
sort(var.imp, decreasing = TRUE)[1:10] # showing only first few
