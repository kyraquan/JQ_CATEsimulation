#classification trees 
#continued afrer ML causal tutorial.R
# Fit tree without pruning first

fmla <- formula(paste(outcome, "~", paste(covariates, collapse=" + ")))
tree <- rpart(fmla, data=data, cp=0, method="anova")  # use method="class" for classification
plot(tree, uniform=TRUE)

plotcp(tree)

# Retrieves the optimal parameter
cp.min <- which.min(tree$cptable[,"xerror"]) # minimum error
cp.idx <- which(tree$cptable[,"xerror"] - tree$cptable[cp.min,"xerror"] < tree$cptable[,"xstd"])[1]  # at most one std. error from minimum error
cp.best <- tree$cptable[cp.idx,"CP"]

# Prune the tree
pruned.tree <- prune(tree, cp=cp.best)

# Retrieves the optimal parameter
cp.min <- which.min(tree$cptable[,"xerror"]) # minimum error
cp.idx <- which(tree$cptable[,"xerror"] - tree$cptable[cp.min,"xerror"] < tree$cptable[,"xstd"])[1]  # at most one std. error from minimum error
cp.best <- tree$cptable[cp.idx,"CP"]

# Prune the tree
pruned.tree <- prune(tree, cp=cp.best)
plot(pruned.tree, uniform=TRUE, margin = .05)
text(pruned.tree, cex=.7)

# Retrieve predictions from pruned tree
y.hat <- predict(pruned.tree)

# Compute mse for pruned tree (using cross-validated predictions)
mse.tree <- mean((xpred.rpart(tree)[,cp.idx] - data[,outcome])^2, na.rm=TRUE)
print(paste("Tree MSE estimate (cross-validated):", mse.tree))


y.hat <- predict(pruned.tree)

# Number of leaves should equal the number of distinct prediction values.
# This should be okay for most applications, but if an exact answer is needed use
# predict.rpart.leaves from package treeCluster
num.leaves <- length(unique(y.hat))

# Leaf membership, ordered by increasing prediction value
leaf <- factor(y.hat, ordered = TRUE, labels = seq(num.leaves))

# Looping over covariates
avg.covariate.per.leaf <- mapply(function(covariate) {
  
  # Coefficients on linear regression of covariate on leaf 
  #  are the average covariate value in each leaf.
  # covariate ~ leaf.1 + ... + leaf.L 
  fmla <- formula(paste0(covariate, "~ 0 + leaf"))
  ols <- lm(fmla, data=transform(data, leaf=leaf))
  
  # Heteroskedasticity-robust standard errors
  t(coeftest(ols, vcov=vcovHC(ols, "HC2"))[,1:2])
}, covariates, SIMPLIFY = FALSE)

print(avg.covariate.per.leaf[1:2])  # Showing only first few

df <- mapply(function(covariate) {
  # Looping over covariate names
  # Compute average covariate value per ranking (with correct standard errors)
  fmla <- formula(paste0(covariate, "~ 0 + leaf"))
  ols <- lm(fmla, data=transform(data, leaf=leaf))
  ols.res <- coeftest(ols, vcov=vcovHC(ols, "HC2"))
  
  # Retrieve results
  avg <- ols.res[,1]
  stderr <- ols.res[,2]
  
  # Tally up results
  data.frame(covariate, avg, stderr, 
             ranking=factor(seq(num.leaves)), 
             # Used for coloring
             scaling=pnorm((avg - mean(avg))/sd(avg)), 
             # We will order based on how much variation is 'explain' by the averages
             # relative to the total variation of the covariate in the data
             variation=sd(avg) / sd(data[,covariate]),
             # String to print in each cell in heatmap below
             # Note: depending on the scaling of your covariates, 
             # you may have to tweak these  formatting parameters a little.
             labels=paste0(formatC(avg),"\n(", formatC(stderr, digits = 2, width = 2), ")"))
}, covariates, SIMPLIFY = FALSE)
df <- do.call(rbind, df)

# a small optional trick to ensure heatmap will be in decreasing order of 'variation'
df$covariate <- reorder(df$covariate, order(df$variation))
df <- df[order(df$variation, decreasing=TRUE),]

# plot heatmap
ggplot(df[1:(8*num.leaves),]) +  # showing on the first few results (ordered by 'variation')
  aes(ranking, covariate) +
  geom_tile(aes(fill = scaling)) + 
  geom_text(aes(label = labels), size=2.5) +  # 'size' controls the fontsize inside cell
  scale_fill_gradient(low = "#E1BE6A", high = "#40B0A6") +
  ggtitle(paste0("Average covariate values within leaf")) +
  theme_minimal() + 
  ylab("") + xlab("Leaf (ordered by prediction, low to high)") +
  labs(fill="Normalized\nvariation") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = .5),
        axis.title.x = element_text(size=9),
        legend.title = element_text(hjust = .5, size=9))
