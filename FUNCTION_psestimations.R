psestimations <- function(y,t,cluster,data){
  
  #splitting train and test into 70% train 30% test
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.6,0.4))
  data.train  <- data[sample, ]
  data.test   <- data[!sample, ]
  
  data.train = dataset1_full[train.sample,]
  data.test = dataset1_full[test.sample,]
  data.train = as.data.frame(data.train)
  data.test = as.data.frame(data.test)
  
  #test/train indicator
  data.train$train =1
  data.test$train=0
  
  
  #estimate ps using BART 
  require(bartCause)
  #run bart including teacher id to account for clustering effects
  bart = bartc(response= yij, 
               treatment= trt, 
               confounders= x1ij+x2ij+V1rep+V2rep+V3rep+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20, 
               parametric = (1|schoolid),#to indicate for the group effect, having trouble
               data = dataset1_full_trt2,
               #subset = 
               keepTrees = TRUE,
               method.rsp="bart", #
               method.trt="bart", #modeling the treatment assignment model
               estimand="ate")
  predict.pscore = bart$p.score
  predict.trt = bart$trt
  
  
  
   
}

names(as.data.frame(dataset1_full_trt2))

fomula <- paste0(names(dataset1_full_trt2)[3:20], collapse="+")
formula <- paste("trt2~",fomula,"+(1|schoolid)")
print(formula)
library(bartCause)
dataset1_full_trt2$schoolid<- (as.factor(dataset1_full_trt2$schoolid))
test1 <- bartc(response = yij1_trt2,
               treatment = trt2,
               confounders = as.matrix(dataset1_full_trt2[,3:20]),
               data = dataset1_full_trt2,
               #parametric = (1+1|schoolid),
               keepTrees = TRUE,
               method.trt = "bart",
               method.rsp = "p.weight",estimated="ate")
dataset1_full_trt2 <- as.data.frame(dataset1_full_trt2)
test1$p.score
names(dataset1_full_trt2)

library(stan4bart)
test2 <- stan4bart(formula = yij1_trt2 ~ trt2+ (1|schoolid), data = dataset1_full_trt2, verbose = -1,
                   chains = 1, iter =10, bart_args = list(n.trees =5))
?bart
dbart
class(dataset1_full_trt2$schoolid)
?stan4bart

fomu <- as.formula(yij1_trt2 ~ 
             x1ij + x2ij + V1rep + V2rep + V3rep +
             V8 + V9 + V10 + V11 + V12 + V13 + V14 + 
             V15 + V16 + V17 + V18 + V19 + V20 + (1|schoolid))

