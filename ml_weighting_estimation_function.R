psfunction <- function(y, w, cluster, X,inits=NULL){
  
}


#iptw function for ATE 


iptw <- function(y, t, w, ps_wgt) {
  eps <- 1e-6 
  ps_wgt[!is.finite(ps_wgt)] <- eps/1e3 
  N <- length(w)
  m1 <- sum(ps_wgt*t*w*y)/sum(ps_wgt*t*w)
  m0 <- sum(ps_wgt*(1-t)*w*y)/sum(ps_wgt*1(1-t)*w)
  ate <- m1 - m0 
  return(ate)
}

#' t: vector if trt indicator 
#' probs: propensity scores 
#' y: vector of outcome variable
#' cluster: vector of cluster indicator 
#' w: optional vector of sampling weights 
#' type: type of weighting estimator, can be IPW or BA 
#' trim: optional value for trimming probabilities
ml_ipsw <- function(t, probs, y, cluster, w=NULL, type="IPW", trim=0)
{
  ind <- (probs >= trim ) & ( probs <= 1-trim)
  t <- t[ind]
  probs <- probs[ind]
  y <- y[ind]
  cluster <- cluster[ind]

  if (is.null(w)){
    w <- rep(1,length(t))
  }
  
  if (type=="IPW"){
    weights <- ifelse(t == 1, 1 / probs, 1 / ( 1 - probs ) )    
  }
  if (type=="BA"){
    weights <- ifelse(t == 1, 1 - probs, probs  )        
  }
  weights <- weights * w
  
  #* marginal estimator
  ind1 <- which(t==1)
  m1 <- sum( y[ind1]*weights[ind1]) / sum(weights[ind1] )
  ind0 <- which(t==0)
  m0 <- sum( y[ind0]*weights[ind0]) / sum(weights[ind0] )
  pi_ma <- m1 - m0    
  
  #* clustered estimator
  a1 <- aggregate( y * weights*(t==1) , list(cluster), sum )
  colnames(a1) <- c("cluster", "sum_y1")
  a1$sum_w1 <- aggregate( weights*(t==1) , list(cluster), sum )[,2]
  a1$sum_y0 <- aggregate( y * weights*(t==0), list(cluster), sum )[,2]
  a1$sum_w0 <- aggregate( weights*(t==0), list(cluster), sum )[,2]
  a1$n_treat <- aggregate((t==1), list(cluster), sum )[,2]    
  a1$pi_h <- a1$sum_y1 / a1$sum_w1 - a1$sum_y0 / a1$sum_w0
  a1$w_h <- aggregate( weights , list(cluster), sum )[,2]
  a1 <- a1[ ! is.na(a1$pi_h) , ]
  pi_cl <- sum( a1$pi_h * a1$w_h ) / sum( a1$w_h )        
  # pi_clu <- mean( a1$pi_h )
  #--- output
  res <- c( pi_ma = pi_ma, pi_cl=pi_cl) 
  #' the IPW estimator is the value pi_ma 
  #' the cluster estimator is the value pi_cl
  return(res)
}

res1 <- ml_ipsw(t = dataset1_full_trt2$trt2,probs = prob2, y=dataset1_full_trt2$yij1_trt2, cluster = as.factor(dataset1_full_trt2$schoolid),
                type = "IPW")
res1
iptw1 <- iptw(t = dataset1_full_trt2$trt2,y=dataset1_full_trt2$yij1_trt2,)



