## -----------------------------------------------------------------------------
## Script04A-GoodnessOfFit
## This script contains GOF Functions. 

## -----------------------------------------------------------------------------
GOF_pvalue_global <- function(gam.fit, set_covariates, BB.stat,
                              X=NULL, mu=NULL, w=NULL){
  
  if (is.null(X)){
    # Extract the design matrix from the GAM fit
    X <- model.matrix(gam.fit)
    # Get the number of observations
    n.e <- nrow(X)
  }
  if (is.null(mu)){
    # Calculate the mean response values
    mu <- gam.fit$fitted.values
  }
  if (is.null(w)){
    # Compute the working residuals
    w <- gam.fit$family$mu.eta(gam.fit$linear.predictors)*
      (gam.fit$y - mu)/(gam.fit$sig2*gam.fit$family$variance(mu))
  }
  
  # Compute the weights for the process
  wt <- sqrt(mu*(1-mu))
  
  # Initialize an empty vector to store maximum statistics
  max_s <- NULL
  
  for (index in set_covariates){
    
    # Ensure index is unlisted
    index = unlist(index)
    
    # Determine the dimension of the covariate set
    dim.k = length(index)
    
    # Extract the columns corresponding to the covariate set from the design
    dim.spl = index
    
    if (dim.k == 1){
      # Psi for univariate covariates is a vector
      Psi <- X[,dim.spl] * w * wt
      Psi.last <- sum(Psi)
      Psi.tilde <- Psi - Psi.last/n.e
      W <- cumsum(Psi.tilde)/sqrt(n.e)
      # Inverse Variance of Psi
      Vs <- ginv(crossprod(Psi.tilde)/(n.e))
      # Squared Inverse Variance of Psi
      E <- eigen(Vs)
      P <- E$vectors
      D.val <- pmax(E$value, 0)
      D.sq <- (sqrt(D.val))
      B.inv.sq <- (P%*%D.sq%*%t(P))
      
    } else {
      # Psi for multivariate covariates is a matrix
      Psi <- X[,dim.spl] * w * wt
      Psi.last <- matrix(apply(Psi, 2, sum), ncol=dim.k)
      Psi.tilde <- Psi - apply(Psi.last, 2, 
                               function(x) x * rep(1/n.e, n.e))
      W <- apply(Psi.tilde, 2, cumsum)/sqrt(n.e)
      # Inverse Variance of Psi
      Vs <- ginv(crossprod(Psi.tilde)/(n.e))
      # Squared Inverse Variance of Psi
      E <- eigen(Vs)
      P <- E$vectors
      D.val <- pmax(E$value, 0)
      D.sq <- diag(sqrt(D.val))
      B.inv.sq <- (P%*%D.sq%*%t(P))
    }
    
    # Standardized Martingale-Residual Process
    efp <- t(B.inv.sq %*% t(W))
    # Compute the maximum statistics
    U.n <- apply(efp, 1, function(x) crossprod(x,x))
    max_s <- c(max_s, max(abs(U.n))/dim.k)
  }
  
  # Compute the test statistic
  stat = max(max_s)
  # Compute the p-value
  pvalue = mean(BB.stat>=stat)
  
  return(pvalue)
}

## -----------------------------------------------------------------------------
GOF_univariate <- function(gam.fit, index,
                           X=NULL, mu=NULL, w=NULL){
  
  if (is.null(X)){
    # Extract the design matrix from the GAM fit
    X <- model.matrix(gam.fit)
    # Get the number of observations
    n.e <- nrow(X)
  }
  if (is.null(mu)){
    # Calculate the mean response values
    mu <- gam.fit$fitted.values
  }
  if (is.null(w)){
    # Compute the working residuals
    w <- gam.fit$family$mu.eta(gam.fit$linear.predictors)*
      (gam.fit$y - mu)/(gam.fit$sig2*gam.fit$family$variance(mu))
  }
  
  # Psi process 
  Psi <- X[,index] * w
  W <- cumsum(Psi)/sqrt(n.e)
  # Inverse Variance of Psi
  Vs <- ginv(crossprod(Psi)/(n.e))
  # Squared Inverse Variance of Psi
  E <- eigen(Vs)
  P <- E$vectors
  D.val <- pmax(E$value, 0)
  D.sq <- (sqrt(D.val))
  B.inv.sq <- (P%*%D.sq%*%t(P))
  
  # Standardized Martingale-Residual Process
  efp <- t(B.inv.sq %*% t(W))
  
  # Compute the p-value
  pvalue = 1 - kolm(max(abs(efp)))
  
  return(list(pvalue, efp))
}

## -----------------------------------------------------------------------------
GOF_multivariate <- function(gam.fit, index, BB.stat,
                             X=NULL, mu=NULL, w=NULL){
  
  if (is.null(X)){
    # Extract the design matrix from the GAM fit
    X <- model.matrix(gam.fit)
    # Get the number of observations
    n.e <- nrow(X)
  }
  if (is.null(mu)){
    # Calculate the mean response values
    mu <- gam.fit$fitted.values
  }
  if (is.null(w)){
    # Compute the working residuals
    w <- gam.fit$family$mu.eta(gam.fit$linear.predictors)*
      (gam.fit$y - mu)/(gam.fit$sig2*gam.fit$family$variance(mu))
  }
  
  # Compute the weights for the process
  wt <- sqrt(mu*(1-mu))
  
  # Determine the dimension of the covariate set
  dim.k = length(index)
  
  # Extract the columns corresponding to the covariate set from the design
  dim.spl = index
  
  # Psi for multivariate covariates is a matrix
  Psi <- X[,dim.spl] * w * wt
  Psi.last <- matrix(apply(Psi, 2, sum), ncol=dim.k)
  Psi.tilde <- Psi - apply(Psi.last, 2, 
                           function(x) x * rep(1/n.e, n.e))
  W <- apply(Psi.tilde, 2, cumsum)/sqrt(n.e)
  # Inverse Variance of Psi
  Vs <- ginv(crossprod(Psi.tilde)/(n.e))
  # Squared Inverse Variance of Psi
  E <- eigen(Vs)
  P <- E$vectors
  D.val <- pmax(E$value, 0)
  D.sq <- diag(sqrt(D.val))
  B.inv.sq <- (P%*%D.sq%*%t(P))
  
  # Standardized Martingale-Residual Process
  efp <- t(B.inv.sq %*% t(W))
  
  # Compute the maximum statistics
  U.n <- apply(efp, 1, function(x) crossprod(x,x))
  stat <- max(abs(U.n))
  
  # Compute the p-value
  pvalue = mean(BB.stat>=stat)
  
  return(list(pvalue, efp))
}

## -----------------------------------------------------------------------------
BB.simulator <- function(set_covariates, n = 2000, nsim = 2000) {
  # Initialize an empty vector to store maximum values
  Bridge.data <- replicate(nsim, {
    max_s <- NULL
    # Loop through each set of covariates
    for (index in set_covariates) {
      
      # Ensure index is unlisted
      index = unlist(index)
      
      # Determine the dimension of the covariate set
      dim.k = length(index)
      
      # Generate n Brownian Bridge paths for each covariate
      BB.data <- replicate(dim.k, BBridge(0, 0, N = n - 1))
      
      # Calculate the process (squared norm) for each path and 
      # store the maximum value divided by the dimension
      process <- apply(BB.data, 1, function(x) crossprod(x, x))
      max_s <- c(max_s, max(process) / dim.k)
      
    }
    # Return the maximum value across all covariate sets
    return(max(max_s))
  })
  
  # Return the vector of maximum values
  return(Bridge.data)
}

## -----------------------------------------------------------------------------
BB.single <- function(dim.k, n.sim=5000, n=2000){
  Bridge.data <- replicate(n.sim, 
                           {
                             BB.data <- replicate(dim.k, BBridge(0,0,N=n-1))
                             process <- apply(BB.data, 1, function(x) crossprod(x,x))
                             return(process)
                           })
  BB.stat <- apply(Bridge.data, 2, function(x) max(abs(x)))
  return(list(Bridge.data, BB.stat))
}

## -----------------------------------------------------------------------------
f <- function(x,i) {exp(-(2*i-1)^2*pi^2/(8*x^2)) }
kolm <- function(x) {sqrt(2*pi)/x*(f(x,1)+f(x,2)+f(x,3)+f(x,4)+
                                     f(x,5)+f(x,6)+f(x,7)+f(x,8)+
                                     f(x,9)+f(x,10))}