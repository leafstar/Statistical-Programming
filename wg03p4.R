# tol the convergence tolerance
# fscale a rough estimate of the magnitude of f at the optimum - used in convergence testing
# maxit the maximum number of BFGS iterations to try before giving up
# 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

get_step_size <- function(f,theta,d){
  upper_bound = 10
  lower_bound = 0
  max.iter = 30 # since we are doing a exponential decay, this bound is big enough for us to find a small step size.
  iter = 0
  eps = 1e-7
  c1 = 0.1 # for cond1
  c2 = 0.9 # we can discuss with simon about this since c2 is auto satisfied.
  alpha = 1
  theta.prime = theta + alpha*d
  g = get_grad(f,theta,eps)
  gamma = c1*sum(g*d) # for cond1 just as the video said
  while(f(theta.prime) > f(theta) + alpha* gamma && iter <= max.iter){
    iter = iter + 1
    alpha = alpha/2
    theta.prime = theta + alpha*d
  }
  
  ## we dont need these two lines guys, just to check if the ratio is less than c2.
  g.prime = get_grad(f, theta.prime, eps)
  print(g.prime %*% d /(g %*% d) <= c2)
  
  if(f(theta.prime) > f(theta)) warning("we cannot find a valid step size")
  
  alpha
}

get_grad <- function(f,theta,eps){
  if(is.null(attributes(f(theta, TRUE))$gradient)){
    ## finite differencing
    grad = theta
    for (i in 1:length(theta)) { ## loop over parameters
      theta1 <- theta; theta1[i] <- theta1[i] + eps ## increase th0[i] by eps nll1 <- nll(th1,t=t80,y=y) ## compute resulting nll
      grad[i] <- (f(theta1) - f(theta))/eps ## approximate -dl/dth[i]
    }
    as.vector(grad)
  }else{
    g.k = attributes(f(theta, TRUE))$gradient
  }
}

bfgs <- function(theta,f,...,tol=1e-5,fscale=1,maxit=100){
  theta.k = theta
  B.k <- I <- diag(length(theta))
  f0 = f(theta.k)
  eps = 1e-7 ## finite difference interval
  g.k = get_grad(f, theta.k, eps) ## initialize a random gradient

  if(any(is.infinite(g.k))) stop("gradient is not finite")
  if(is.infinite(f0)) stop("objective value is not finite")

  iter = 0

  while(max(abs(g.k)) >= (abs(f0)+fscale)*tol && iter <= maxit){
    f0 = f(theta.k)
    
    
    g.k = get_grad(f, theta.k, eps)
    
    
    d = -B.k %*% g.k

    
    alpha = get_step_size(f, theta.k, d)
    #print(alpha)   no need
    theta.kprime = as.vector(theta.k + alpha * d)

    g.kprime = get_grad(f, theta.kprime, eps)

    
    ## no need just checking the c2 condition
    print(g.kprime %*% d /(g.k %*% d))
    
    
    s.k = theta.kprime - theta.k
    y.k = g.kprime - g.k
    
    rho.k.i = sum(s.k*y.k)
    rho.k = 1/rho.k.i

    ## faster implementation
    B.kprime = B.k + ((rho.k.i+ as.vector(t(y.k)%*%B.k%*%y.k))*s.k %*% t(s.k))/(rho.k.i**2) - (B.k%*%y.k%*%t(s.k)+s.k%*%(t(y.k)%*%B.k))/rho.k.i
    
    theta.k = theta.kprime
    B.k = B.kprime
    iter = iter + 1
    
  }
  
  if(iter > maxit) warning("max iteration has been reached")
  
  
  ## fast way to compute H as B^-1
  H = chol2inv(chol(B.k))
  
  if (!isSymmetric(H)){
    H <- 0.5 * (t(H) + H)
  }
  
  list(f=f, theta=theta.k, iter=iter, g=g.k, H=H)
}

#  end  of   implementation 
########################
########################

### just for test, will delete everything below this
rb <- function(theta,getg=FALSE,k=10) {
  ## Rosenbrock objective function, suitable for use by ’bfgs’
  z <- theta[1]; x <- theta[2]
  f <- k*(z-x^2)^2 + (1-x)^2 + 1
  if (getg) {
    attr(f,"gradient") <- c(2*k*(z-x^2),
                            -4*k*x*(z-x^2) -2*(1-x))
  }
  f
} ## rb


### this one is that the gradient function is not provided.
rb2 <- function(theta,getg=FALSE,k=10) {
  ## Rosenbrock objective function, suitable for use by ’bfgs’
  z <- theta[1]; x <- theta[2]
  f <- k*(z-x^2)^2 + (1-x)^2 + 1
  f
} ## rb
bfgs(c(-1,2),rb)




camel3 <- function(xx,getg = FALSE)
{
  ##########################################################################

  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- 2*x1^2
  term2 <- -1.05*x1^4
  term3 <- x1^6 / 6
  term4 <- x1*x2
  term5 <- x2^2
  
  y <- term1 + term2 + term3 + term4 + term5
  return(y)
}

bfgs(c(-1,2),camel3)




