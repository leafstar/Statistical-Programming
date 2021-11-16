#####  Karla Itzel Vega Ortega S2126801  #####
#####  Muxing Wang S2201749              #####
#####  Nutza Tokhadze S1778736           #####

# Task: to write an R function, bfgs, implementing the BFGS quasi-Newton minimization method.
# BFGS optimization function should operate broadly in the same way as optim or nlm

# Theta is a vector of initial values for the optimization parameters.
# f is the objective function to minimize Its first argument is the vector of optimization 
# parameters. Its second argument is a logical indicating whether or not gradients of the 
# objective w.r.t. the parameters should be computed
# Remaining arguments will be passed from bfgs using ...
# The scalar value returned by f will have a gradient attribute if the second argument to
# f is TRUE.
# tol the convergence tolerance
# fscale a rough estimate of the magnitude of f at the optimum - used in convergence testing
# maxit the maximum number of BFGS iterations to try before giving up

# RETURN INFORMATION 
# f the scalar value of the objective function at the minimum.
# theta the vector of values of the parameters at the minimum.
# iter the number of iterations taken to reach the minimum.
# g the gradient vector at the minimum (so the user can judge closeness to numerical zero).
# H the approximate Hessian matrix (obtained by finite differencing) at the minimum.

# The function should issue errors or warnings (using stop or warning as appropriate) in 
# the following cases. 
# 1. If the objective or derivatives are not finite at the initial theta; 
# 2. If the step fails to reduce the objective but convergence has not occurred 
# 3. If maxit is reached without convergence.
# 4. If the supplied f does not supply a gradient attribute when requested, then you 
# should compute the gradient by finite differencing

get_step_size <- function(f,theta,d){                    # Create a function called get_step_size with three inputs
  upper_bound = 10
  lower_bound = 0
  max.iter = 20                                          # Since we are doing a exponential decay, this bound is big enough for us to find a small step size.
  iter = 0                                               # We initialize the iter value 
  eps = 1e-7                                             
  c1 = 0.1                                               # for cond1
  c2 = 0.9    #we can discuss with simon about this since c2 is auto satisfied.
  alpha = 1
  theta.prime = theta + alpha*d
  g = get_grad(f,theta,eps)
  gamma = c1*sum(g*d) # for cond1 just as the video said
  while(f(theta.prime) >= f(theta) + alpha* gamma && iter <= max.iter){
    iter = iter + 1
    alpha = alpha/2
    theta.prime = theta + alpha*d
  }
  #print(iter)
  cat("alhpagamma",alpha* gamma,"\n")
  print(theta)
  
  cat("rhs",f(theta) + alpha* gamma,"\n")
  print(theta.prime)
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
  
  cat("optim val", f(theta.k), "\n")
  list(f=f, theta=theta.k, iter=iter, g=g.k, H=H)
}

#  end  of   implementation 
########################
########################

### just for test, will delete everything below this

bfgs(c(-1,2),rb)

f = beale
init = c(-1,1)
res1 = bfgs(init,f)
res2 = optim(init,f,g,method = "BFGS", hessian = TRUE)
res3 = nlm(f,init,hessian = TRUE)

cat("theta", res1$theta,"\n", res2$par, "\n", res3$estimate, "\n")

cat("hessian",res1$H,"\n", res2$hessian, "\n", res3$hessian,"\n")

cat("num_iteration",res1$iter, ",", res3$iterations)

cat(res2$value,",", res3$minimum,"\n")

source("./p4_functions.R")
