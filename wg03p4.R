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

get_step_size<- function(f,theta,d){                        
  alpha = 1
  theta.prime = theta + alpha*d
  g = attributes(f(theta, TRUE))$gradient
  g.prime = attributes(f(theta.prime, TRUE))$gradient
  while(f(theta.prime) >= f(theta) ){
    alpha = alpha/2
    theta.prime = theta + alpha*d
  }
  #|| g.prime %*% d /(g %*% d)
  alpha
}

bfgs <- function(theta,f,...,tol=1e-5,fscale=1,maxit=100){
  
  theta.k = theta
  B.k <- I <- diag(length(theta))
  f0 = f(theta.k)
  g.k = attributes(f(theta.k, TRUE))$gradient
  iter = 0
  while(max(abs(g.k)) >= (abs(f0)+fscale)*tol && iter <= maxit){
    f0 = f(theta.k)
    g.k = attributes(f(theta.k, TRUE))$gradient
    
    
    
    d = -B.k %*% g.k
    
    #cond2(rb,theta.k,d)
    
    alpha = get_step_size(f, theta.k, d)
    
    theta.kprime = theta.k + alpha * d
    g.kprime = attributes(f(theta.kprime, TRUE))$gradient
    print(g.kprime %*% d /(g.k %*% d))
    s.k = theta.kprime - theta.k
    y.k = g.kprime - g.k
    rho.k = 1/sum(s.k*y.k)
    
    B.kprime = (I - rho.k* s.k %*% t(y.k)) %*% B.k %*% (I - rho.k * y.k %*% t(s.k) ) + rho.k * s.k %*% t(s.k)
    theta.k = theta.kprime
    B.k = B.kprime
    iter = iter + 1
    
  }
  list(f=f, theta=theta.k, iter=iter, g=g.k, H=B.k)
}


rb <- function(theta,getg=FALSE,k=10) {
  ## Rosenbrock objective function, suitable for use by Ã¢ÂÂbfgsÃ¢ÂÂ
  z <- theta[1]; x <- theta[2]
  f <- k*(z-x^2)^2 + (1-x)^2 + 1
  if (getg) {
    attr(f,"gradient") <- c(2*k*(z-x^2),
                            -4*k*x*(z-x^2) -2*(1-x))
  }
  f
} ## rb

bfgs(c(-1,2),rb)


## I use this function to explore the relationship between step size and the wolfe conditionn2.
## you may ignnore this.
cond2<- function(f,theta,d){
  alpha = seq(0,1,0.001)
  ratio = c()
  for (a in alpha){
    theta.prime = theta + a*d
    g = attributes(f(theta, TRUE))$gradient
    g.prime = attributes(f(theta.prime, TRUE))$gradient
    ratio = c(ratio, g.prime %*% d /(g %*% d))
  }
  plot(alpha,ratio)
}
cond2(rb,c(-1,2),-diag(2) %*% res$g)
