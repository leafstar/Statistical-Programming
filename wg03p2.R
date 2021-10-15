##                Group 3               ##
## Muxing Wang (s2201749)               ##
## Karla Itzel Vega Ortega (s2126801)   ##
## Nutsa Tokhadze (s1778736)            ##
##########################################


## inputs are the parameters and the output is the number of people in each state each day.
simulation<-function(n = 5.5e6, ne = 10, nt = 150, gamma = 1/3, delta = 1/5, lambda = 0.4/n){
  ## SEIR stochastic simulation model
  ## states: 0 -> S(Susceptible); 1 -> E(Exposed); 
  ##         2 -> I(Infective);   3 -> R(Recovery) 
  ## n = population size; ne =initial exposed; nt = number of days
  
  x <- rep(0,n) ## initialize to susceptible state
  beta <- rlnorm(n,0,0.5); beta <- beta/mean(beta) ## generate the contact rate
  
  initial_exposure = sample(1:n, 10) ## sample 10 people in E state
  x[initial_exposure] = 1
  S <- E <- I <- R <- rep(0, nt) ## set up storage for pop in each state
  S[1] = n - ne; E[1] = ne
  
  for (i in 2:nt){
    u <- runif(n) ## uniform random deviates
    exposed_prob = lambda * beta[x == 0] * sum(beta[x==2])
    x[x==2 & u<delta] <- 3 ## I -> R with prob delta
    x[x==1 & u<gamma] <- 2 ## E -> I with prob gamma
    x[x==0 & u<exposed_prob] <- 1 ## S -> E with prob 'exposed_prob'
    S[i] <- sum(x==0); E[i] <- sum(x==1)
    I[i] <- sum(x==2); R[i] <- sum(x==3)
  }
  list(S=S,E=E,I=I,R=R,beta=beta)
}

epi <- simulation() ## run simulation
hist(epi$beta,xlab="beta",main="") ## beta distribution
plot(log(epi$S),ylim=c(0,log(max(epi$S))),xlab="day",ylab="N") ## S black
points(log(epi$E),col=4);points(log(epi$I),col=2) ## E (blue) and I (red)

