##                Group 3               ##
## Muxing Wang (s2201749)               ##
## Karla Itzel Vega Ortega (s2126801)   ##
## Nutsa Tokhadze (s1778736)            ##
##########################################
## Address of github repo:.....
                                                          
                                                          ## Covid and the hazards of opportunistic sampling

## Our main task is to show what the daily infection trajectories look like for the population with the lowest individual transmission probabilities, and how
## they compare to the infection trajectories for the whole population. 
## For that purpose we split the population by 4 categories 1) Susceptible 2) Exposed 3) Infective and 4) Recovered. 
## (1) Start the epidemic by setting 10 randomly chosen people to the Exposed state. Simulated for 100 model days, recorded the number of new infections each day. 
## (2) Record the number of new infections among the 10% of the population with the lowest transmission rates. 
## (3) Record the number of new infections in a random sample of 0.1% of the population.
## (4) Produce a plot showing how the daily infection trajectories compare between the whole population, the ‘cautious 10%’ and the 0.1% random sample.
## (5) Finally write code to visualize the variability in the results from simulation to simulation, by running 10 replicate simulations, and plotting these.

   ## Step(1) Started the epidemic by setting 10 randomly chosen people to the Exposed state. Simulated for 100 days, recorded the number of new infections each day.
## Defined Stochastic Simulation Model function 
## inputs are the parameters and the output is the number of people in each state each day.
simulation<-function(n = 5.5e6, ne = 10, nt = 150, gamma = 1/3, delta = 1/5, lambda = 0.4/n){ 
  ## states: 0 -> S(Susceptible); 1 -> E(Exposed); 
  ##         2 -> I(Infective);   3 -> R(Recovery) 
  ## n = population size; ne =initial exposed; nt = number of days; 
  ## gamma=daily prob E->I, delta=daily prob I->R lambda=Overall viral infectivity parameter.
  
  x <- rep(0,n)                                                            ## initialize to susceptible state
  beta <- rlnorm(n,0,0.5); beta <- beta/mean(beta)                         ## generates contact rate with other people
  
  initial_exposure = sample(1:n, 10)                                       ## start with the sample 10 people in E state
  x[initial_exposure] = 1
  S <- E <- I <- R <- rep(0, nt)                                           ## set up storage for pop in each state
  S[1] = n - ne; E[1] = ne                                                 ## initialize 
  
  for (i in 2:nt){                                                         ## Loop over days
    u <- runif(n)                                                          ## uniform random deviates
    exposed_prob = lambda * beta[x == 0] * sum(beta[x==2])                 ## Set probability of uninfected person of being infected and entering (E) state
    x[x==2 & u<delta] <- 3                                                 ## I -> R with prob delta
    x[x==1 & u<gamma] <- 2                                                 ## E -> I with prob gamma
    x[x==0 & u<exposed_prob] <- 1                                          ## S -> E with prob 'exposed_prob'
    S[i] <- sum(x==0); E[i] <- sum(x==1)                                   ## record the number of new infections each day for each states
    I[i] <- sum(x==2); R[i] <- sum(x==3)
  }
  list(S=S,E=E,I=I,R=R,beta=beta)                                          
}

## Step (2) Recorded the number of new infections among the 10% of the population with the lowest transmission rates

   ## I think we should do this step somewhere in the end.
epi <- simulation() ## run simulation
hist(epi$beta,xlab="beta",main="") ## beta distribution
plot(log(epi$S),ylim=c(0,log(max(epi$S))),xlab="day",ylab="N") ## S black
points(log(epi$E),col=4);points(log(epi$I),col=2) ## E (blue) and I (red)

