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
  cautious_group = which(beta <= quantile(beta, 0.1)) ## 10% of the most cautious group
  random_group = sample(1:n, n/1000)
  initial_exposure = sample(1:n, ne) ## sample 10 people in E state
  x[initial_exposure] = 1
  S1 <- E1 <- I1 <- R1 <- rep(0, nt) ## set up storage for pop in each state
  S2 <- E2 <- I2 <- R2 <- rep(0, nt) ## set up storage for pop in each state
  S3 <- E3 <- I3 <- R3 <- rep(0, nt) ## set up storage for pop in each state
  S1[1] = n - ne; E1[1] = ne
  S2[1] = sum(x[cautious_group]==0); E2[1] <- sum(x[cautious_group]==1)
  S3[1] = sum(x[random_group]==0); E2[1] <- sum(x[random_group]==1)
  for (i in 2:nt){
    u <- runif(n) ## uniform random deviates
    exposed_prob = lambda * beta * sum(beta[x==2])

    
    x[x==2 & u<delta] <- 3 ## I -> R with prob delta
    x[x==1 & u<gamma] <- 2 ## E -> I with prob gamma
    x[x==0 & u<exposed_prob] <- 1 ## S -> E with prob 'exposed_prob'
    #length(x[x==0 & u<exposed_prob])
    
    S1[i] <- sum(x==0); E1[i] <- sum(x==1)
    I1[i] <- sum(x==2); R1[i] <- sum(x==3)
    
    S2[i] <- sum(x[cautious_group]==0); E2[i] <- sum(x[cautious_group]==1)
    I2[i] <- sum(x[cautious_group]==2); R2[i] <- sum(x[cautious_group]==3)
    
    S3[i] <- sum(x[random_group]==0); E3[i] <- sum(x[random_group]==1)
    I3[i] <- sum(x[random_group]==2); R3[i] <- sum(x[random_group]==3)
    
    
  }
  list(S1=S1,E1=E1,I1=I1,R1=R1,S2=S2,E2=E2,I2=I2,R2=R2,S3=S3,E3=E3,I3=I3,R3=R3,beta=beta)
}

epi <- simulation() ## run simulation
#hist(epi$beta,xlab="beta",main="") ## beta distribution
plot(log(epi$I1),ylim=c(0,log(max(epi$S1))),xlab="day",ylab="log(new infection)",cex=0.1, main = "infectious trend of 3 groups in log scale") ## S black
points(log(epi$I2),col=4,cex=0.1);points(log(epi$I3),col=2,cex=0.1)  ## E (blue) and I (red);
legend(1, 15, legend=c("whole population", "cautious group", "random 0.1% group"),
       col=c("black", "blue","red"), lty=1:2, cex=0.8)
which(epi$I1 == max(epi$I1))
which(epi$I2 == max(epi$I2))
which(epi$I3 == max(epi$I3))
