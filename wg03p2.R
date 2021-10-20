##                Group 3               ##
## Muxing Wang (s2201749)               ##
## Karla Itzel Vega Ortega (s2126801)   ##
## Nutsa Tokhadze (s1778736)            ##
##########################################
## Address of github repo: https://github.com/leafstar/Statistical-Programming

                                                   ## Covid and the hazards of opportunistic sampling ##

## The Covid pandemic is an ongoing global pandemic already two years. One of the main features of the Covid-19 epidemic in the UK and 
## generally in the world is that the estimates of the number of new cases per day are not always based on properly sampled statistical 
## surveys.Thus, estimates are under question. The main purpose of this project is to show what the daily infection trajectories look like 
## for the population with the lowest individual transmission probabilities, and how they compare to the infection trajectories for the 
## whole population in the UK. For that purpose we split the population by 4 categories 1) Susceptible 2) Exposed 3) Infective and 4) 
## Recovered and do the following steps:
## (1) Show daily infection trajectories for three groups 1) Whole population 2) Cautious group - 10% of the population with the lowest 
## individual transmission probabilities and 3) Random sample of 0.1% of the population n=5.5e6.
## (2) Produce a plot showing how the daily infection trajectories compare between the whole population,the ‘cautious 10%’ and the 0.1% 
## random sample. 
## (3) Run 10 replicate simulations for each groups.
## (4) Produce plots for 10 replicate simulations showing daily infection trajectories for each groups and their variability.


## Step (1):Show daily infection trajectories for three groups 1) Whole population 2) Cautious group - 10% of the population with the 
## lowest individual transmission probabilities and 3)Random sample of 0.1% of the population n=5.5e6. 
## Defined Stochastic Simulation Model function; Started the epidemic by setting 10 randomly chosen people to the Exposed state and 
## Simulated for 150 days. Inputs are the parameters and the output is the number of people in each state each day.
simulation<-function(n = 5.5e6, ne = 10, nt = 150, gamma = 1/3, delta = 1/5, lambda = 0.4/n){ 
  ## states: 0 -> S(Susceptible); 1 -> E(Exposed); 
  ##         2 -> I(Infective);   3 -> R(Recovery) 
  ## n = population size of 5.5e6; ne =initial exposed 10 randomly chosen people; nt = number of days: 150; 
  ## gamma=daily prob E->I, delta=daily prob I->R lambda = Overall viral infectivity parameter.
                      
  beta <- rlnorm(n,0,0.5); beta <- beta/mean(beta)               ## generate contact rate with other people
  cautious_group = which(beta <= quantile(beta, 0.1))            ## 10% of the most cautious group
  random_group = sample(1:n, n/1000)                             ## create random sample of 0.1% of the population
  initial_exposure = sample(1:n, ne)                             ## start with a sample of 10 people in E state
  I1 <- I2 <- I3 <- rep(0, nt)                                   ## set up storage for number of infective people for the 3 groups
  S_ind = 1:n; E_ind = initial_exposure; I_ind = c(); R_ind = c(); 
  for (i in 2:nt){                                               ## loop over 150 days
    u_stoe <- runif(length(S_ind))                               ## uniform random deviates
    u_etoi <- runif(length(E_ind)) 
    u_itor <- runif(length(I_ind)) 
    exposed_prob = lambda * beta * sum(beta[I_ind])              ## daily chance of a transmission between infected and uninfected
                                    
    R_ind = c(R_ind, I_ind[u_itor< delta]); I_ind = I_ind[! I_ind %in% R_ind]     ## Change from state I -> R with prob delta
    I_ind = c(I_ind, E_ind[u_etoi< gamma]); E_ind = E_ind[! E_ind %in% I_ind]     ## Change from state E -> I with prob gamma
    E_ind = c(E_ind, S_ind[u_stoe<exposed_prob[S_ind]]); S_ind = S_ind[! S_ind %in% E_ind]  ## Change from state S -> E with prob 'exposed_prob'
    
    ## Record of the number of new infections each day for the 3 chosen groups

    I1[i] <- length(I_ind);                                        ## Record of Infective for total population
    I2[i] <- sum(I_ind %in% cautious_group);                       ## Record of Infective for cautious group
    I3[i] <- sum(I_ind %in% random_group);                         ## Record of Infective for random sample of 0.1% of the population
  }
  list(I1=I1,I2=I2,I3=I3,beta=beta)
}

#Step (2):Produce a plot showing how the daily infection trajectories compare between the whole population, the ‘cautious 10%’ and the 
## 0.1% random sample.
a<-Sys.time()
epi <- simulation()
b<-Sys.time()
print(b-a)
## run simulation
peak1 = which(epi$I1 == max(epi$I1))                               ## Day of the maximum Infected people in the whole population
peak2 = which(epi$I2 == max(epi$I2))                               ## Day of the maximum Infected people in the 10% population
peak3 = which(epi$I3 == max(epi$I3))                               ## Day of the maximum Infected people in the 0.1% of the population
plot(epi$I1/max(epi$I1),ylim=c(0,1.1),xlab="day",ylab="standardized quantity",
     lwd=3, main = "Infectious trend of 3 groups",type = "l") 
lines(1:150,epi$I2/max(epi$I2),col=4,lwd=3)
lines(1:150,epi$I3/max(epi$I3),col=2,lwd=3)                   
legend(0, 1, legend=c("whole population", "cautious group", "random 0.1% group"),
       col=c("black", "blue","red"), lty=1, lwd=3,cex =0.75)       ## E (blue)  I (red) S black;
text(peak1-10,1, peak1, col = 1)                                   ## Label in the peak day of the whole population
text(peak2+10,1, peak2, col = 4)                                   ## Label in the peak day for the 10% of the population
text(peak3,1.1, peak3, col = 2)                                    ## Label in the peak day for the 0.1% of population
text(40,0.4, "the colored numbers \nare the peak days")

##Step (3):Run 10 replicate simulations for each groups
## the aim of this loop is to generate the 10 simulations and then create a vector that stores the 10 outputs
epi10=list()                                                       ## Create an empty list to store the values of the simulation
for (i in 1:10){                                                   ## indicate in the for loop that the simulation has to run 10 times
  epi10[[length(epi10)+1]] <- simulation()                         ## Store the outputs in a vector called epi10
}

##Step (4):Produce plots for 10 replicate simulations showing daily infection trajectories for:1) the whole population, 
## 2) the ‘cautious 10%’ and 3) the 0.1% random sample.
#These plots show 10 simulations for above discussed three groups. 
#The peaks in the graph indicates the day of the maximum number of infected people.
xlim = 150                                                         ## x limit for the graph

##(4.1) Plot for the whole population
plot(log(epi10[[1]]$I1),ylim=c(0,log(max(epi10[[1]]$I1))),
     xlab="day",ylab="log(new infection)",cex=0.1, main = "10 simulations for 3 groups",type="l")
for (i in 2:10) {
  lines(1:xlim,log(epi10[[i]]$I1), col = 1)
}

##(4.2) Plot for the cautious group-10%
for (i in 1:10) {
  lines(1:xlim,log(epi10[[i]]$I2), col = 3)
}

##(4.3) Plot for the random sample of 0.1% of the population 
for (i in 1:10) {
  lines(1:xlim,log(epi10[[i]]$I3), col = 2)
}
legend(0, 1, legend=c("whole population", "cautious group", "random 0.1% group"),
       col=c("black", "blue","red"), lty=1, lwd=3,cex =0.75)           

                                                                ## Conclusion ##

## After analyzing daily infection trajectories in the UK for three different groups- whole populations, cautious group, and 0.1% random 
## sample of the whole population, we can conclude that for the whole population and random group of 0.1%, infection peaks faster than 
## for the cautious ones. For 10% of the population with the lowest individual transmission probabilities infection peaks later. Therefore, 
## we think that the cautious group doesn’t track the whole population trajectory while the results from the 0.1% random group are more 
## representative. This conclusion can be valid for the ZOE symptom tracker app results as well, which shows the similar patterns as 
## REACT-2 and ONS (which use random samples) but albeit with the timings not exactly matching. Hence, the reason for this mismatch might 
## be the fact that the ZOE sample is not a random sample of the UK population like a cautious group in our case. 


