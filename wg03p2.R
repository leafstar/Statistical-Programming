##                Group 3               ##
## Muxing Wang (s2201749)               ##
## Karla Itzel Vega Ortega (s2126801)   ##
## Nutsa Tokhadze (s1778736)            ##
##########################################
## Address of github repo: https://github.com/leafstar/Statistical-Programming.git
                                                          
                    ####################################################################################
                    ################ Covid and the hazards of opportunistic sampling ###################
                    ####################################################################################

## The Covid pandemic is an ongoing global pandemic already two years. One of the main features of the Covid-19 epidemic in the UK and generally in the world 
## is that the estimates of the number of new cases per day are not always based on properly sampled statistical surveys. Thus, estimates are under question. 
## The main purpose of this assignment is to show what the daily infection trajectories look like for the population with the lowest individual transmission 
## probabilities, and how they compare to the infection trajectories for the whole population in the UK. 
## For that purpose we split the population by 4 categories 1) Susceptible 2) Exposed 3) Infective and 4) Recovered and do following steps:
  ## (1) Show daily infection trajectories for three groups 1) Whole population 2) Cautious group - 10% of the population with the lowest probability transmission
  ## rates and 3) Random sample of 0.1% of the populatio n.
  ## (2) Produce a plot showing how the daily infection trajectories compare between the whole population, the ‘cautious 10%’ and the 0.1% random sample. 
  ## (3) Run 10 replicate simulations for each groups.
  ## (4) Produce three plots for 10 replicate simulations showing daily infection trajectories for each groups and their variability.

## Defined Stochastic Simulation Model function
## Started the epidemic by setting 10 randomly chosen people to the Exposed state and Simulated for 150 days .
## inputs are the parameters and the output is the number of people in each state each day.



  ## (1) Show daily infection trajectories for three groups 1) Whole population 2) Cautious group - 10% of the population with the lowest probability transmission
  ## rates and 3) Random sample of 0.1% of the populatio  n.

simulation<-function(n = 5.5e6, ne = 10, nt = 150, gamma = 1/3, delta = 1/5, lambda = 0.4/n){ 
  ## states: 0 -> S(Susceptible); 1 -> E(Exposed); 
  ##         2 -> I(Infective);   3 -> R(Recovery) 
  ## n = population size of 5.5e6; ne =initial exposed 10 randomly chosen people; nt = number of days: 150; 
  ## gamma=daily prob E->I, delta=daily prob I->R lambda=Overall viral infectivity parameter.
  
  x <- rep(0,n)  ## initialize to susceptible state the whole population
  
  beta <- rlnorm(n,0,0.5); beta <- beta/mean(beta)                ## generates contact rate with other people
  cautious_group = which(beta <= quantile(beta, 0.1))             ## 10% of the most cautious group
  random_group = sample(1:n, n/1000)                              ## creat random sample of 0.1% of the population.
  initial_exposure = sample(1:n, ne)                              ## start with a sample of 10 people in E state, this assignment will be ramdom
  x[initial_exposure] = 1                                         ## The exposed group is going to be inserted to the initialize vector
  I1 <- I2 <- I3 <- rep(0, nt)                                    ## set up storage for number of infective people for the 3 groups
  for (i in 2:nt){                                                ## loop over the 150 days
    u <- runif(n)                                                 ## uniform random deviates
    exposed_prob = lambda * beta * sum(beta[x==2])                ## daily chance of a transmission between infected person i and uninfected person
    x[x==2 & u<delta] <- 3                                        ## Change from state I -> R with prob delta
    x[x==1 & u<gamma] <- 2                                        ## Change from state E -> I with prob gamma
    x[x==0 & u<exposed_prob] <- 1                                 ## Change from state S -> E with prob 'exposed_prob'

    ## Record of the number of new infections (as x == 2) each day for the 3 chosen groups

    I1[i] <- sum(x==2);                                           ## Record of Infective for total population
    I2[i] <- sum(x[cautious_group]==2);                           ## Record of Infective for cautiouse group
    I3[i] <- sum(x[random_group]==2);                             ## Record of Infective for random sample of 0.1% of the population
  }
  list(I1=I1,I2=I2,I3=I3,beta=beta)
}

  #Step (2):Produce a plot showing how the daily infection trajectories compare between the whole population, the ‘cautious 10%’ and the 0.1% random sample.

epi <- simulation()                                               ## run simulation, the argument are already stated in line 30
peak1 = which(epi$I1 == max(epi$I1))                              ## Day of the maximun Infected people in the whole population
peak2 = which(epi$I2 == max(epi$I2))                              ## Day of the maximun Infected people in the 10% population
peak3 = which(epi$I3 == max(epi$I3))                              ## Day of the maximun Infected people in the 0.1% of the population
plot(log(epi$I1),ylim=c(0,log(max(epi$I1))+5),xlab="day",ylab="log(new infection)",cex=0.1, main = "infectious trend of 3 groups in log scale") 
points(log(epi$I2),col=4,cex=0.1);points(log(epi$I3),col=2,cex=0.1)                   ## E (blue)  I (red) S black;
legend(0, 15, legend=c("whole population", "cautious group", "random 0.1% group"),
       col=c("black", "blue","red"), lty=1:2, cex=0.8)
text(peak1,log(max(epi$I1)),peak1)                                ##Label in the peak day of the whole population
text(peak2,log(max(epi$I2)),peak2)                                ##Label in the peak day for the 10% of the population
text(peak3,log(max(epi$I3)),peak3)                                ##Label in the peak day for the 0.1% of population

  ##Step (3):Run 10 replicate simulations for each groups, the aim of this loop is to generate the 10 simulations and then create a vector that stores the 10 outputs

epi10=list()                                                      ##Create an empty list to store the values of the simulation
for (i in 1:10){                                                  ##indicate in the for loop that the simulation has to run 1o times
  epi10[[length(epi10)+1]] <- simulation()                        ##Store the outputs in a vector called epi10
}

  ##Step (4): Produce plots for 10 replicate simulations showing daily infection trajectories for the 1)whole population, the 2)‘cautious 10%’ and the 3)0.1% random sample.

xlim = 150
##(4.1) PLOT WHOLE POPULATION
#This plot shows the 10 simulations for the whole population. The peaks in the graph 
#indicates the day of the maximum number of infected people, based on that sample size
#The for loop helps us to plot the 10 lines with different colors in order to 
#be able to distinguish the variations
plot(log(epi10[[1]]$I1),ylim=c(0,log(max(epi10[[1]]$I1))),xlab="day",ylab="log(new infection)",cex=0.1, main = "whole population",type="l")
for (i in 2:10) {
  lines(1:xlim,log(epi10[[i]]$I1), col = rainbow(10)[i])
}

##(4.2) PLOT FOR THE CAUTIONS GROUP 10% OF THE POPULATION
#This plot shows the 10 simulations for the cautions group. The peak in the graph 
#indicates the day of the maximum number of infected people, based on that sample size
#The for loop helps us to plot the 10 lines with different colors in order to 
#be able to distinguish the variations
plot(log(epi10[[1]]$I2),ylim=c(0,log(max(epi10[[1]]$I2))),xlab="day",ylab="log(new infection)",cex=0.1, main = "cautious group",type="l")
for (i in 2:10) {
  
  lines(1:xlim,log(epi10[[i]]$I2), col = rainbow(10)[i])
  
}

##(4.3) PLOT FOR RANDOM SAMPLE OF 0.1% OF THE POPULATION 
#This plot shows the 10 simulations for the 0.1% of the total population. The peak in the graph 
#indicates the day of the maximum number of infected people, based on that sample size
#The for loop helps us to plot the 10 lines with different colors in order to 
#be able to distinguish the variations in the data
plot(log(epi10[[1]]$I3),ylim=c(0,log(max(epi10[[1]]$I3))),xlab="day",ylab="log(new infection)",cex=0.1, main = "0.1% group",type="l")
for (i in 2:10) {
  lines(1:xlim,log(epi10[[i]]$I3), col = rainbow(10)[i])
}


#CONCLUSION OF THE PRACTICAL
#
#
#
#
#
#