### Author: Joseph Eddy
### Script for simulating organism species survival as described
### in FiveThirtyEight's Riddler Classic (1/27/17).

### "At the beginning of time, there is a single microorganism. 
### Each day, every member of this species either splits into two copies of itself 
### or dies. If the probability of multiplication is p, what are the chances 
### that this species goes extinct?"



### Function for simulating species population over time using problem rules
### Args -- 
###  p: probability of multiplication
###  T: Time limit of simulation in days
###
### Return -- length T vector whose tth element gives
###           the # of organisms at day t, starting from 1 on day 1

simulate_species <- function(p, T)
{
  members_time <- c(1, rep(0,T-1))
  members <- 1
  t <- 1
  
  while (members > 0 & t < T)
  {
    t <- t+1
    members <- sum(2 * rbinom(members, 1, p))
    members_time[t] <- members
  }
  
  members_time
}

### Function for simulating the proportion of species that survive 
### over time using the problem rules, for N iterations of simulation
### Args -- 
###  p: probability of multiplication
###  T: Time limit of simulation in days
###  N: Number of simulations
###
### Return -- length T vector whose tth element gives
###           the proportion of simulations where the species
###           survived to day t, starting from day 1

simulate_survivalRatio <- function(p, T, N)
{ 
  i <- 0
  survivalRatio <- rep(0,T)
  
  while (i < N)
  {
    simul <- simulate_species(p, T)
    survivalRatio <- survivalRatio + (simul > 0)
    i <- i + 1
  }
  
  survivalRatio / N
}

### Using the supporting functions above, here is code
### for creating a plot for survival ratios for 6 p values over time,
### running 10000 simulations for each p value 
### (this takes a while! swap 10000 with smaller N for a faster run) 

data <- data.frame(time = seq(1, 30), 
          simulate_survivalRatio(.80, 30, 10000),
          simulate_survivalRatio(.65, 30, 10000),
          simulate_survivalRatio(.50, 30, 10000),
          simulate_survivalRatio(.35, 30, 10000),
          simulate_survivalRatio(.20, 30, 10000),
          simulate_survivalRatio(.05, 30, 10000))

plot(data[,2] ~ time, 
     data = data,
     type = "l", 
     ylim = c(0, 1),
     xlab = "Day",
     ylab = "Survival Ratio")

plot_colors <- c("royalblue","cyan","green", 
                 "red", "purple", "pink")

i <- 2
while (i <= ncol(data))
{
  lines(data$time, data[,i], col = plot_colors[i-1])
  i <- i+1
}

legend(12,1.03, c("p = .80","p = .65","p = .50","p = .35","p = .20", "p = .05"), 
       lwd=c(2.5,2.5),
       cex=0.75,
       col=plot_colors,
       ncol = 3)
