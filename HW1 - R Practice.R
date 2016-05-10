################################

# HW #1 - Practice Using R
# ESM 211 - Applied Population Ecology
# Katherine Millage

# Coding pages 8, 9, and 13 from Chapter 1 of the Stephens Primer

#################################
#########
#### Part 1: Page 8, box 1
#########
# Projecting Population Size - calculating population sizes for 10 time points beyond the initial. 

# Defining parameters
N0 <- 1 # Initial population size is 1 individual
lambda <- 2 # Finite rate of increase (per capita rate of growth if pop is growing geometrically)
time <- 0:10 # Series of time: 0 years since initial to 10 years since initial

# Calculating number of individuals at each time period
Nt <- N0 * lambda^time # number of individuals at each time is equal to the initial number of individuals times the growth rate raised to the time step 

##########
#### Part 2: Page 8, box 2
##########
# Effects of Initial population size - looking at how different initial population sizes change trajectories. 

# Defining parameters
N0_2 <- c(10, 20, 30) # Initial populations sizes of 10, 20 and 30
lambda_2 <- 2 # Finite rate of increase (per capita rate of growth if pop is growing geometrically)
time_2 <- 0:4 # Series of time: 0 years since initial to 4 years since initial

# Calculating number of individuals at each time period for each initial population size. 
Nt.s <- sapply(N0_2, function(n) n * lambda_2^time_2) # sapply applies the same function to each initial population size

##########
#### Part 3: Page 9, box 1
##########
# Graphing a matrix - plotting the resuls of our last function

expplot <- matplot(time_2, Nt.s, pch = 1:3)

logplot <- matplot(time_2, Nt.s, log = "y", pch = 1:3)

##########
#### Part 4: Page 13, box 1
##########
# Comparing arithmetic and geometric averages

# Defining parameters
t <- 5 # time period is 5 years
library(primer) # Loading the package with sparrows
data(sparrows) # pull sparrows data, includes count data for sparrows for each year between 1966 and 2003 by 3 different observers
SS6 <- sparrows[1:(t+1),] # Subsetting the sparrows data set to just include all columns cooresponding to rows 1 to the row after our t (year 6). 

# Calculating lambda for each generation
SSgr <- SS6$Count[2:(t+1)]/SS6$Count[1:t]
lam.A <- sum(SSgr)/t # Arithmatic mean
lam.G <- prod(SSgr)^(1/t) # Geometric mean

# Plot the data and projections for each type of mean
N0_4 <- SS6$Count[1]
plot(0:t, SS6$Count, ylab = "Projected Population Size", xlab = "Time Step")
lines(0:t, N0_4 * lam.A^(0:t), lty = 2)
lines(0:t, N0_4 * lam.G^(0:t), lty = 1)
legend(0,70,c("Arithmetic Mean", "Geometric Mean"), title = "Projections Based On:", lty = 2:1, bty = "n", xjust = 0)

##########
#### Part 5: Page 31, question 1.3 (a)
##########
# There were about 630 million people on the planet in 1700, and 6.3 billion in 2003. What was the intrinsic rate of increase, r? 

# Define parameters
N0_5 <- 630000000
N_2003 <- 6300000000
time_5 <- 2003 - 1700

lambda_5 <- (N_2003/N0_5)^(1/time_5) # Average lambda is 
r <-  log(lambda_5) # intrinsic rate of increase was 0.0076, which is the natural log of lambda