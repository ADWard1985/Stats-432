# Clear all variables before starting
rm(list = ls())









# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining a function / macro that will facilitate the operations of the
# Gibbs Sampling algorithm. Parameter definitions:

    # n = number of iterations / samples to take
    # B = Cut-off parameter of the truncated bivariate exponential


# 1.1 - Defining the function
Gibbs_Algorithm <- function(niterations, n, a, b){

  
  # 1.1a - Create blank output matrix Gibbs_Out with niterations-rows and 2 columns
  Gibbs_Out <- matrix(NA, nrow = niterations, ncol = 2)  # (n,2) matrix for output of chain
      Gibbs_Out
  
  
  # 1.1b - Initialize the chain
      # Draw an observation from the beta(a,b) distribution. Call this theta1
      # Then draw an observation from a Bin(n, theta1 ). Call this X1.
      # Combine theta1 and X1 into a vector called initial and overwrite the first
      # row of the Gibbs_Out matrix
      theta1 <- rbeta(1, a, b)
  
      X1 <- rbinom(1, n, theta1)
  
      initial <- c( theta1 ,   X1 )
  
      Gibbs_Out[1,] <- initial

      Gibbs_Out
      
  # 1.1c - Obtain subsequent elements of the chain
  for (i in 2:niterations){
    
    # 1.1c1 - Isolate previous state of variable X
    old_X <- Gibbs_Out[i-1,2]
        old_X
    
    # 1.1c2 - Sample a new value of Theta from its conditional distribution given
    # the previous value of X
    New_theta <- rbeta(1, old_X + a, n - old_X + b)
        New_theta
        
    # 1.1c3 - Sample a new value of X from its conditional distribution given
    # the new value of theta
    New_X <- rbinom(1, n, New_theta)
        New_X
    
    # 1.1c4 - Append the new values of X and theta into a vector
    New_Vec <- c(New_theta, New_X)
        New_Vec
        
    # 1.1c5 - Overwrite the ith row of the Gibbs_Out matrix
    Gibbs_Out[i,] <- New_Vec
    Gibbs_Out
    }

  
  # 1.1d - Return the matrix named Gibbs_Out
  return( list(Gibbs_Out = Gibbs_Out) )
  
}






# SECTION 2 #
#------------------------------------------------------------------------------#
# Running the function produced in the previous section and analyzing results

# 2.1a - Define burn-in and number of elements of the chain to use after the
# burn-in has occurred.
burnin  <- 1000
mcmc    <- 9000


# 2.1b - Running the function
set.seed(12345)
Gibbs_Result <- Gibbs_Algorithm(niterations = burnin + mcmc, n = 15, a = 3, b = 7)



# 2.1c - Refining the elements of the chain produced by the Metropolis-Hastings
# algorithm to just the elements after the burn-in
post_burn <- tail(Gibbs_Result$Gibbs_Out, mcmc)


# 2.1d - Splitting the output matrix into vectors
post_burn_theta <- post_burn[,1]
post_burn_X <- post_burn[,2]



# 2.1e - Creating time series plots
want_elements <- (1):(mcmc)
par(mfrow=c(1,2))

plot(want_elements, post_burn_theta, 
     type = "l", main = "Time series plot of theta values", xlab = "Transitions", ylab = "theta")

plot(want_elements, post_burn_X, 
     type = "l", main = "Time series plot of X values", xlab = "Transitions", ylab = "X")
par(mfrow=c(1,1))



# 2.1f - Creating auto-correlation plots
par(mfrow=c(1,2))
acf(post_burn_theta, main = "Autocorrelation plot of theta values")
acf(post_burn_X, main = "Autocorrelation plot of X values")
par(mfrow=c(1,1))



# 2.1g - Creating histograms of the (marginal) distributions of theta and X
par(mfrow=c(1,2))
hist(post_burn_theta, nclass = 100, col = "grey",
     freq = FALSE, xlim = c(0, 0.8), xlab = "theta",
     main = "theta histogram")
hist(post_burn_X, nclass = 16, col = "grey",
     freq = FALSE, xlim = c(0, 15), xlab = "X",
     main = "X histogram")








