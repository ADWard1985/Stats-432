# CHAPTER   11 - Gibbs Sampling

# PROGRAM   11.4b - Gibbs Sampling (2D Exponential Example)

# SUMMARY   In this program we use the method of Gibbs Sampling in order to draw
#           a sequence of observations from a bivariate normal distribution with
#           probability density function defined by:
#
#           \mu = (0,0) and \Sigma = 1      \rho
#                                    \rho   1
#
#           The program is structured as follows...
#
#           SECTION 1 - We define a function / macro that will facilitate the 
#           operations of the Gibbs Sampling algorithm.
#
#           SECTION 2 - Running the function produced in the previous section 
#           and analyzing results






# Clear all variables before starting
rm(list = ls())





# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining a function / macro that will facilitate the operations of the
# Gibbs Sampling algorithm. Parameter definitions:

    # n   = number of iterations / samples to take
    # rho = degree of correlation between variables X1 and X2 



# 1.1 - Defining the function
Gibbs_Algorithm <- function(n, rho){

  
  # 1.1a - Create blank output matrix Gibbs_Out with n-rows and 2 columns
  Gibbs_Out <- matrix(NA, nrow = n, ncol = 2)  # (n,2) matrix for output of chain
  
  
  
  # 1.1b - Initialize the chain by taking two values drawn from a standard normal
  # distribution and over-writing first row of the Gibbs_Out matrix.
  initial <- c( rnorm(1), rnorm(1) )
  Gibbs_Out[1,] <- initial

  
  
  # 1.1c - Obtain subsequent elements of the chain
  for (i in 2:n){
    
    # 1.1c1 - Isolate previous state of variable X2 
    old_X2 <- Gibbs_Out[i-1,2]

    # 1.1c2 - Sample a new value of X1 from its conditional distribution given
    # the previous value of X2
    New_X1 <- rnorm(1, mean = rho * old_X2, sd = sqrt(1 - rho^2)   )
    
    # 1.1c3 - Sample a new value of X2 from its conditional distribution given
    # the new value of X1
    New_X2 <- rnorm(1, mean = rho * New_X1, sd = sqrt(1 - rho^2)   )
    
    # 1.1c4 - Append the new values of X1 and X2 into a vector
    New_Vec <- c(New_X1, New_X2)
    
    # 1.1c5 - Overwrite the ith row of the Gibbs_Out matrix
    Gibbs_Out[i,] <- New_Vec
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
Gibbs_Result <- Gibbs_Algorithm(n = burnin + mcmc, rho = 0.7)



# 2.1c - Refining the elements of the chain produced by the Metropolis-Hastings
# algorithm to just the elements after the burn-in
post_burn <- tail(Gibbs_Result$Gibbs_Out, mcmc)


# 2.1d - Splitting the output matrix into vectors
post_burn_x1 <- post_burn[,1]
post_burn_x2 <- post_burn[,2]


# 2.1e - Creating time series plots
want_elements <- (1):(mcmc)
par(mfrow=c(1,2))

plot(want_elements, post_burn_x1, 
     type = "l", main = "Time series plot X1", xlab = "Transitions", ylab = "x_1")

plot(want_elements, post_burn_x2, 
     type = "l", main = "Time series plot X2", xlab = "Transitions", ylab = "x_2")


# 2.1f - Creating auto-correlation plots
par(mfrow=c(1,2))
acf(post_burn_x1, main = "Autocorrelation plot X1")
acf(post_burn_x2, main = "Autocorrelation plot X2")




# 2.1g - Creating histograms of the (marginal) distributions of X1 and X2
par(mfrow=c(1,2))
hist(post_burn_x1, nclass = 16, col = "grey",
     freq = FALSE, xlab = "X1", main = "X1 histogram")
hist(post_burn_x2, nclass = 16, col = "grey",
     freq = FALSE, xlab = "X2", main = "X2 histogram")








