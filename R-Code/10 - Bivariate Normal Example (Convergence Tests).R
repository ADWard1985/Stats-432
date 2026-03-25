# CHAPTER   10 - Convergence Diagnostics

# PROGRAM   10 - Bivariate Normal Example (Convergence Tests)

# SUMMARY 
# In this program we use the Metropolis-Hastings algorithm to obtain a series of
# observations drawn from the bivariate normal distribution defined by
#     \mu = (3,1)
#     \Sigma =   1.0  -0.5
#               -0.5   2.0
#
# The program is structured as follows.
#
# Section 1:
# Defining a function / macro that will facilitate the operations of the
# Metropolis-Hastings algorithm.
#
# Section 2:
# Running the function produced in the previous section and analyzing results
#
# Section 3:
# Obtaining Summary statistics using functions from the Coda package
#
# Section 4:
# Performing the Geweke time series diagnostic test
#
# Section 5:
# Performing the Gelman & Rubin multiple sequence diagnostic test
#
# Section 6:
# Performing the Heidelberger & Welch diagnostic test




# Clear all variables before starting
rm(list = ls())


# Calling required packages
library(coda)
library(mvtnorm)



# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining a function / macro that will facilitate the operations of the
# Metropolis-Hastings algorithm. Parameter definitions:

    # n -> Number of iterations
    # a -> Defines 1st element of the initializing vector X^(0) = (a,b)
    # b -> Defines 2nd element of the initializing vector X^(0) = (a,b)


# 1.1a - Define the mean vector and covariance matrix of the target bivariate
# normal distribution
mu.vector <- c(3, 1)                              # Create vector
variance.matrix <- cbind(c(1, -0.5), c(-0.5, 2))  # Create matrix


# 1.2 - Defining the function that will facilitate the MH algorithm
MHbvn <- function(n, a, b){
  
  # 1.2a - Create empty output matrix (xbvn) and empty output vector (accepted)
  xbvn <- matrix(NA, nrow = n, ncol = 2)  # (n,2) matrix for output of chain
  accepted <- rep(NA,n)                   # vector of length n

  
  # 1.2b - Initialize the chain and over-write first row of the xbvn matrix
  initial <- c(a,b)
  xbvn[1,] <- initial
  accepted[1] <- 1
  

  # 1.1c - Obtain the elements of the sequence {X^(t)}_{t=2}^n
  for (i in 2:n){
      
      # 1.1c1 - Isolate current state
      current_state <- xbvn[i-1,]
      
      # 1.1c2 - Draw an observation from N( X_1^(t-1) , 1)
      x_1 <- rnorm(n = 1, mean = current_state[1], sd = 1)
      
      # 1.1c3 - Draw an observation from N( X_2^(t-1) , 2^0.5)
      x_2 <- rnorm(n = 1, mean = current_state[2], sd = 2^0.5)
      
      # 1.1c4 - Combine x_1 and x_2 into a vector
      proposed_state <- c(x_1, x_2)
      
      # 1.1c5 - Define acceptance ratio
      r <-  dmvnorm(proposed_state, mu.vector, variance.matrix)  /
            dmvnorm(current_state,  mu.vector, variance.matrix)
      
      # 1.1c6 - Define acceptance probability
      aprob <- min(1, r, na.rm = T)
      
      # 1.1c7 - Define uniform random variable
      u <- runif(1)
      
      # 1.1c8 - Determine if proposed state is accepted or rejected
      # If acceptance probability >= uniform random variable, accept proposed state
      # If acceptance probability <  uniform random variable, reject proposed state
      if (aprob >= u) xbvn[i,] <- proposed_state
      if (aprob <  u) xbvn[i,] <- current_state
      
      # 1.1c9 - Append result to accepted vector
      # If acceptance probability >= uniform random variable, accept proposed state
      # If acceptance probability <  uniform random variable, reject proposed state
      if (aprob >= u) accepted[i] <- 1
      if (aprob <  u) accepted[i] <- 0
      }
  
  
  # 1.1d - Return the matrix named 'xbvn' and the vector named 'accepted'
  return(list(xbvn=xbvn, accepted=accepted))
  
}

      # There are two outputs of this function / macro:
      # $xbvn     -  A list of all the 2d-elements produced in the chain
      # $accepted -  A binary list of whether each proposed state was accepted or 
      #              rejected in each iteration




# SECTION 2 #
#------------------------------------------------------------------------------#
# Running the function produced in the previous section and analyzing results

# 2.1a - Define burn-in and number of elements of the chain to use after the
# burn-in has occurred.
burnin  <- 1000
mcmc    <- 9000


# 2.1b - Running the function
set.seed(12345)
MHresult <- MHbvn(burnin + mcmc, a=0 , b=0)


# 2.1c - Specify the acceptance rate
acceptance_rate <- mean(MHresult$accepted)
acceptance_rate


# 2.1d - Refining the elements of the chain produced by the Metropolis-Hastings
# algorithm to just the elements after the burn-in
post_burn <- tail(MHresult$xbvn, mcmc)



# 2.1e - Splitting the output matrix into vectors
post_burn_x1 <- post_burn[,1]
post_burn_x2 <- post_burn[,2]



# 2.1f - Creating time series plots
want_elements <- (1):(mcmc)
par(mfrow=c(1,2))

plot(want_elements, post_burn_x1, 
     type = "l", main = "Time series plot", xlab = "Transitions", ylab = "x_1")

plot(want_elements, post_burn_x2, 
     type = "l", main = "Time series plot", xlab = "Transitions", ylab = "x_2")



# 2.1g - Creating auto-correlation plots
par(mfrow=c(1,2))
acf(post_burn_x1, main = "Autocorrelation plot")
acf(post_burn_x2, main = "Autocorrelation plot")



# 2.1h - Obtain the mean of the sample of observations produced
mean_vector <- c( mean(post_burn_x1), mean(post_burn_x2) )
mean_vector



# 2.1i - Obtain covariance matrix of the sample of observations produced
cov_matrix <- cov(post_burn)
cov_matrix





# SECTION 3 #
#------------------------------------------------------------------------------#
# Obtaining Summary statistics using functions from the Coda package

# 3.1a - Convert the chain into an MCMC object, using the coda package function
post_burn0 <- mcmc(post_burn)

# 3.1b - Produce summary statistics
summary(post_burn0)




# SECTION 4 #
#------------------------------------------------------------------------------#
# Performing the Geweke time series diagnostic test

# 4.1a - Performing the Geweke test
geweke.diag(post_burn0)

# 4.1b - Creating Geweke plots
geweke.plot(post_burn0)




# SECTION 5 #
#------------------------------------------------------------------------------#
# Performing the Gelman & Rubin multiple sequence diagnostic test

# 5.1a - Producing several different chains and specifying them to be 
# mcmc objects
set.seed(12345)
MHresult1 <- MHbvn(burnin + mcmc, a=0 , b=0)
chain1 <- mcmc( MHresult1$xbvn )

set.seed(12345)
MHresult2 <- MHbvn(burnin + mcmc, a=-5 , b=-5)
chain2 <- mcmc( MHresult2$xbvn )

set.seed(12345)
MHresult3 <- MHbvn(burnin + mcmc, a=10 , b=10)
chain3 <- mcmc( MHresult3$xbvn )

set.seed(12345)
MHresult4 <- MHbvn(burnin + mcmc, a=-20 , b=-20)
chain4 <- mcmc( MHresult4$xbvn )



# 5.1b - Combining the chains into an mcmc.list
gel_rub_list <- mcmc.list( list(chain1, chain2, chain3, chain4 ) )


# 5.1c - Conducting the Gelman & Rubin multiple sequence diagnostic test
gelman.diag(gel_rub_list)


# 5.1d - Producing Gelman-Rubin plots
gelman.plot(gel_rub_list)




# SECTION 6 #
#------------------------------------------------------------------------------#
# Performing the Heidelberger & Welch diagnostic test for convergence

# 6.1a - Converting the MHresult$xbvn chain to an mcmc object (Heidelberger &
# Welch test must be run on the whole chain)
heidelchain <- mcmc(MHresult$xbvn)

# 6.1b - Conducting the Heidelberger & Welch diagnostic test
heidel.diag(heidelchain, eps = 0.1, pvalue = 0.05)

