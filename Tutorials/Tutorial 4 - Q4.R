# Clear all variables before starting
rm(list = ls())

# Calling required libraries
library(markovchain)
library(expm)




# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining Markov chains and calculating their stationary distributions

# 1.1a - Define transition matrix P
statenames <- c("1", "2", "3", "4" , "5" )
P <- matrix(  c(	
  0.95, 0.05, 0.00, 0.00, 0.00,
  0.05, 0.90, 0.05, 0.00, 0.00,
  0.00, 0.05, 0.90, 0.05, 0.00,
  0.00, 0.00, 0.05, 0.90, 0.05,
  0.00, 0.00, 0.00, 0.05, 0.95), nrow=5, byrow=TRUE,
  dimnames = list(statenames, statenames))

# 1.1b - Specifying that the transition matrix P defines a Markov chain
mcp <- new("markovchain", transitionMatrix=P)


# 1.1c - Determine the stationary distribution of the Markov chain defined by P
steadyStates(mcp)



# 1.1d - Define transition matrix Q
statenames <- c("1", "2", "3", "4" , "5" )
Q <- matrix(  c(	
  0.50, 0.25, 0.25, 0.00, 0.00,
  0.25, 0.50, 0.15, 0.10, 0.00,
  0.15, 0.25, 0.50, 0.10, 0.00,
  0.10, 0.00, 0.10, 0.60, 0.20,
  0.00, 0.00, 0.00, 0.20, 0.80), nrow=5, byrow=TRUE,
  dimnames = list(statenames, statenames))

# 1.1e - Specifying that the transition matrix Q defines a Markov chain
mcq <- new("markovchain", transitionMatrix=Q)


# 1.1f - Determine the stationary distribution of the Markov chain defined by P
steadyStates(mcq)







# SECTION 2 #
#------------------------------------------------------------------------------#
# Determining the rate of convergence of either Markov chain to the limiting
# distribution


# 2.1a - Define the transition matrix X with all elements equal to 0.2. 
statenames <- c("1", "2", "3", "4" , "5" )
X <- matrix(  c(	
  0.20, 0.20, 0.20, 0.20, 0.20,
  0.20, 0.20, 0.20, 0.20, 0.20,
  0.20, 0.20, 0.20, 0.20, 0.20,
  0.20, 0.20, 0.20, 0.20, 0.20,
  0.20, 0.20, 0.20, 0.20, 0.20), nrow=5, byrow=TRUE,
  dimnames = list(statenames, statenames))



# 2.1b - Specify parameters and define a blank vector matrix to hold results
L <- 100
frobenius_distance <- matrix(NA, L, 2)
frobenius_distance



# 2.1c - Constructing  Do-loop to raise P^n and Q^n and see how far the result
# is from X in the Frobenius norm
for (n in 1:L) {

  # Calculate the distance between P^n and X in the Frobenius norm, i.e.
  # calculate || P^n - X ||_F and append to the nth row and 1st column of the
  # matrix frobenius_distance
  frobenius_distance[n,1] <- norm(P%^%n - X, type = "F") 
  
  
  # Calculate the distance between Q^n and X in the Frobenius norm, i.e.
  # calculate || Q^n - X ||_F and append to the nth row and 2nd column of the
  # matrix frobenius_distance
  frobenius_distance[n,2] <- norm(Q%^%n - X, type = "F") 
 
}



# 2.1d - Constructing a graph showing the rate of convergence
matplot(as.data.frame(frobenius_distance),type="l", xlab = "n", ylab="Distance", main="Rate of Convergence to Limiting Distribution")




