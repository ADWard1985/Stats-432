# Clear all variables before starting
rm(list = ls())

# Calling required libraries
library(markovchain)
library(expm)




# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining the Markov chain and calculating the stationary / limiting distributions

# 1.1a - Define transition matrix P
statenames <- c("1", "2", "3", "4" , "5" )
P <- matrix(  c(	
  0.00, 0.80, 0.00, 0.00, 0.20,
  0.50, 0.00, 0.25, 0.00, 0.25,
  0.00, 0.30, 0.30, 0.40, 0.00,
  0.00, 0.00, 0.50, 0.00, 0.50,
  0.50, 0.00, 0.25, 0.25, 0.00), nrow=5, byrow=TRUE,
  dimnames = list(statenames, statenames))

# 1.1b - Specifying that the transition matrix P defines a Markov chain
mcp <- new("markovchain", transitionMatrix=P)


# 1.1c - Determine the stationary distribution of the Markov chain defined by P
steadyStates(mcp)




# SECTION 2 #
#------------------------------------------------------------------------------#
# Empirically calculating the convergence of the nth step transition matrix


P%^%1

P%^%2

P%^%4

P%^%6

P%^%8

P%^%10

P%^%12

P%^%14

P%^%16

P%^%18

P%^%20

P%^%25

P%^%30

P%^%35
