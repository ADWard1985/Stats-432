# CHAPTER   8 - Markov Chains

# PROGRAM   8.1 - Random Walk on a Markov Chain

# SUMMARY   In this short program we define a Markov chain by specifying a
#           transition matrix and then construct a random walk on the chain.




# Clear all variables before starting
rm(list = ls())
set.seed(2346789)



# SECTION 1 #
#------------------------------------------------------------------------------#
# Constructing the Markov chain and performing a random walk

# 1.1a - Make sure you have the "markov chain" package installed before running.
# Calling the markov chain package. Defining state space and transition matrix.
library(markovchain)
statenames <- c("1", "2", "3", "4" , "5")
P <- matrix(c(0.00, 0.80, 0.00, 0.00, 0.20,
              0.50, 0.00, 0.25, 0.00, 0.25,
              0.00, 0.30, 0.30, 0.40, 0.00,
              0.00, 0.00, 0.50, 0.00, 0.50,
              0.50, 0.00, 0.25, 0.25, 0.00),nrow=5,byrow=TRUE,
              dimnames=list(statenames,statenames))


# 1.1b - Specifying that the transition matrix defines a Markov chain
mc <- new("markovchain", transitionMatrix=P)


# 1.1c - Constructing a graph of the Markov chain
mc@transitionMatrix <- P
plot(mc)


# 1.1d - Conducting a random walk on the Markov Chain stating a state 1
markovchainSequence(n=10, markovchain=mc, t0="1", 
                    include=TRUE)

