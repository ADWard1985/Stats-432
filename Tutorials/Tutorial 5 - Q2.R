# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining a function / macro that will facilitate the operations of the
# Metropolis-Hastings algorithm. Parameter definitions

    # n = number of elements of the chain
    # start = initial state of the chain



# 1.1 - Defining the function / macro 
MetHat1 <- function(n, start){
  
    # 1.1a - Create blank vectors
    x <- rep(NA, n)
    accepted <- rep(NA, n)
    
    
    # 1.1b - Initialize the chain
    x[1] <- start
    accepted[1] <- 1
    
    
    # 1.1c - Obtain the elements of the sequence {X^(t)}_{t=2}^n
    for (i in 2:n){
      
      
        # 1.1c1 - Draw a proposed state y from the proposal density 
        #         q(x,y) = q(y) = 1 + U[0,1]
        y <- 1 + runif(1)
        
        # 1.1c2 - Define f(y)
        if (y >= 1 && y <= 2) fy <- y^2 * (1-y)^2 else fy <- 0
        
        # 1.1c3 - Define f(x)
        if (x[i-1] >= 1 && x[i-1] <= 2) fx <- x[i-1]^2 * (1-x[i-1])^2 else fx <- 0
        
        # 1.1c3 - Define acceptance ratio
        r <- fy / fx
        
        # 1.1c4 - Define the acceptance probability
        aprob <- min(1,r)
        
        # 1.1c5 - Define uniform random variable
        u <- runif(1)
        
        # 1.1c6 - Determine if proposed state is accepted or rejected
        # If acceptance probability >= uniform random variable, accept proposed state
        # If acceptance probability <  uniform random variable, reject proposed state
        if (aprob >= u) x[i] <- y
        if (aprob <  u) x[i] <- x[i-1]
        
        # 1.1c7 - Append result to accepted vector
        # If acceptance probability >= uniform random variable, accept proposed state
        # If acceptance probability <  uniform random variable, reject proposed state
        if (aprob >= u) accepted[i] <- 1
        if (aprob <  u) accepted[i] <- 0
      
    }
    
    # 1.1d - Return list of states and accept-reject occurrences
    return(list(x=x, accepted=accepted))

}



    # There are two outputs of this function / macro:
    # $x        -  A list of all the elements produced in the chain
    # $accepted -  A binary list of whether each proposed state was accepted or 
    #              rejected in each iteration







# SECTION 2 #
#------------------------------------------------------------------------------#
# Running the function / macro produced in the previous section and analyzing results


# 2.1a - Define burn-in and number of elements of the chain to use after the
# burn-in has occurred.
burnin  <- 1000
mcmc    <- 9000



# 2.1b - Running the function / macro defined in the previous section. 
# Results are saved in a list object called MH_Output
set.seed(12345)
MH_Output <- MetHat1(burnin + mcmc, start = 1.5)



# 2.1c - Specify the acceptance rate.
acceptance_rate <- mean(MH_Output$accepted)
acceptance_rate



# 2.1d - Create a time series plot of all elements in the chain AFTER the burn-in
want_elements <- (burnin + 1):(burnin + mcmc)
plot(want_elements, MH_Output$x[want_elements], type = "l",
     main = "Time series plot", ylab = "x")



# 2.1e - Create autocorrelation plot of all elements in the chain after the burn-in
acf(MH_Output$x[want_elements], main = "Autocorrelation plot")






# SECTION 3 #
#------------------------------------------------------------------------------#
# Performing simple Monte Carlo integration

# 3.1a - Refining the elements of the chain produced by the Metropolis-Hastings
# algorithm to just the elements after the burn-in
post_burn <- tail(MH_Output$x, mcmc)



# 3.1b - Calculate approximate value of E[X]
g3 <- function(x){ log(sqrt(x)) }
barg3 = sum(g3(post_burn)) / mcmc
barg3

 