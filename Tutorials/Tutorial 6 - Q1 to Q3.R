# Clear all variables before starting
rm(list = ls())

# Calling required packages
library(coda)

# Specifying the width of the uniform proposal density used throughout
delta <- 0.15


# SECTION 1 #
#------------------------------------------------------------------------------#
# Defining a function / macro that will facilitate the operations of the
# Metropolis-Hastings algorithm using a uniform random walk sampler


# 1.1 - Defining the function / macro 
MetHat <- function(n, start, proposal_width){
  
    # 1.1a - Create blank vectors
    x <- rep(NA, n)
    accepted<- rep(NA, n)
    
    
    # 1.1b - Initialize the chain
    x[1] <- start
    accepted[1] <- 1
    
    
    # 1.1c - Obtain the elements of the sequence {X^(t)}_{t=2}^n
    for (i in 2:n){
      
      
        # 1.1c1 - Draw a proposed state y from the proposal density defined by
        #         y_{t+1} = X^(t) + \epsilon_t  where \epsilon_t ~ U[-d/2, d/2]
        y <- runif(1, 
                 min = x[i-1] - proposal_width / 2, 
                 max = x[i-1] + proposal_width / 2)
        
        
        # 1.1c2 - Define f(y)
        if (y >= 0 && y <= 3) 
            fy <- 1/18 * y^2 + 1/6  else fy <- 0
        
            
        # 1.1c3 - Define f(x)
        if (x[i-1] >= 0 && x[i-1] <= 3) 
            fx <- 1/18 * x[i-1]^2 + 1/6  else fx <- 0
    
        
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
    return( list(x = x, accepted = accepted) )

}



    # There are two outputs of this function / macro:
    # $x        -  A list of all the elements produced in the chain
    # $accepted-  A binary list of whether each proposed state was accepted or 
    #                     rejected in each iteration









# SECTION 2 #
#------------------------------------------------------------------------------#
# Running the function / macro produced in the previous section


# 2.1a - Define burn-in and number of elements of the chain to use after the
# burn-in has occurred.
burnin  <- 1000
mcmc    <- 9000



# 2.1b - Running the function / macro defined in the previous section. 
# Results are saved in a list object called MH_Output
set.seed(12345)
MH_Output <- MetHat(n = burnin + mcmc, 
                    start = 1.5, 
                    proposal_width = delta)





# SECTION 3 #
#------------------------------------------------------------------------------#
# Basic diagnostics


# 3.1a - Specify the acceptance rate of the chain
acceptance_rate <- mean(MH_Output$accepted)
acceptance_rate


# 3.1b - Refining the elements of the chain produced by the Metropolis-Hastings
# algorithm to just the elements after the burn-in
post_burn <- tail(MH_Output$x, mcmc)


# 3.1c - Create a time series plot of all elements in the chain AFTER the burn-in
want_elements <- (1):(mcmc)
plot(want_elements, post_burn[want_elements], type = "l",
     main = "Time series plot", ylab = "x")


# 3.1d - Create autocorrelation plot of all elements in the chain after the burn-in
acf(post_burn[want_elements], main = "Autocorrelation plot")


# 3.1f - Convert the post-burn in chain to a mcmc object obtain summary stats.
# Make sure you have downloaded and called the 'coda' package
post_burn0 <- mcmc(post_burn)
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

    # Chain 0
    MH_Output0 <- MetHat(n = burnin + mcmc, 
                        start = 0, 
                        proposal_width = delta)
    chain0 <- mcmc(MH_Output0$x)
    
    
    # Chain 1
    MH_Output1 <- MetHat(n = burnin + mcmc, 
                         start = 1, 
                         proposal_width = delta)
    chain1 <- mcmc(MH_Output1$x)
    
    
    # Chain 2
    MH_Output2 <- MetHat(n = burnin + mcmc, 
                         start = 2, 
                         proposal_width = delta)
    chain2 <- mcmc(MH_Output2$x)
    
    
    # Chain 3
    MH_Output3 <- MetHat(n = burnin + mcmc, 
                         start = 3, 
                         proposal_width = delta)
    chain3 <- mcmc(MH_Output3$x)

    


# 5.1b - Combining the chains into an mcmc.list
gel_rub_list <- mcmc.list( list(chain0, chain1, chain2, chain3 ) )


# 5.1c - Conducting the Gelman & Rubin multiple sequence diagnostic test
gelman.diag(gel_rub_list)


# 5.1d - Producing Gelman-Rubin plot
gelman.plot(gel_rub_list)






# SECTION 6 #
#------------------------------------------------------------------------------#
# Performing the Heidelberger & Welch diagnostic test for convergence

# 6.1a - Converting the original MH_Output$x chain to an mcmc object 
# (Heidelberger & Welch test must be run on the whole chain)
heidelchain <- mcmc(MH_Output$x)

# 6.1b - Conducting the Heidelberger & Welch diagnostic test
heidel.diag(heidelchain, eps = 0.1, pvalue = 0.05)

