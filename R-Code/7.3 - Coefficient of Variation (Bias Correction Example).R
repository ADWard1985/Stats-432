# CHAPTER   7 - Bootstrapping

# PROGRAM   7.3 - Coefficient of Variation (Bias Correction Example)

# SUMMARY   A random sample of size 30 is obtained from a population with a 
#           log-normal distribution. We use this sample to create an estimate
#           of the coefficient of variation for the population.

#           1,000 bootstrap samples are then produced the same sample statistic
#           is calculated. The mean of these sample statistics is calculated
#           and used to create a bias corrected estimate of the population
#           coefficient of variation.





# Clear all variables before starting
rm(list = ls())





# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and constructing the sample statistic \hat{\theta}

# 1.1a - Read in the data 
x.data <- c(22,949,538,165,12,418,11,2451,66,3220,712,9,1977,66,30,
            19,491,1732,9,107,151,89,677,81,300,1445,54,410,170,171)
x.data


# 1.1b - Specify the length of the vector
n <- length(x.data)
n


# 1.1c - Determine the coefficient of variation for the data
Theta_hat <- sd(x.data)/mean(x.data)
Theta_hat

    ## \hat{\theta} = 1.475417







# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting the bootstrapping procedure 


# 2.1a - Conduct bootstrapping procedure

  # Specify the number of bootstrap samples B
  B <- 1000
  
  # Create a blanck vector called 'boot.cv' of length B
  boot.cv <- rep(NA,B) 
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(10)
  
  # Create 'Do-Loop'
  for (b in 1:B) {
    
    # Create a bootstrap sample of length n from the data set x.data
    this.x <- sample(x.data,n,replace=T)
    
    # Calculate the coefficient of variation for this bootstrap sample and append
    # as the bth element of the vector boot.cv
    boot.cv[b] <- sd(this.x)/mean(this.x)
  }




# 2.1b - Calculating the mean of the bootstrap estimates
bar_theta_star <- mean(boot.cv)
bar_theta_star

    ## \bar{\theta}^* = \frac{1}{B} \sum_{i=1}^B \hat{\theta}^{*i} = 1.435166



# 2.1c - Calculate Bias
bias <- bar_theta_star - Theta_hat
bias

    ## Bias = \bar{\theta}^* - \hat{\Theta} 
    ##      = 1.435166 - 1.475417
    ##      = -0.04025062



# 2.1d - Bias Corrected estimate of \theta
theta_bc <- 2 * Theta_hat - bar_theta_star
theta_bc





