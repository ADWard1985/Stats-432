# CHAPTER   7 - Bootstrapping

# PROGRAM   7.2 - Coefficient of Variation (Bootstrapping Example)

# SUMMARY   A random sample of size 30 is obtained from a population with a 
#           log-normal distribution. We wish to create a confidence interval for
#           the coefficient of variation \theta = \sigma / \mu for this 
#           population.




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


# 1.1c - Construct a box-plot of the data
boxplot(x.data)


# Determine the coefficient of variation for the data
data.cv <- sd(x.data)/mean(x.data)
data.cv

    ## \hat{\theta} = 1.475417







# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting the bootstrapping procedure to determine the standard error
# of \hat{\Theta} and construct a confidence interval for \Theta

# 2.1a - Conduct bootstrapping procedure

  # Specify the number of bootstrap samples B
  B <- 1000
  
  # Creating a blank vector called 'boot.cv' of length B
  boot.cv <- rep(NA,B) 
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(10)
  
  # Create 'Do-Loop'
  for (b in 1:B) {
    
    # Create a bootstrap sample of length n from the data set x.data
    this.x <- sample(x.data, n, replace=T)
    
    # Calculate the coefficient of variation for this bootstrap sample and append
    # as the bth element of the vector boot.cv
    boot.cv[b] <- sd(this.x)/mean(this.x)
  }




# 2.1b - Construct histogram of the sample statistics produced
library(ggplot2)
ggplot(as.data.frame(boot.cv),aes(boot.cv))+
  geom_histogram(aes(y=..density..), bins=12,fill="transparent", color="black")+
  geom_density(col="blue",lwd=1)

    ## Histogram indicates that the sampling distribution is approximately normal
    ## => The standard bootstrap confidence interval is appropriate.




# 2.1c - Calculating the standard error
boot.cv.se <- sd(boot.cv)
boot.cv.se

    ## s.e.(\hat{\theta}) = 0.2073488



# 2.1d - Constructing the standard bootstrapping procedure
CI.lim <- data.cv + c(-1,1)*1.96*boot.cv.se
CI.lim

    ## 95%-CI: [1.069013 , 1.881820]



