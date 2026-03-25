# CHAPTER   7 - Bootstrapping

# PROGRAM   7.5 - Temporal Clustering (Parametric Bootstrapping Example)

# SUMMARY   The length of time in days between magnitude 2.5 (or above) 
##          earthquakes along a particular fault line is recorded over a period 
##          of 1,835 days. The results of this process are shown in the table 
##          below:
##
##          35	  4.6	  40.8	  1.8	    1.7	  56	  71.3	  111	  60.1	102.2
##          28.5	4.3	  3	      1.1	    11.4	14.8	27.3	  28.3	73.2	0.2
##          13.6	9.5	  67.1	  28.3	  0.9	  13.8	48.8	  43.8	18.4	49.3
##          47	  23.5	13.2	  5.4	    100.1	74.4	6.1	    7.6	  12.8	48.4
##          28	  52	  49.6	  132.6	  31	  13	  97.6	  51.4	9.3	  61.8
##
##          We model these events as arising from a Gamma(alpha, lambda) 
##          distribution. In the first section of this program we create point
##          estimates \hat{alpha} and \hat{\lambda} of these parameters.
##
##          In the second section we construct estimates of the standard error
##          of \hat{alpha} and use this information to construct a 95% CI for
##          alpha. The fact that this CI contains the value 1, indicates that
##          there is no evidence against the assumption that
##            i.    \alpha = 1
##            ii.   The data are drawn from an exponential distribution with 
##                  rate \lambda
##            iii.  The earthquake events exhibit no temporal clustering.



# Clear all variables before starting
rm(list = ls())

# Call required packages
library(tidyverse)



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and calculating sample statistics (point estimates)

# 1.1a - Reading in the data
boot4.data <- c(35.0,4.6,40.8,1.8,1.7,56.0,71.3,111.0,60.1,102.2,
                28.5,4.3,3.0,1.1,11.4,14.8,27.3,28.3,73.2,0.2,13.6,9.5,67.1,28.3,
                0.9,13.8,48.8,43.8,18.4,49.3,47.0,23.5,13.2,5.4,100.1,74.4,6.1,
                7.6,12.8,48.4,28.0,52.0,49.6,132.6,31.0,13.0,97.6,51.4,9.3,61.8)


# 1.1b - Define the length of the data set  
n <- length(boot4.data)
n


# 1.1c - Calculate point estimate of lambda
hat_lambda <- mean(boot4.data)/var(boot4.data)
hat_lambda

    ## \hat{lambda} = 0.03395001


# 1.1d - Calculate point estimate of alpha
hat_alpha <- mean(boot4.data)**2 / var(boot4.data)
hat_alpha

    ## \hat{alpha} = 1.245897



  





# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting the non-parametric bootstrapping procedure, calculating the standard
# error of \hat{alpha} and constructing a confidence interval for \alpha

# 2.1a - Conducting the bootstrapping procedure

  # Define the number of bootstrap samples
  B <- 1000
  
  # Define a vector called 'boot.alpha' of length B
  boot.alpha <- rep(NA,B)
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(1234)
  
  # For i=1,...,B do
  for (b in 1:B) {
    
    # Obtain n observations from Gamma(\hat{\alpha}, \hat{\lambda}) distribution
    pseudo.data <- rgamma(n, shape=hat_alpha, rate=hat_lambda)
    
    # Calculate \alpha^{*i} for the bootstrap sample
    this.alpha <- mean(pseudo.data)**2 / var(pseudo.data)
    
    # Append to the 'boot.alpha' vector
    boot.alpha[b] <- this.alpha
  }

  


# 2.1b - Produce histogram of the \alpha^{*i} obtained
ggplot(as.data.frame(boot.alpha),aes(boot.alpha))+
  geom_histogram(aes(y=..density..), bins=12,fill="transparent", color="black")+
  geom_density(col="blue",lwd=1)
  
    ## The \alpha^{*i} values look to be approximately normally distributed
    ## => 95% CI can be created using the standard CI formula


# 2.1c - Calculating the standard error of \hat{\alpha}
se.alpha <- sd(boot.alpha)
se.alpha

    ## s.e.(\hat{\alpha}) = 0.340212


# 2.1d - Constructing the standard confidence interval
alpha.std.CI <- c(hat_alpha - 1.96 * se.alpha, hat_alpha + 1.96 * se.alpha)
alpha.std.CI

    ## 95% CI for alpha [0.5790819, 1.9127130]




# CONCLUSION #
#------------------------------------------------------------------------------#
# The value 1 is contained in the  95% confidence interval for alpha. As such, 
# we have no evidence that earthquakes along the fault line in question exhibit
# temporal clustering.










