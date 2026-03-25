# Clear all variables before starting
rm(list = ls())

# Call required packages
library(tidyverse)


# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and calculating sample statistics (point estimates)

# 1.1a - Read in the 'Propeller Blades' data set 
Prop1a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\Tutorials\\R Data\\Propeller Blades.csv")
Prop1a


# 1.1b - Define the number of rows in the data set
n <- nrow(Prop1a)
n


# 1.1c - Calculate point estimate of lambda
hat_lambda <- 1 / mean(Prop1a$Time)
hat_lambda

    ## \bar{x} = 498.4668         (average time until propeller splinters)
    ## \hat{lambda} = 0.002006152





# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting a parametric bootstrapping procedure


# 2.1a - Conducting the bootstrapping procedure

  # Define the number of bootstrap samples
  L <- 5000
  
  # Define a vector called 'boot.lambda' of length B
  boot.lambda <- rep(NA,L)
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(12345)
  
  # For i=1,...,L do
  for (i in 1:L) {
    
    # Obtain n observations from the exp(\hat{\lambda}) distribution
    pseudo.data <- rexp(n, rate=hat_lambda)
    
    # Calculate \lambda^{*i} for the bootstrap sample
    this.lambda <- 1 / mean(pseudo.data)
    
    # Append to the 'boot.lambda' vector
    boot.lambda[i] <- this.lambda 
  }


  i <- 1
  boot.lambda
  pseudo.data
  this.lambda




# SECTION 3 #
#------------------------------------------------------------------------------#
# Analyzing results, calculating the standard error of \hat{lambda} and 
# constructing appropriate confidence intervals for \lambda

  
# 3.1a - Calculating the standard error of \hat{\lambda}
se.lambda <- sd(boot.lambda)
se.lambda

  ## s.e.(\hat{\lambda}) = 6.25193e-05



# 3.1b - Constructing the standard 95% bootstrap confidence interval for lambda
lambda.std.CI <- c(hat_lambda - 1.96 * se.lambda, hat_lambda + 1.96 * se.lambda)
lambda.std.CI

    ## Standard CI:   [0.001883614, 0.002128690]


# 3.1c - Obtain Efron's interval and Hall's interval
Efron <- quantile(boot.lambda, probs = c(0.025, 0.975))
Efron

    ## Efron's CI:    [2.5%PC , 97.5PC] 
    ##              = [0.001890860 , 0.002133642]


Hall <- c( 2*hat_lambda - quantile(boot.lambda, probs = c(0.975)), 
           2*hat_lambda - quantile(boot.lambda, probs = c(0.025)))
Hall

    ## Hall's CI:     [2 * \hat{lambda} - 97.5PC , 2 * \hat{lambda} - 2.5PC]
    ##              = [0.001878661 , 0.002121443]




# 3.1d - Produce histogram of the \lambda^{*i} obtained
ggplot(as.data.frame(boot.lambda),aes(boot.lambda))+
  geom_histogram(aes(y=..density..), bins=20,fill="transparent", color="black")+
  geom_density(col="blue",lwd=1)

    ## Distribution of \lambda^{*i} values looks approximately normal




# 3.1e - Create qqplot of \lambda^{*i} values
qqnorm(boot.lambda)
qqline(boot.lambda)

    ## QQ-plot suggests that \lambda^{*i} values are approximately normally distributed
