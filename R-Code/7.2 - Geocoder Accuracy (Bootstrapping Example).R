# CHAPTER   7 - Bootstrapping

# PROGRAM   7.2 - Geocoder Accuracy (Bootstrapping Example)

# SUMMARY   A geocoder is a tool into which addresses are fed and which returns
##          the geo-coordinates (Long, Lat) of those addresses. All geocoders
##          work on the same basic principle. They contain a reference list of
##          all possible addresses (along with their geo-coordinates) and a fuzzy 
##          matching algorithm which matches input address strings onto the 
##          reference list. Because the matching algorithm is fuzzy, there is 
##          always a degree of error associated to geocoders. For example the
##          input address:
##              Unit 5, 21 Willis Street, Wellington, 6011
##          may be mistakenly matched onto the address
##              21 Willis Street, Wellington, 6011
##          a couple of hundred metres away.
##
##          A new geocoder has been built. We wish to determine the distance 
##          \Theta such that 95% of addresses geocoded are correct to within
##          \Theta metres.
##
##          A sample of 4,500 addresses (whose geo-cordinates are known a priori) 
##          is taken. For each of those addresses the geodetic (straight line)
##          distance between the actual address and matched address has been
##          calculated. Since the distribution of \Theta is not known and it
##          it is prohibitively time consuming to re-sample for the population
##          by running additional samples of addreses through the geocoder
##          (30 min processing time for each batch of 4,500 addresses), we attempt
##          to calculate the standard error of \hat{\Theta} and construct a 
##          confidence interval for \Theta using a bootstrapping algorithm.




# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the 'Geocoder Data.csv' and extracting the 95th percentile 
# geodetic distance

# 1.1a - Read in 'Geocoder Data.csv'
Geo1a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\R Data\\Chapter 7 - Bootstrapping\\Geocoder Data.csv")
Geo1a


# 1.1b - Refining to required fields only
Geo1b = subset(Geo1a, select = c(ID, Geodetic_Distance_m) )
Geo1b

# 1.1c - Calculating the sample statistic \hat{Theta}
Geo1c <- quantile(Geo1b$Geodetic_Distance_m, probs = c(0.95))
Geo1c

    ## \hat{Theta} = 697.35 
    ## i.e. the geocoder gets 95% of addresses correct to within 697.35m








# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting the bootstrapping procedure to determine the standard error
# of \hat{\Theta} and construct a confidence interval for \Theta

# 2.1a - Specify the number of observations in our sample
n <- length(Geo1b$ID)
n



# 2.1b - Conduct bootstrapping procedure

  # Specify number of bootstrap samples B
  B <- 1000
  
  
  # Creating a blank vector called 'Geo2b' of length B
  Geo2b <- rep(NA,B)
  
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(123)
  
  
  # Create 'Do-Loop'
  for (i in 1:B) {
    
    # Obtain list of data rows WITH REPLACEMENT
    Bootstrap_Rows <- sample(1:n, n, replace=T)
    
    # Create a bootstrap from the vector Geo1b of length n = Geo2a with replacement
    Bootstrap_Sample_i <- Geo1b[Bootstrap_Rows,]
    Bootstrap_Sample_i <- Bootstrap_Sample_i[order(Bootstrap_Sample_i$ID),]
    
    
    # Calculate the 95th percentile distance for this bootstrap sample and append
    # to the ith element of the vector Geo2b
    Geo2b[i] <- quantile( Bootstrap_Sample_i$Geodetic_Distance_m, probs = c(0.95) )
    }

  

  
  

  
  
# 2.1c - Obtain standard error of \hat{Theta}
Geo2c <- sd(Geo2b)
Geo2c
  
  
  
# 2.1d - Create histogram of the bootstrap sample statistics
library(ggplot2)
ggplot(as.data.frame(Geo2b),aes(Geo2b))+
    geom_histogram(aes(y=..density..), bins=12,fill="transparent", color="black")+
    geom_density(col="blue",lwd=1)

  ## Data does not appear to be normally distributed.
  ## => Construct percentile based confidence intervals
  
  
  




# 2.1e - Creating a 95% confidence interval for \Theta using Efron's Interval
Geo2e <- quantile(Geo2b, probs = c(0.025, 0.975))
Geo2e

    ## Efron's 95%-CI: [567.9538, 813.0150]



# 2.1f - Creating a 95% confidence interval for \Theta using Hall's Interval
Geo2f <- c( 2*Geo1c - quantile(Geo2b, probs = c(0.975)), 
            2*Geo1c - quantile(Geo2b, probs = c(0.025)))
Geo2f

    ## Hall's 95%-CI: [581.6850, 826.7463]


