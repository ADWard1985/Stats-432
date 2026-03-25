# Clear all variables before starting
rm(list = ls())

# Call required packages
library(tidyverse)

# Section 1
#-------------------------------------------------------------------------------
# Conducting principal component analysis on the original sample of data

# 1.1a - Read in data trees which is stored in R's memory by default
trees



# 1.1b - Set the number of rows and columns in the data set as variables
n <- nrow(trees)
  n
p <- ncol(trees)
  p

  
  
  
# 1.1c - Standardize the variables, then obtain the covariance matrix 
std_trees <- trees
std_trees[1 : 3] <- as.data.frame(scale(std_trees[1 : 3]))
std_trees

  mean(std_trees$Girth)
  sd(std_trees$Girth)

cov(std_trees)


# 1.1d - Obtain correlation matrix of the non-standardised data
data.corr <- cor(trees)
data.corr



# 1.1e - Obtain eigenvalues and eigenvectors of correlation matrix
data.eigen <- eigen(data.corr)

    # Obtain eigenvectors of the correlation matrix / covariance matrix of standardized data
    eigen(data.corr)$vectors

    # Obtain eigenvalues of the correlation matrix / covariance matrix of standardized data
    eigen(data.corr)$values

    # obtain proportions of variance explained by each eigenvector
    data.percent <- 100*data.eigen$values/sum(data.eigen$values)
    data.percent
 
    

    

    
    
# Section 2
#-------------------------------------------------------------------------------
# Constructing principal component analysis on boot strapped samples to create
# a confidence interval for the proportion of variance explained by the 1st
# principal component


# 2.1a - Specify the number of bootstrapped samples to take and constructing blank
# matrices for results
B <- 1000
boot.eigen <- matrix(NA,B,p)
boot.percent <- matrix(NA,B,p)


    
    

    
# 2.1b - Conducting the bootstrapping procedure
  
  # Set the seed to ensure the same bootstrap samples are created each time
  set.seed(12345)
  
  # Create Do-loop
  for (i in 1:B) {
  
      # Obtain list of data rows to re-sample from
      new.rows <- sample(1:n, n, replace=T)
      
      # Obtain bootstrap sample 
      boot.data <- trees[new.rows,]
      
      # Obtain the eigenvalues
      new.eigen <- eigen(cor(boot.data))[[1]]
      
      # Put eigenvalue in bth row of the boot.eigen matrix
      boot.eigen[i,] <- new.eigen
      
      # Calculate proportion of variance explained and put in bth row of boot.percen matrix
      boot.percent[i,] <- 100*new.eigen/sum(new.eigen)
  
  }
  
  
  
  
  
# Section 3
#-------------------------------------------------------------------------------  
# Analyzing results
  
# 3.1a - Determining whether the proportion of variance explained by each
# principal component in each of the bootstrap samples is approximately normally
# distributed by constructing histograms, boxplots and QQ-plots
  
  # Create 3x3 matrix
  par(mfrow=c(3,3))
  
  # Histograms
  hist(boot.percent[,1],main="% variance, PC1")
  hist(boot.percent[,2],main="% variance, PC1")
  hist(boot.percent[,3],main="% variance, PC1")
  
  # Boxplots
  boxplot(boot.percent[,1],main="% variance, PC1")
  boxplot(boot.percent[,2],main="% variance, PC2")
  boxplot(boot.percent[,3],main="% variance, PC3")
  
  # QQ-plots
  qqnorm(boot.percent[,1],main="% variance, PC1") 
  qqline(boot.percent[,1])
  qqnorm(boot.percent[,2],main="% variance, PC2")  
  qqline(boot.percent[,2])
  qqnorm(boot.percent[,3],main="% variance, PC3") 
  qqline(boot.percent[,3]) 
  
  ## Data looks approximately normally distributed. At least for the first
  ## principal component. Consequently, standard 95% bootstrap confidence interval
  ## is valid
  

  
# 3.1b - Find the bootstrap standard errors:
percent.se <- apply(boot.percent,2,sd)
percent.se




# 3.1c - Create standard 95% bootstrap confidence intervals:
  
    # Create blank 3x3 matrix
    percent.mat <- matrix(NA,3,3)
    
    # Specify names of rows and columns
    dimnames(percent.mat) <- list(c("PC1","PC2","PC3"), c("Estimated %","Lower","Upper"))

    # Obtain confidence interval for proportions of variance explained
    percent.mat[,1] <- data.percent
    percent.mat[,2] <- data.percent - 1.96*percent.se
    percent.mat[,3] <- data.percent + 1.96*percent.se
    percent.mat
    

    
    ## Note: These are not independent estimates. A lower value for the first 
    ## percentage will lead to a higher value for at least one other percentage.
    ## Yet we are predominantly interested in the first principal component
    ## since it explains the vast majority of the variance in the data.
    
    
    





