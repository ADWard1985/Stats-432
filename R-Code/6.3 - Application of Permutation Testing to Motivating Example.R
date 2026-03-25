# CHAPTER 6 - PERMUTATION TESTING

# PROGRAM   - 6.3 - Application of Permutation Testing to Motivating Example

# SUMMARY   - In this program we analyse whether the mean length of fish inside 
#             (A) and outside (B) a marine reserve differ significantly. This is 
#             done by performing both a two-sided and one-sided permutation test.





# Clear all variables before starting
rm(list = ls())

# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and creating a dataframe

# 1.1a - Read in and group the data
y <- c(49,43,31,44,34,35,47,52,40,53,
       73,68,64,70,46,68,65,32,33,39)
    
    # y


group <- factor(rep(c("A","B"),c(10,10)))

    # group


# 1.1b - Create a data frame
ex1.df <- data.frame(y=y,group=group)
ex1.df



# 1.1c - Delete vectors called 'y' and 'group' from R's memory
rm(y)
rm(group)








# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting a two-sided permutation test using the absolute value of the 
# usual t-statistic
# H0: No difference in mean length of fish inside (A) & outside (B) reserve
# H1: Mean length of fish differs inside (A) & outside (B) reserve



# 2.1a - Determine the abs value of the observed test statistic
data.t <- abs( t.test(y ~ group, data=ex1.df, var.equal=T) $statistic )
data.t
    ## | t_obs | = 2.273289




# 2.1b - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 2.1b1 - Define number of reorders samples to take
  replic <- 1000
  
  # 2.1b2 - Create blank vector called pseudo.t
  pseudo.t <- rep(NA,replic)
  
  # 2.1b3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(123456)
  
  
  # 2.1b4 - Construct "Do-Loop"
  for (i in 1:replic) 
    {
      # Construct reorded sample of categorical variables
      group.perm <- sample(ex1.df$group)
    
      # Obtain test statistic from reordered sample and append to ith element of blank vector
      pseudo.t[i] <- abs( t.test(y ~ group.perm, data=ex1.df, var.equal=T) $statistic )
    }

  
  

# 2.1c - Construct graph showing the generated distribution of test statistics 
# and the position of t_obs
hist(pseudo.t)
lines(c(data.t,data.t),c(0,350),col="red",lwd=3)


# 2.1d - Calculate the p-value for the test
length( pseudo.t [pseudo.t>=data.t] ) / replic


    ##  Conclusion - The p-value produced provides some evidence against H0.
    ##  The p-value produced is similar to that obtained in the t-test produced  
    ##  in program '6.2 - Motivating Example' conducted on the same data. 
    ##  However, this permutation test makes no assumptions about the normality 
    ##  of the data or whether the groups having equal variance.










# SECTION 3 #
#--------------------------------------------------------------------------------------------#
# Conducting a one-sided permutation test using the usual t-statistic
# H0: No difference in mean length of fish inside (A) & outside (B) reserve
# H1: Mean length of fish is greater outside (B) marine reserve than inside (A)

# 3.1a - Determine the value of the observed test statistic and store the result
data.t1 <- t.test(y ~ group, data=ex1.df, var.equal=T)$statistic
data.t1
    ## t_obs = -2.273289



# 3.1b - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 3.1b1 - Define number of reordered samples to take
  replic1 <- 1000
  
  # 3.1b2 - Create blank vector called pseudo.t1
  pseudo.t1 <- rep(NA,replic1)
  
  # 3.1b3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(123456)

  # 3.1b4 - Construct "Do-Loop"
  for (i in 1:replic1) 
    {
      # Construct reordered sample of categorical variables
      group.perm1 <- sample(ex1.df$group)
      
      # Obtain test statistic from reordered sample and append to ith element of blank vector
      pseudo.t1[i] <- t.test(y ~ group.perm1, data=ex1.df, var.equal=T)$statistic
    }
  
  

# 3.1c - Construct graph showing the generated distribution of test statistics 
# and the position of t_obs
hist(pseudo.t1)
lines(c(data.t1,data.t1),c(0,350),col="red",lwd=3)

# 3.1d - Calculate the p-value for the test
length( pseudo.t1 [ pseudo.t1 <= data.t1 ]) / replic1


    ##  Conclusion - The p-value produced provides some evidence against the null hypothesis

