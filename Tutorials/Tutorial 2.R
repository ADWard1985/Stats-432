# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and conducting normal theory two-way ANOVA

# 1.1a - Read in the 'Cholesterol.csv' data set 
chol1a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\Tutorials\\R Data\\Cholesterol.csv")
chol1a


    # 1.1a1 - Create data for a marginal means plot
    chol1a1 <- chol1a
    chol1a1$group = paste(chol1a1$sex, chol1a1$dosage)
    chol1a1 <- subset(chol1a1, select = c(cholesterol, group))
    chol1a1
    
    tapply(chol1a1$cholesterol, chol1a1$group, mean)


    
  
# 1.1b - Conduct normal theory two-way ANOVA
chol1b <- aov(cholesterol ~ sex + dosage + sex*dosage , chol1a)
summary(chol1b)


    # 1.1b1 - Conduct normal theory two-way ANOVA with variables in opposite order
    chol1b1 <- aov(cholesterol ~ dosage + sex + dosage*sex , chol1a)
    summary(chol1b1)









# SECTION 2 #
#------------------------------------------------------------------------------#
# Conduct permutation test for interaction effects

# 2.1a - Fit main effects model and extract residuals
chol1a
chol2a <- lm(cholesterol ~ sex + dosage, chol1a)
chol2a.res <- resid(chol2a)
chol2a.res


# 2.1b - Use residuals as response in a two-way model with interaction
chol2b <- lm(chol2a.res ~ sex + dosage + sex*dosage, data=chol1a)
anova(chol2b)


# 2.1c - Extract initial F-statistic for interaction effect
chol2c <- anova(chol2b)["sex:dosage", "F value"]
chol2c



# 2.1d - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 2.1d1 - Define number of reordered samples to take
  replic <- 1000
  
  # 2.1d2 - Create blank vector called chol2d
  chol2d <- rep(NA,replic)
  
  # 2.1d3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
    # 2.1d4 - Construct "Do-Loop"
    for (i in 1:replic) 
    {
      # Construct reordered sample of residuals.
      reordered_residuals <- sample(chol2a.res)
      
      # Construct linear model modelling reordered residuals as a function
      # of sex, dosage and any interaction between the two
      this.aov <- lm(reordered_residuals ~ sex + dosage + sex*dosage, data=chol1a)
      anova(this.aov)
      
      # Append F-Statistic to the ith element of the vector chol2d
      chol2d[i] <- anova(this.aov)["sex:dosage","F value"]
    }

  i <- 1
  chol2d
  chol2a.res
  reordered_residuals
  chol1a
  


# 2.1e - Construct histogram showing the generated distribution of test 
# statistics and the position of chol2c
hist(chol2d)
lines(c(chol2c, chol2c),c(0,600),col="red",lwd=3)

# 2.1f - Determine p-value for interaction
length ( chol2d [ chol2d >= chol2c  ] ) / replic

## p-value = 0.831
## No evidence for interaction effect between sex and dosage









# SECTION 3 #
#------------------------------------------------------------------------------#
# Conduct permutation test for effect of sex alone on cholesterol levels

# 3.1a - Conducting one-way ANOVA for the effect of sex on cholesterol
chol1a
chol3a <- aov(cholesterol ~ sex, data=chol1a)
summary(chol3a)


# 3.1b - Extracting initial F-statistic
chol3b <- summary(chol3a)[[1]]["sex","F value"]
chol3b



# 3.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 3.1c1 - Define number of reordered samples to take
  replic1 <- 1000
  
  # 3.1c2 - Create blank vector called chol3c
  chol3c <- rep(NA,replic1)
  
  # 3.1c3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
  # 3.1c4 - Construct "Do-Loop"
  for (i in 1:replic1) 
  {
    # Construct reordered sample of categorical variables
    reordered_sample1 <- sample(chol1a$sex)
    
    # Construct ANOVA model
    this.aov1 <- aov(cholesterol ~ reordered_sample1 , data=chol1a)
    summary(this.aov1)
    
    # Append F-Statistic to the ith element of the vector chol3c
    chol3c[i] <- summary(this.aov1)[[1]]["reordered_sample1","F value"]
  }

  i <- 1
  chol3c
  chol1a
  reordered_sample1
  
  

# 3.1d - Construct histogram showing the generated distribution of test 
# statistics and the position of chol3b
max.x <- 1 + max(chol3b, max(chol3c))
hist(chol3c, xlim=c(0,max.x))
lines(c(chol3b, chol3b),c(0,700),col="red",lwd=3)


# 3.1e - Determine p-value for test
length ( chol3c [ chol3c >= chol3b  ] ) / replic1


## p-value = 0
## very strong evidence for the effect of sex on cholesterol levels








# SECTION 4 #
#------------------------------------------------------------------------------#
# Conduct permutation test for effect of dosage CONDITIONAL on sex  

# 4.1a - Conducting two-way ANOVA (without interaction) for the effect dosage
# (conditional on sex) on chemical X. 
chol1a
chol4a <- aov(cholesterol ~ sex + dosage, data=chol1a)
summary(chol4a)



# 4.1b - Extracting initial F-statistic
chol4b <- summary(chol4a)[[1]]["dosage","F value"]
chol4b



# 4.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 4.1c1 - Define number of reordered samples to take
  replic2 <- 1000
  
  # 4.1c2 - Create blank vector called chol4c
  chol4c <- rep(NA,replic2)
  
  # 4.1c3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
  # 4.1c4 - Construct "Do-Loop"
  for (i in 1:replic2) 
  {
    # Construct reordered sample of categorical variable dosage, keeping
    # sex unchanged
    reordered_sample2 <- sample(chol1a$dosage)
    
    # Construct ANOVA model
    this.aov2 <- aov(cholesterol ~ sex + reordered_sample2 , data=chol1a)
    summary(this.aov2)
    
    # Append F-Statistic to the ith element of the vector chol4c
    chol4c[i] <- summary(this.aov2)[[1]]["reordered_sample2","F value"]
  }


  i <- 1
  chol4c
  chol1a
  reordered_sample2
  
  
  

# 4.1d - Construct histogram showing the generated distribution of test
# statistics and the position of chol4b  
hist(chol4c)
lines(c(chol4b, chol4b),c(0,600),col="red",lwd=3) 


# 4.1e - Determine p-value for test
length ( chol4c [ chol4c >= chol4b  ] ) / replic2


## p-value = 0.007
## Very strong evidence for the effect of dosage on cholestrol even when 
## controlling for sex






