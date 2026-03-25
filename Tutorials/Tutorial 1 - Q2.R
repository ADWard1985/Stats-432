# Tutorial 1 - Q2




# Clear all variables before starting
rm(list = ls())





# SECTION 0 #
#------------------------------------------------------------------------------#
# Reading in the data

# 0.1a - Read in the 'Secretin.csv' data set 
cal0a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\Tutorials\\R Data\\Incoming Calls.csv")
str(cal0a)


# 0.1b - Dropping the variable 'Period'
cal0b = subset(cal0a, select = -c(Period))
cal0b






# SECTION 1 #
#------------------------------------------------------------------------------#
# Looking at whether the normality condition is satisfied

# 1.1a - Creating histograms of the data in each group
par(mfrow=c(1,3))
hist(cal0b$A, prob=T, main="Call Centre A", xlab="Incoming Calls")
hist(cal0b$B, prob=T, main="Call Centre B", xlab="Incoming Calls")
hist(cal0b$C, prob=T, main="Call Centre C", xlab="Incoming Calls")
par(mfrow=c(1,1))


# 1.1b - Stacking the data, creating a linear model and constructing a normal
# normal QQ-plot

  # Stack the data
  cal1b1 <- stack(cal0b)
  cal1b1
  
  # Reorder and rename
  cal1b2 <- cal1b1[, c("ind", "values")]              # Reorder columns
  names(cal1b2)[names(cal1b2)=='ind']    <- 'centre'  # Rename ind = centre
  names(cal1b2)[names(cal1b2)=='values'] <- 'calls'   # Rename values = calls
  cal1b2
  
  # Construct linear model
  cal1b3 <- lm(calls ~ centre, data=cal1b2)
  
  # QQ-Plot of residuals
  qqnorm(cal1b3$res, main="")
  qqline(cal1b3$res)



# 1.1c - Conduct Shapiro-Wilks test
shapiro.test(cal1b3$res)


    ## Shapiro-Wilks p-value = 2.095e-11
    ## Very strong evidence against the null hypothesis that the data is 
    ## normally distributed



# SECTION 2 #
#------------------------------------------------------------------------------#
# Looking at whether the condition of homogeneous variance is satisfied

# 2.1a - Constructing boxplots of the data at each school and associated sd
boxplot(cal0b, col = rainbow(ncol(cal0b)) ,  ylab='Incoming Calls')
sapply(cal0b, sd)


# 2.1b - Create Residual vs Fitted Value Plot
plot(cal1b3$res ~ cal1b3$fit, xlab="Fitted value", ylab="Residuals")
abline(h=0,col="gray")


# 2.1c - Conducting a Levene's test
library(car)
leveneTest(cal1b2$calls, cal1b2$centre)

    ## Levene's test p-value = 0.349
    ## NO evidence against the null hypothesis that the data in each group arises
    ## from populations with equal variance








# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting a permutation test

# 3.1a - Constructing the ANOVA Model
cal3a <- aov(calls ~ centre, data=cal1b2)
summary(cal3a)


# 3.1b - Obtain F Statistic for initial sample
cal3b <- summary(cal3a)[[1]]["centre","F value"]
cal3b

## F_obs = 0.1079835




# 3.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

# Define number of re-ordered samples to take
L <- 1000

# Create a blank vector called Cal7c of length L
cal3c <- rep(NA,L)

# Set seed to ensure same re-ordered samples obtained each time code is run
set.seed(12345)

# Do for i = 1,...,L
for (i in 1:L) {
  
  # Obtain a re-ordered sample from the cal1b2 data set
  Reordered_Sample <- sample(cal1b2$centre)
  
  # Conduct ANOVA on the re-ordered sample
  this.aov <- aov(calls ~ Reordered_Sample, data=cal1b2)
  summary(this.aov)
  
  # Extract the calculated F-Statistic and add to the ith element of the
  # vector cal3c
  cal3c[i] <- summary(this.aov)[[1]]["Reordered_Sample","F value"]
}



# 3.1d - Construct histogram showing the generated distribution of F-statistics 
hist(cal3c)
lines(c(cal3b, cal3b),c(0,400),col="red",lwd=3)


# 3.1e - Calculate the p-value for the test.
length( cal3c [cal3c >= cal3b ] ) / L



