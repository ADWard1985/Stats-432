# Tutorial 1 - Q1




# Clear all variables before starting
rm(list = ls())





# SECTION 0 #
#------------------------------------------------------------------------------#
# Reading in the data

# 0.1a - Read in the 'Secretin.csv' data set 
sec0a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\Tutorials\\R Data\\Secretin.csv")
str(sec0a)


# 0.1b - Dropping the variable 'Participant'
sec0b = subset(sec0a, select = -c(Participant))
sec0b






# SECTION 1 #
#------------------------------------------------------------------------------#
# Looking at whether the normality condition is satisfied

# 1.1a - Creating histograms of the data in each group
par(mfrow=c(1,3))
hist(sec0b$Non_Drinkers,      prob=T, main="Non-Drinkers",      xlab="Secretin Level")
hist(sec0b$Moderate_Drinkers, prob=T, main="Moderate Drinkers", xlab="Secretin Level")
hist(sec0b$Heavy_Drinkers,    prob=T, main="Heavy Drinkers",    xlab="Secretin Level")
par(mfrow=c(1,1))


# 1.1b - Stacking the data, creating a linear model and constructing a normal
# normal QQ-plot

  # Stack the data
  sec1b1 <- stack(sec0b)
  sec1b1
  
  # Reorder and rename
  sec1b2 <- sec1b1[, c("ind", "values")]                 # Reorder columns
  names(sec1b2)[names(sec1b2)=='ind']    <- 'group'      # Rename ind = group
  names(sec1b2)[names(sec1b2)=='values'] <- 'secretin'   # Rename values = secretin
  sec1b2
  
  # Construct linear model
  sec1b3 <- lm(secretin ~ group, data=sec1b2)
  
  # QQ-Plot of residuals
  qqnorm(sec1b3$res, main="")
  qqline(sec1b3$res)



# 1.1c - Conduct Shapiro-Wilks test
shapiro.test(sec1b3$res)


    ## Shapiro-Wilks p-value = 0.1382
    ## NO evidence against the null hypothesis that the data is normally distributed



# SECTION 2 #
#------------------------------------------------------------------------------#
# Looking at whether the condition of homogeneous variance is satisfied

# 2.1a - Constructing boxplots of the data at each school and associated sd
boxplot(sec0b, col = rainbow(ncol(sec0b)) ,  ylab='Secretin Levels')
sapply(sec0b, sd)


# 2.1b - Create Residual vs Fitted Value Plot
plot(sec1b3$res ~ sec1b3$fit, xlab="Fitted value", ylab="Residuals")
abline(h=0,col="gray")


# 2.1c - Conducting a Levene's test
library(car)
leveneTest(sec1b2$secretin, sec1b2$group)

    ## Levene's test p-value = 0.8697
    ## NO evidence against the null hypothesis that the data in each group arises
    ## from populations with equal variance




# SECTION 3 #
#------------------------------------------------------------------------------#
# The above sections indicate that the ANOVA conditions are satisfied. As such,
# we apply one way ANOVA to the data

# 3.1a - Constructing the ANOVA Model
sec3a <- lm(secretin ~ group, data=sec1b2)
anova(sec3a)

  # Altenative way of doing the same thing
  sec3a <- aov(secretin ~ group, data=sec1b2)
  summary(sec3a)

    ## H0: Mean secretin level is the same for all groups
    ## H1: Mean secretin level differs across groups

    ## F-statistic = 0.468
    ## df = (2, 297)
    ## p-value = 0.627

    ## No evidence against the null hypothesis that the mean level of secretin
    # is the same across all groups.


