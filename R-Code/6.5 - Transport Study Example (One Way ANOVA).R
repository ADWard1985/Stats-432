# CHAPTER 6 - PERMUTATION TESTING

# PROGRAM   - 6.5 - Transport Study Example (One Way ANOVA)

# SUMMARY   - In this program we analyse whether the mean travel time from a 
#             certain suburb to the city centre differs by mode of transport 
#             (specifically bus, car or cycle).
#
#             Section 1 - Data is read in and a data frame constructed             
#             Section 2 - ANOVA test is conducted
#             Section 3 - Non-parametric Kruskal-Wallis test conducted
#             Section 4 - Permutation test conducted using the usual F-statistic
#             Section 5 - Permutation test conducted using equivalent test stat


# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and creating a dataframe

# 1.1a - Read in and group the data
  
  # Read in the data
  y <- c(21,23,27,22,31,23,28,22,25,29,27,30,29)
    y

  # Group the data
  group <- factor(rep(c("Bus","Car","Cycle"),c(5,4,4)))
    group
    

# 1.1b - Create a data frame
ex2.df <- data.frame(y=y, group=group)
  ex2.df
  
  
# 1.1c - Remove the vectors y and group from R's memory
rm(y)
rm(group)







# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting one-way ANOVA on the data to test between the hypotheses
# H0 : \mu_i = \mu_j for all i,j = 1,..,3
# H1 : \mu_i \neq \mu_j for some i,j = 1,..,3

# 2.1a - Conducting a one-way ANOVA test on the data.
ex2.aov <- aov(y ~ group, data=ex2.df)
summary(ex2.aov)

    ## F-statistic = 2.452, P-value = 0.136
    ## No evidence against null hypothesis of equal means



# 2.1b - Constructing a box-plot of the data.
time <- c(21,23,27,22,31,23,28,22,25,29,27,30,29)
method <- factor(rep(c("Bus","Car","Cycle"),c(5,4,4)))

library(ggplot2)
ggplot(ex2.df, aes(x = time, y = method, fill="lightblue")) +
  geom_boxplot(alpha = 0.2) +
  geom_point(aes(colour = method), size = 3) +
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

    ## Box-plot indicates asymmetry (suggests data not from a normal distribution).
    ## Box-plot also suggests non-equal variance between groups.



# 2.1c - Constructing a normal QQ-plot
qqnorm( resid(ex2.aov) )
qqline( resid(ex2.aov) )

    ## Graph suggests non-normality of the data


    ## Conclusion:
    ## One-way ANOVA does not provide any evidence for a difference in mean time 
    ## between transport modes.
    ## However, the assumptions of the ANOVA test (normality & equal variance) 
    ## are likely violated. Consequently, the p-value produced must be viewed 
    ## with extreme caution.




# SECTION 3 #
#------------------------------------------------------------------------------#
# Given that the ANOVA conditions are violated we conduct a (non-parametric)
# Kruskal-Wallis test to test between the hypotheses
# H0 : Data come from populations with the same distribution
# H1 : Data do not come from populations with the same distribution

# 3.1a - Conduct Kruskal-Wallis test
kruskal.test(y ~ group, data=ex2.df)

    ## Chi-squared = 3.7675, P-value = 0.152. 
    ## => No evidence against null hypothesis






# SECTION 4 #
#------------------------------------------------------------------------------#
# Conducting a permutation test using the usual F-statistic to test between 
# H0 : Mean travel time is the same across all transport modes
# H1 : Mean travel time differs same across all transport modes


# 4.1a - Extract observed test statistic from summary table (see Section 2.1a)
data.F <- summary(ex2.aov)[[1]]["group","F value"]
data.F

    ## F-statistic = 2.452304



# 4.1b - Construct a distribution of test statistics from a cohort of 1,000 
# re-ordered samples.
    
    # 4.1b1 - Define number of reordered samples to construct
    replic <- 1000
    
    # 4.1b2 - Create blank vector of required length
    pseudo.F <- rep(NA, replic)
    
    # 4.1b3 - Set seed to ensure same reordered samples produced each time
    set.seed(12345)
    
    
    tstart <- Sys.time()
    
      # 4.1b4 - Begin "do-loop"
      for (i in 1:replic) {
        
        # Create reordered sample of categorical variable
        group.perm <- sample(ex2.df$group)
        
        # Obtain F-statistic from reordered sample
        this.aov <- aov(y ~ group.perm, data=ex2.df)
          
        # Append F-statistic to ith element of blank vector
        pseudo.F[i] <- summary(this.aov)[[1]]["group","F value"]
      }
    
    Sys.time()-tstart
 
    i <- 1
    ex2.df
    group.perm
    summary(this.aov)
    pseudo.F

    
        

# 4.1c - Construct histogram of F-statistics from reordered sample and location
# of F-statistic from our reordered sample
hist(pseudo.F)
abline(v=data.F, lty=2, lwd=2, col="red")


# 4.1d - Calculate the p-value for the test
length( pseudo.F[pseudo.F >= data.F] )/ replic

    ## P-value = 0.137
    ## No evidence against the null hypothesis







# SECTION 5 #
#------------------------------------------------------------------------------#
# Conducting a permutation test using an equivalent test statistic to test 
# between the hypotheses
# H0 : Mean travel time is the same across all transport modes
# H1 : Mean travel time differs same across all transport modes

# 5.1a - Lifting the data out of the data frame
ex2.df

y <- ex2.df$y
  y

group <- ex2.df$group
  group



# 5.1b - Calculating observed equivalent test statistic ( sum Ti-squared / ni )
ni.vect <- as.numeric(table(group))
ni.vect

data.TS.oneway <- sum( (tapply(y, group, sum))^2 / ni.vect  )
data.TS.oneway




# 5.1c - Construct a distribution of test statistics from a cohort of 1,000 
# re-ordered samples.

  # 5.1c1 - Define number of reordered samples to construct
  replic <- 1000
  
  # 5.1c2 - Create blank vector of required length
  pseudo.TS.oneway <- rep(NA, replic)

  # 5.1c3 - Set seed to ensure same reordered samples produced each time
  set.seed(12345)
  
  
  tstart <- Sys.time()
  
    # 5.1c4 - Begin "do-loop"
    for (i in 1:replic) {
      
      # Create reordered sample of response variable
      y.perm <- sample(y)
      
      # Obtain test statistic and append to ith element of blank vector
      pseudo.TS.oneway[i] <- sum( (tapply(y.perm, group, sum))^2 / ni.vect )
      
    }
  Sys.time()-tstart

  
  i <- 1
  y.perm
  pseudo.TS.oneway


  
  
# 5.1d - Construct graph showing the generated distribution of test statistics
hist(pseudo.TS.oneway)
abline(v=data.TS.oneway,lty=2, lwd=2, col="red")


# 5.1e - Calculate the p-value for the test
length( pseudo.TS.oneway[pseudo.TS.oneway >= data.TS.oneway] )/ replic

    ## P-value = 0.126
    ## No evidence against the null hypothesis