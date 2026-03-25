# CHAPTER 6 - PERMUTATION TESTING

# PROGRAM   - 6.2 - Motivating Example

# SUMMARY   - In this program we analyse whether the mean length of fish inside 
#             (A) and outside (B) a marine reserve differ significantly. We 
#             begin by performing a t-test and obtaining the associated p-values. 
#             However, we then show graphically that the assumptions of the 
#             t-test (i.e. independence, normality equal variance across groups) 
#             are unlikely to be satisfied. As such, we then perform a
#             non-parametric Mann-Whitney U-test (special case of a Kruskal-Wallis 
#             test) to test whether or not the data comes from two populations 
#             with the same distribution.


# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and creating a dataframe

# 1.1a - Read in the data
y <- c(49,43,31,44,34,35,47,52,40,53,
       73,68,64,70,46,68,65,32,33,39)

   # y

group <- factor(rep(c("A","B"),c(10,10)))

   # group



# 1.1b - Create a data frame
ex1.df <- data.frame(y=y, group=group)
ex1.df



# 1.1c - Delete vectors called 'y' and 'group' from R's memory
rm(y)
rm(group)







# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting a t-test and graphically assessing whether the conditions of a 
# t-test (i.e. independence, normality and homogeonous variance) are satisfied.

# 2.1a - Conducting the t-test
t.test(y ~ group, data=ex1.df, var.equal=T)

  # Result: t = -2.2733, df = 18, p-value = 0.03549


# 2.1b - Constructing a combined box-plot and dot-plot for the data
library(ggplot2)
ggplot(ex1.df, aes(x = y, y = group, fill="lightblue")) +
  geom_boxplot(alpha = 0.2) +
  geom_point(aes(colour = group), size = 3) +
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")


  ##  The graph produced in the stanza of code above indicates
  ##    i.  Asymmetry -> Data does not come from a normal distribution
  ##    ii. Unequal variance
  ##  From this we conclude that the assumptions of the t-test are not satisfied.
  ##  Consequently, the t-statistic does not follow a T_{n_A + n_B - 2} distribution
  ##  and we must view the p-value produced within the t-test above with extreme
  ##  scepticism.





# SECTION 3 #
#--------------------------------------------------------------------------------------------#
# Conducting a (non-parametric) Mann-Whitney U-test to determine whether the data come from 
# the same population (or different populations with the same distribution).

# 3.1a - Conducting the Mann-Whitney U-test. Note that the MW U-test is a 
# special case of the Kruskal-Wallis test when just two groups are present.
kruskal.test(y ~ group, data=ex1.df)

    ## Kruskal-Wallis chi-squared = 2.5219, df = 1, p-value = 0.1123


