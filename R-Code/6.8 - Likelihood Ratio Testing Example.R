# CHAPTER   6 - Permutation Testing

# PROGRAM   6.8 - Likelihood Ratio Testing Example

# SUMMARY   In this program we conduct a permutation test for regression
#           coefficients following the procedure outline in Section 6.8b of
#           the lecture notes. 
#
#           We have a response variable Y and five independent variables X1, X2,
#           X3, X4 & X5. Our initial model consists of variables X1, X2 & X3 and
#           we want to know whether variables X4 & X5 should be added to the
#           model GIVEN that X1, X2 & X3 are already contained in the model.
#
#           In other words, We wish to test between the following hypothesis:
#             H0 : Y ~ X1 + X2 + X3             (Reduced model)
#             H1 : Y ~ X1 + X2 + X3 + X4 + X5   (Complete model)
#
#           In Section 1 of the code we test between H0 and H1 using a normal
#           theory likelihood ratio test. However, we show that the conditions
#           of the test (i.e. that residuals are normally distributed) are
#           violated.
#
#           In Section 2 of the code we test between H0 and H1 using a
#           permutation based likelihood ratio test. 





# Clear all variables before starting
rm(list = ls())


# Calling required packages



# SECTION 1 #
#------------------------------------------------------------------------------#
# Conduct Normal Theory Likelihood Ratio Test


# 1.1a - Read in 'Regression Coefficients.csv' data set and analysing visually
Reg1a <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\R Data\\Chapter 6 - Permutation Testing\\Regression Coefficients.csv")
str(Reg1a)

  # Specify parameters of the data
  n <- nrow(Reg1a)
  p <- ncol(Reg1a)-1

  
  # Construct correlation plots
  pairs(Reg1a)
  

  
# 1.1b - Obtain SSE for complete model
Reg1b <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = Reg1a)
  Reg1b1 <- sum( resid(Reg1b)^2 )
  

# 1.1c - Obtain SSE for reduced model
Reg1c <- lm(Y ~ X1 + X2 + X3, data = Reg1a)
  Reg1c1 <- sum( resid(Reg1c)^2 )
  

# 1.1d - Obtain F-statistic for initial sample
Reg1d <-  ( (Reg1c1 - Reg1b1) / 2 ) / (Reg1b1 / (n - p)  )
Reg1d

    ## F_obs = 1.267167



# 1.1e - Obtain P-value for the test
1 - pf(Reg1d, 2, n-p  )

    ## p-value = 0.2863446
    ## No evidence against the null hypothesis that the reduced model is the
    ## true model. However, is the assumption that the residuals are normally
    ## distributed satisfied?


# 1.1f - Construct a QQ-Plot of residuals from the reduced model
qqnorm(Reg1c$res, main="")
qqline(Reg1c$res)  

    ## Based on the QQ-plot it would appear that residuals are not normally 
    ## distributed. As such, the p-value produced in the Section 1.1e must be
    ## viewed with extreme caution. A permutation test must therefore be
    ## conducted.







# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting a Permutation Based Likelihood Ratio Test

# 2.1a - Define number of reordered samples to take
L <- 1000

# 2.1b - Create blank vector called Reg1b
Reg2b <- rep(NA, L)

# 2.1c - Set seed to ensure that the same reordered samples obtained each time
set.seed(12345)


# 2.1d - Construct "Do-Loop"
for (i in 1:L) 
  {

  # 2.1d1 - Obtain data rows for reduced model and add a column called ID
  # specifying the row number
  Reg2d1 <- subset(Reg1a, select = c(Y, X1, X2, X3))
  Reg2d1$ID <- seq.int( nrow(Reg2d1) ) 
  
  # 2.1d2 - Obtain reordered list of data rows
  Reg2d2 <- sample(1:n, n, replace=F)
  
  # 2.1d3 - Obtain reordered data rows for variables X4 and X5 and add a column
  # called ID specifying the row number
  Reg2d3 <- Reg1a[Reg2d2,]
  Reg2d3 <- subset(Reg2d3, select = c(X4, X5))
  Reg2d3$ID <- seq.int( nrow(Reg2d3) )
  
  # 2.1d4 - Merge the Reg2d1 & Reg2d3 data frames by ID then drop this variable
  Reg2d4 <- merge(Reg2d1, Reg2d3, by = "ID")
  Reg2d4 <- subset(Reg2d4, select = -c(ID))
  
  # 2.1d5 - Obtain SSE for complete model
  Reg2d5 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = Reg2d4)
  Reg2d5 <- sum( resid(Reg2d5)^2 )
  
  # 2.1d6 - Obtain SSE for reduced model (should always be the same as Reg1c1)
  Reg2d6 <- lm(Y ~ X1 + X2 + X3, data = Reg1a)
  Reg2d6 <- sum( resid(Reg2d6)^2 )
  
  # 2.1d7 - Obtain F-statistic
  Reg2d7 <-  ( (Reg2d6 - Reg2d5) / 2 ) / (Reg2d5 / (n - p)  )
  
  # 2.1d8 - Append the F-Statistic to the ith element of the vector Reg2b
  Reg2b[i] <- Reg2d7
  
  }



# 2.1e - Construct histogram showing the generated distribution of test 
# statistics and the position of Reg1d
max.x <- 1 + max(Reg1d, max(Reg2b))
hist(Reg2b)
lines(c(Reg1d, Reg1d),c(0,650),col="red",lwd=3)

# 2.1f - Determine p-value for test
length ( Reg2b [ Reg2b >= Reg1d  ] ) / L

    ## p-value = 0.295
    ## No evidence against the null hypothesis that the reduced model is the
    ## correct model. Conclude that X4 and X5 should not be added to the model.

