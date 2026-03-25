# CHAPTER   7 - Bootstrapping

# PROGRAM   7.4 - Insurance Model (Bootstrapping Residuals Example)

# SUMMARY   Data on 1338 medical insurance policies issued in the USA were 
##          collected. There is interest in understanding the pattern of billing 
##          for medical costs by health insurance companies. The variables in 
##          the dataset are:
##
##          charges (Y): Individual medical costs billed by health insurance ($1000s)
##          age: Age of primary beneficiary
##          bmi: Body mass index (weight/height^2) of primary beneficiary
##          children: Number of dependent children covered by health insurance
##          sex: Gender of insurance contractor (female, male)
##          smoker: Current smoking status of primary beneficiary (yes, no)
##
##          We being this program by creating a linear model of charges as a
##          function of the other 5 variables (plus intercept) and extracting the
##          estimated regression coefficients.
##
##          We then show that the residuals of the model are not normally distributed
##          and as such we cannot appeal to the standard theory to estimate the
##          standard errors associated to the coefficients.
##
##          Instead, we estimate the standard error of the regression coefficients
##          by bootstrapping the residuals.



# Clear all variables before starting
rm(list = ls())






# SECTION 1 #
#------------------------------------------------------------------------------#
# Fitting a model and demonstrating non-normality of residuals

# 1.1a - Read in 'Insurance Data Set.csv' and checking it is stored as dataframe
ins <- read.csv("F:\\Lecture Courses\\STAT 432\\Additional Material\\R Data\\Chapter 7 - Bootstrapping\\Insurance Data Set.csv")
class(ins)


# 1.1b - Create the linear regression model
fit1 <- lm(charges~age + bmi + children + sex + smoker, data=ins)


# 1.1c - Obtain the coefficients from the model
beta.obs <- coef(fit1)
beta.obs

    ## Intercept  = -18.192844969
    ## age        =  0.009404807
    ## bmi        =  6.789264995
    ## children   =  4.982170995
    ## sex        =  1.522586654  if male,   0 if female
    ## smoker     =  34.420450541 if smoker, 0 if non-smoker




# 1.1d - Constructing a Residual vs Fitted plot and a QQ Plot to assess normality

  # Create two graphs side by side
  par(mfrow=c(1,2))
  
  # Create residual vs fitted value plot
  plot(fit1$res ~ fit1$fit, xlab="Fitted value", ylab="Residuals")
  abline(h=0,col="gray")
  
  # Create normal QQ-Plot of residuals
  qqnorm(fit1$res, main="")
  qqline(fit1$res)

    ## The residual plots show considerable departure of the residuals from 
    ## normality, particularly at the extremes of the distribution.
    ## We conclude that the residuals are not normally distributed and hence
    ## we cannot rely on normal theory to create standard errors or confidence
    ## intervals for the regression coefficients







# SECTION 2 #
#------------------------------------------------------------------------------#
# Bootstrapping of residuals to estimate standard error of regression coefficients


# 2.1a - Extract residuals
resid <- residuals(fit1)


# 2.1b - Extract fitted values
yhat <- fitted(fit1)


# 2.1c - Specify the number of variables inc intercept (p) and number of observations (n)
p<-ncol(model.matrix(fit1))
p

n<-length(yhat)
n




# 2.1d - Conducting bootstrapping procedure 

  # Specify the number of bootstrap samples
  B <- 1000
  
  # Create a blank matrix called 'betas' with B rows and p columns
  # i.e. each line will correspond to one set of beta coefficients produced
  # from the bootstrapping procedure
  betas <- matrix(NA, nrow = B, ncol = p)
  
  # Set seed to ensure we obtain the same bootstrap samples each time code is run
  set.seed(123)
  
  # For i=1,...,B do
  for (b in 1:B){
    
    # Create bootstrap residual vector by re-sampling with replacement from residual vector
    newresid <- sample(resid, size=n, replace=T)
    
    # Create new y-values
    newy <- yhat + newresid
    
    # Determine coefficients for the new model
    newfit <- lm(newy ~ age + bmi + children + sex + smoker, data=ins)
    
    # Append to bth row of the betas matrix
    betas[b,]<-coef(newfit)
  }


  
  
# 2.1e - Construct histograms of the coefficients produced
par(mfrow=c(2,3))
for (i in 1:p){  hist(betas[,i]) }

  ## All histograms are roughly symmetric and look normally distributed
  ## => Can use standard bootstrap confidence interval



# 2.1f - Calculate std error of regression coefficients
beta.se<-apply(betas, 2, sd)
beta.se


# 2.1g - Calculate standard CIs
  betaCI.LL <- beta.obs - 1.96*beta.se
  betaCI.UL <- beta.obs + 1.96*beta.se
  beta.CI <- paste("(",round(betaCI.LL,2),", ",round(betaCI.UL,2),")",sep="")



  

  
  
  
# SECTION 3 #
#------------------------------------------------------------------------------#
# Constructing a summary results table

# 3.1a - Print results table
  beta.df<-cbind(names(coef(fit1)),
                         round(beta.obs,2),round(beta.se,3),beta.CI)
  colnames(beta.df)<-c("Coefficient", "Estimate", "Std. Error", "95% CI")
  library(pander)
  pander(as.data.frame(beta.df,row.names = FALSE),caption="Bootstrap std errors and confidence intervals")


  

  





