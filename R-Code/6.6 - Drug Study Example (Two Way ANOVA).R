# CHAPTER 6 - PERMUTATION TESTING

# PROGRAM   - 6.6 - Drug Study Example (Two Way ANOVA)

# SUMMARY   - In this program we look to determine whether:
#                 i.  Genotype (AA, Aa, aa)
#                 ii. Two different prescription drugs (A, B)
#             Have an effect on the level of chemical X within the blood streams
#             of patients. People with different genotypes at a certain gene may
#             differ in terms of the concentration of the chemical within their 
#             blood streams. As such, genotype must be controlled for first. We 
#             also test for the presence of an interaction effect between 
#             genotype and drug used.

#             The table below shows the concentration of the chemical in the 
#             blood two hours after taking a standard dose of Drug A or Drug B 
#             for people of each genotype.

#             (Drug = A, Genotype = AA) = {13,7,5,9,12,10,5,9}
#             (Drug = A, Genotype = Aa) = {11,12,9}
#             (Drug = A, Genotype = aa) = {16}
#             (Drug = B, Genotype = AA) = {6,8,9,12,9,5,10}
#             (Drug = B, Genotype = Aa) = {13,9,9,12}
#             (Drug = B, Genotype = aa) = {19}

# The code is structured as follows:
#
# Section 1 - Normal Theory two-way ANOVA
# Section 2 - Conduct permutation test for interaction effect
# Section 3 - Conduct permutation test for effect of genotype
# Section 4 - Conduct permutation test for effect of drug controlling genotype






# Clear all variables before starting
rm(list = ls())


# SECTION 1 #
#------------------------------------------------------------------------------#
# Conducting normal theory two way ANOVA to test for interaction, the effect of 
# genotype, then the effect of drug conditional on genotype.

# 1.1a - Read in the data and create a dataframe
chem <- c(13,7,5,9,12,10,5,9,11,12,9,16,
          6,8,9,12,9,5,10,13,9,9,12,19)

    chem


# 1.1b - Specify the first 12 obs to be drug A and the second 12 obs to be drug B
drug <- factor(rep(c("A","B"),c(12,12)))

   drug


# 1.1c - Specify which observations refer to which genotype
genotype <- as.factor(c(rep(c("AA","Aa","aa"),c(8,3,1)),
                        rep(c("AA","Aa","aa"),c(7,4,1))))

  genotype


#1.1d - Create data frame and show result
ex3.df <- data.frame(chem, drug, genotype)
  
  ex3.df

    
    
# 1.1e - Create ANOVA for factors genotype and drug with interaction terms
ex3.aov <- aov(chem ~ genotype + drug + genotype * drug, data=ex3.df)
summary(ex3.aov)


  # Note the difference in F-Statistics and P-values if factors entered into
  # the model in the opposite order
  ex3.aov1 <- aov(chem ~ drug + genotype + drug * genotype, data=ex3.df)
  summary(ex3.aov1)

  
    ## RESULTS TABLE
    ## -------------
    ##                Df  Sum Sq  Mean Sq   F value   Pr(>F)    
    ## genotype       2   145.43  72.71     11.621    0.000575
    ## drug           1   0.03    0.03      0.005     0.942930    
    ## genotype:drug  2   4.86    2.43      0.389     0.683480    
    ## Residuals      18  112.63  6.26  


# The results table above indicates:
#     - No evidence of interaction 
#     - Evidence for the effect of genotype on chemical X
#     - No evidence for the effect of drug on chemical X controlling for genotype

# However, the p-values produced are only valid if the usual two-way ANOVA
# conditions are satisfied (i.e. the data from all cells are independent,
# normally distributed and have constant variance). Since there is no way for
# us to tell whether these conditions are satisfied, we must perform additional
# additional permutation tests.






# SECTION 2 #
#------------------------------------------------------------------------------#
# Conduct permutation test for interaction effects

# 2.1a - Fit main effects model and extract residuals
ex3.df
chem.lm <- lm(chem ~ genotype + drug, data=ex3.df)
chem.res <- resid(chem.lm)
chem.res 


# 2.1b - Use residuals as response in a two-way model with interaction
res.lm <- lm(chem.res ~ genotype + drug + genotype * drug, data=ex3.df)
anova(res.lm)


# 2.1c - Extract initial F-statistic for interaction effect
data.F.res <- anova(res.lm)["genotype:drug", "F value"]
data.F.res
  
  

# 2.1d - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 2.1d1 - Define number of reordered samples to take
  replic <- 1000
  
  # 2.1d2 - Create blank vector called pseudo.F.res
  pseudo.F.res <- rep(NA,replic)
  
  # 2.1d3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
  # 2.1d4 - Construct "Do-Loop"
  for (i in 1:replic) 
    {
      # Construct reordered sample of residuals.
      res.perm <- sample(chem.res)
      
      # Construct linear model modelling reordered residuals as a function
      # of genotype, drug and any interaction between the two
      this.lm <- lm(res.perm ~ genotype + drug + genotype * drug, data=ex3.df)
      anova(this.lm)
      
      # Append F-Statistic to the ith element of the vector pseudo.F.res
      pseudo.F.res[i] <- anova(this.lm)["genotype:drug","F value"]
    }

  
  pseudo.F.res
  i <- 1
  chem.res
  res.perm
  ex3.df
  
  
# 2.1e - Construct histogram showing the generated distribution of test 
# statistics and the position of data.F.res
hist(pseudo.F.res)
lines(c(data.F.res, data.F.res),c(0,600),col="red",lwd=3)
  
# 2.1f - Determine p-value for interaction
length ( pseudo.F.res [ pseudo.F.res >= data.F.res  ] ) / replic

    ## p-value = 0.694
    ## No evidence for interaction effect between genotype and drug







# SECTION 3 #
#------------------------------------------------------------------------------#
# Conduct permutation test for effect of genotype alone on Chemical X

# 3.1a - Conducting one-way ANOVA for the effect of genotype of chemical X
ex3.df
geno.aov <- aov(chem ~ genotype, data=ex3.df)
summary(geno.aov)


# 3.1b - Extracting initial F-statistic
data.F.geno <- summary(geno.aov)[[1]]["genotype","F value"]
data.F.geno



# 3.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 3.1c1 - Define number of reordered samples to take
  replic1 <- 1000
  
  # 3.1c2 - Create blank vector called pseudo.F.geno
  pseudo.F.geno <- rep(NA,replic1)
  
  # 3.1c3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
  # 3.1c4 - Construct "Do-Loop"
  for (i in 1:replic1) 
    {
      # Construct reordered sample of categorical variables
      geno.perm <- sample(ex3.df$genotype)
     
      # Construct ANOVA model
      this.aov <- aov(chem ~ geno.perm , data=ex3.df)
      summary(this.aov)
      
      # Append F-Statistic to the ith element of the vector pseudo.F.geno
      pseudo.F.geno[i] <- summary(this.aov)[[1]]["geno.perm","F value"]
    }

  
  i <- 1
  ex3.df
  geno.perm
  pseudo.F.geno

  
# 3.1d - Construct histogram showing the generated distribution of test 
# statistics and the position of data.F.geno
max.x <- 1 + max(data.F.geno, max(pseudo.F.geno))
hist(pseudo.F.geno, xlim=c(0,max.x))
lines(c(data.F.geno, data.F.geno),c(0,600),col="red",lwd=3)
  
  
# 3.1e - Determine p-value for test
length ( pseudo.F.geno [ pseudo.F.geno >= data.F.geno  ] ) / replic1


    ## p-value = 0
    ## Very strong evidence for the effect of genotype on chemical X








# SECTION 4 #
#------------------------------------------------------------------------------#
# Conduct permutation test for effect of drug conditional on genotype  

# 4.1a - Conducting two-way ANOVA (without interaction) for the effect drug
# (conditional on genotype) on chemical X. 
ex3.df
drug.aov<-aov(chem ~ genotype + drug, data=ex3.df)
summary(drug.aov)



# 4.1b - Extracting initial F-statistic
data.F.drug <- summary(drug.aov)[[1]]["drug","F value"]
data.F.drug



# 4.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # 4.1c1 - Define number of reordered samples to take
  replic2 <- 1000
  
  # 4.1c2 - Create blank vector called pseudo.F.drug
  pseudo.F.drug <- rep(NA,replic2)
  
  # 4.1c3 - Set seed to ensure that the same reordered samples obtained each time
  set.seed(12345)
  
  # 4.1c4 - Construct "Do-Loop"
  for (i in 1:replic2) 
    {
      # Construct reordered sample of categorical variable drug, keeping
      # genotype unchanged
      drug.perm <- sample(ex3.df$drug)
      
      # Construct ANOVA model
      this.aov2 <- aov(chem ~ genotype + drug.perm , data=ex3.df)
      summary(this.aov2)
      
      # Append F-Statistic to the ith element of the vector pseudo.F.drug
      pseudo.F.drug[i] <- summary(this.aov2)[[1]]["drug.perm","F value"]
    }
 

  i <- 1
  ex3.df
  drug.perm
  pseudo.F.drug
  
  
# 4.1d - Construct histogram showing the generated distribution of test
# statistics and the position of data.F.geno    
hist(pseudo.F.drug)
lines(c(data.F.drug, data.F.drug),c(0,600),col="red",lwd=3) 


# 4.1e - Determine p-value for test
length ( pseudo.F.drug [ pseudo.F.drug >= data.F.drug  ] ) / replic2


## p-value = 0.971
## No evidence for the effect of drug on chemical X conditional on genotype







# CONCLUSION #
#--------------------------------------------------------------------------------------------#
# The ANOVA tests and permutation tests above yield the same conclusions.
# - No evidence for interaction effect between genotype and drug
# - Very strong evidence for the effect of genotype on chemical X
# - No evidence for the effect of drug on chemical X conditional on genotype


