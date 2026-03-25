# Clear all variables before starting
rm(list = ls())

# Calling required libraries
library(markovchain)
library(expm)


statenames <- c("1", "2", "3", "4" , "5")

P <- matrix(c(0.20, 0.30, 0.10, 0.30, 0.10,
              0.00, 0.10, 0.40, 0.20, 0.30,
              0.20, 0.20, 0.20, 0.20, 0.20,
              0.30, 0.20, 0.20, 0.10, 0.20,
              0.30, 0.20, 0.10, 0.20, 0.20),nrow=5,byrow=TRUE,
            dimnames=list(statenames,statenames))


P%^%1

P%^%2

P%^%3

P%^%4

P%^%5

P%^%6

P%^%7

P%^%8

P%^%9

P%^%10
