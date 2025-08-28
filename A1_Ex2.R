# Exercise 2 [20 Points]

## Setup

library(tidyverse)
set.seed(2025)

## Q1

X = list()
for(i in 1:100) {
  name = paste("n_", i, sep = '')
  vect = rnorm(5, 20, 10)
  X[[i]] = list(name, vect)
}

## Q2

str(X)

## Q3

V <- vector("numeric", 100L)
for(i in 1:100) {
  V[i] = sum(nth(X[[i]][2],1))
}

## Q4

M <- matrix(nrow = 0, ncol = 5)
for(i in 1:100) {
  M <- rbind(M, nth(X[[i]][2], 1))
}

## Q5

row <- 1
col <- 1
runsum <- 0
end <- FALSE

while(runsum < 4000 && end == FALSE) {
  runsum <- runsum + M[row,col]
  if(row < nrow(M)) {
    row <- row + 1
  } else if (col < ncol(M)) {
    row <- 1
    col <- col + 1
  } else {
    end <- TRUE
  }
}

cat("Column totals:", colSums(M), "\nRunning sum:", runsum, "\nNumber of columns:", col)
