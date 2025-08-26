# Exercise 1 [20 points]

## Setup

library(tidyverse)

## Q1

X <- read.csv("vic_elect.csv") |> 
  as.tibble()

## Q2

str(X)

## Q3

sum(X$demand)

## Q4 <- REVISE!

X |> 
  filter(month == "Jul") |> 
  mean()

## Q5 <- REVISE!

X |> 
  filter(temp < 15) |> 
  median()

## Q6

X |>
  arrange(desc(demand)) |> 
  head(1)