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

## Q4

X |> 
  filter(month == "Jul") |>
  summarise(mean(demand))

mean(Y[['demand']])

## Q5

X |> 
  filter(temp < 15) |> 
  summarise(median(demand))

## Q6

h_dmd <- X |>
  arrange(desc(demand)) |> 
  head(1)

cat("Date of highest demand:", h_dmd$month, h_dmd$year)