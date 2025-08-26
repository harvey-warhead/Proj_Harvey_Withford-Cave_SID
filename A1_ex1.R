# Exercise 1 [20 points]

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
  mean()

## Q5

X |> 
  filter(temp < 15) |> 
  median()

## Q6

X |>
  arrange(desc(demand)) |> 
  head(1)