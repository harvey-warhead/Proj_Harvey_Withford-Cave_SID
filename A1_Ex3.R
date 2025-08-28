# Exercise 3

## Setup

library(tidyverse)
library(patchwork)

data_sig <- read.csv("A1_Ex3_signif.csv")
data_nsig <- read.csv("A1_Ex3_not_signif.csv")

## 1 Hypothesis

hypothesis <- function(file_name) {
  cat("Hypothesis for", deparse(substitute(file_name)), "\nH_0: β = 0 i.e. a linear model does not fit the data, against\nH_1: β ≠ 0 i.e. a linear model does fit the data")
}

## 2 Assumptions

assumptions <- function(file_name) {
  model <- lm(file_name)
  fit <- fitted(model)
  res <- residuals(model)
  results <- data.frame(fitted_vals = fit, residuals = res)
}


p1 <- A1_Ex3_signif |> 
  ggplot(aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm")

p2 <- ggplot(data = results, aes(x = fitted_vals, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, aes(colour = "blue"))

p3 <- ggplot(results, aes(x = residuals)) +
  geom_histogram()

p1 / (p2 + p3)

## 3 Linear regression report

lm(A1_Ex)

the_model <- lm(Y~X, data = A1_Ex3_signif)

sum <- summary.lm(the_model)

coef(the_model)["X"] # the slope estimate
confint(the_model, parm = "X") #[1,2]  the confidence interval for the slope default level is .95
summary(the_model)$coefficients["X", "t value"]  # t value for the slope
the_model$df.residual #the degrees of freedom
summary(the_model)$coefficients["X", "Pr(>|t|)"] # the pvalue

## 4 Decision

## 5 Conclusion

# Reject the null hypothesis

