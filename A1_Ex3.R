# Exercise 3

## mytest function

mytest <- function() {
  setup()
  csv_files <- list("A1_Ex3_not_signif.csv", "A1_Ex3_signif.csv")
  for(file in csv_files) {
    file_name <- read_csv(file, show_col_types = 0)
    readline(prompt="Press [enter] to continue")
    hypothesis(file_name)
    print(assumptions(file_name))
    fit1 <- fit(file_name)
    decision(fit1)
    conclusion(fit1)
  }
}

## Setup

setup <- function() {
  library(tidyverse)
  library(patchwork)
  library(broom)
  library(mycor)
}

## 1 Hypothesis

hypothesis <- function(file_name) {
  return(cat("Hypothesis for", deparse(substitute(file_name)), "\nH_0: β = 0 i.e. a linear model does not fit the data, against\nH_1: β ≠ 0 i.e. a linear model does fit the data\n"))
}

## 2 Assumptions

assumptions <- function(file_name) {
  model <- lm(Y~X, data = file_name)
  fit <- fitted(model)
  res <- residuals(model)
  results <- data.frame(fitted_vals = fit, residuals = res)

  p1 <- file_name |> 
    ggplot(aes(x = X, y = Y)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  p2 <- ggplot(data = results, aes(x = fitted_vals, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, colour = "blue", linetype = "dashed", linewidth = 0.7) +
    labs(x = "fitted values")
  
  p3 <- ggplot(results, aes(x = residuals)) +
    geom_histogram()
  
  out <- p1 / (p2 + p3)
  
  return(out)
}

## 3 Linear regression report

fit <- function(file_name) {
  model <- lm(Y~X, data = file_name)
  
  slope_est <- coef(model)["X"] # (β̂̂) slope estimate
  confint <- confint(model, parm = "X", level = 0.95) #  95% confidence interval
  t_val <- summary(model)$coefficients["X", "t value"]  # t statistic
  df <- model$df.residual # df
  p_val <- summary(model)$coefficients["X", "Pr(>|t|)"] #glance(model)[["p.value"]] # P value

  out <- list(
    slope_est,
    confint,
    t_val,
    df,
    p_val
  )
  
  cat("Slope estimate:", slope_est,
      "\nConfidence interval (95%):", sprintf("[%.6f, %.6f]", confint[1], confint[2]),
      "\nt statistic:", t_val,
      "\nDegrees of freedom:", df,
      "\nP value:", p_val)
  cat("\nP value:", p_val)
  
  return(out)
}

## 4 Decision

decision <- function(fit1) {
  if(p_val < 0.05) {
    dec <- "reject"
  }
  else {
    dex <- "not reject"
  }
  cat(dec)
}

## 5 Conclusion
  
conclusion <- function(fit1) {
  
}

# Reject the null hypothesis
