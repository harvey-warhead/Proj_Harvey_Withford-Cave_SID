# Exercise 3

# Attempting a distinction/high distinction solution
# HD: Use of a single function to call the test functions
# D: No use of mylm

## Run function mytest()

#BEFORE RUNNING:
#ensure files "A1_Ex3_signif.csv" and "A1_Ex3_not_signif.csv"
#are within the project directory
#AND all packages referenced by setup() function are installed

mytest(list("A1_Ex3_signif.csv", "A1_Ex3_not_signif.csv"))

#more files can be added to this list

## mytest function

mytest <- function(csv_files) {
  setup()
  for(file in csv_files) {
    file_name <- read_csv(file, show_col_types = 0)
    readline(prompt=cat("================================",
                        "\nTEST DATA:", file, "\nPress [enter] to continue"))
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
  return(cat("HYPOTHESIS: \nH_0: β = 0 i.e. a linear model does not fit the data, against\nH_1: β ≠ 0 i.e. a linear model does fit the data"))
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
    geom_smooth(method = "lm", formula = 'y~x')
  
  p2 <- ggplot(data = results, aes(x = fitted_vals, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, colour = "blue", linetype = "dashed", linewidth = 0.7) +
    labs(x = "fitted values")
  
  p3 <- ggplot(results, aes(x = residuals)) +
    geom_histogram(bins = 30)
  
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
  p_val <- glance(model)[["p.value"]] # P value
  rsq <- glance(model)[["r.squared"]] # I want r squared values in addition to assess the strength of the correlation

  out <- list(
    slope_est,
    confint,
    t_val,
    df,
    p_val,
    rsq
  )
  
  cat("\nREPORT:",
      "\nSlope estimate:", slope_est,
      "\nConfidence interval (95%):", sprintf("[%.6f, %.6f]", confint[1], confint[2]),
      "\nt statistic:", t_val,
      "\nDegrees of freedom:", df,
      "\nP value:", p_val)

  return(out)
}

## 4 Decision

decision <- function(fit1) {
  if(fit1[[5]] < 0.05) {
    return(cat("\nDECISION:\nReject null hypothesis at 5% level, as", fit1[[5]], "< 0.05."))
  }
  else {
    return(cat("\nDECISION:\nCannot reject null hypothesis at 5% level, as", fit1[[5]], "> 0.05."))
  }
}

## 5 Conclusion
  
conclusion <- function(fit1) {
  strength <- case_when(fit1[[6]] > 0.8 ~ "very strong", # indicates strength of
                        fit1[[6]] > 0.6 ~ "strong",      # correlation based on
                        fit1[[6]] > 0.4 ~ "medium",      # r squared values
                        fit1[[6]] > 0.2 ~ "weak",
                        fit1[[6]] > 0 ~ "very weak"
  )
  if(fit1[[5]] < 0.05) {
    result <- "is a linear relationship between X and Y"
  }
  else {
    result <- "is no relationship between X and Y"
  }
  return(cat("\nCONCLUSION:\nWe can endorse the above decision based on both the confidence interval",
             sprintf("[%.6f, %.6f]", fit1[[2]][1], fit1[[2]][2]),
             "and the P value", fit1[[5]], ".",
             "\nThe linear correlation between X and Y is", strength,
             "given an r squared value of", fit1[[6]], ".",
             "\nThe data therefore suggests that there", result,
             "with a slope estimate β = ", fit1[[1]],
             ".\n"))
}
