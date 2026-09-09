library(dplyr)
library(np)

# Code for assignment

df_A <- as.data.frame(samples_smok_data$A)
df_B <- as.data.frame(samples_smok_data$B)
df_C <- as.data.frame(samples_smok_data$C)

# OLS

run_my_ols <- function(dataset_element) {
  df <- as.data.frame(dataset_element)
  # Split the datasets into treated and nontreated
  
  df_treated <- df[df$smoking == 1, ]
  df_control <- df[df$smoking == 0, ]
  
  # Fit the models
  fit1 <- lm(severity ~ age + male, data = df_treated)
  fit0 <- lm(severity ~ age + male, data = df_control)
  
  # Predict outcomes of the scenarios
  yhat1 <- predict(fit1, newdata = df)
  yhat0 <- predict(fit0, newdata = df)
  
  # Compute the regression imputation (Risk difference)
  # (Estimated E[Y(1)-Y(0)])
  delta <- mean(yhat1 - yhat0)
  
  cat("\nEstimated RD:", delta, "\n")
  
  # Return the estimate
  return(delta = delta)
}

# Run the function for the different datasets (A, B & C)
ols_A <- run_my_ols(df_A)
ols_B <- run_my_ols(df_B)
ols_C <- run_my_ols(df_C)


# Logit

run_my_logit <- function(dataset_element) {
  df <- as.data.frame(dataset_element)
  
  # Split the datasets into treated and nontreated
  df_treated <- df[df$smoking == 1, ]
  df_control <- df[df$smoking == 0, ]
  
  # Fit the models
  fit1 <- glm(severity ~ age + male, data = df_treated, family = binomial)
  fit0 <- glm(severity ~ age + male, data = df_control, family = binomial)
  
  # Predict outcomes of the scenarios
  yhat1 <- predict(fit1, newdata = df, type = "response")
  yhat0 <- predict(fit0, newdata = df, type = "response")
  
  # Obtain the result from the RI
  delta <- mean(yhat1 - yhat0)
  
  cat("\nEstimated RD:", delta, "\n")
  
  # Return the estimate
  return(delta = delta)
}

# Run the function for the different datasets (A, B & C)
logit_A <- run_my_logit(df_A)
logit_B <- run_my_logit(df_B)
logit_C <- run_my_logit(df_C)



run_my_kernel <- function(df, regtype = "lc", nmulti = 10) {
  
  # Prepare the data
  df$smoking  <- as.factor(df$smoking)
  df$male     <- as.factor(df$male)
  df$severity <- as.numeric(df$severity)
  df$age      <- as.numeric(df$age)
  
  # Split the datasets into treated and nontreated
  df_treated <- df[df$smoking == 1, ]
  df_control <- df[df$smoking == 0, ]
  
  # Estimate the bandwidths for model treated with LOOCV to get the optimal bw
  bw1 <- np::npregbw(
    severity ~ male + age, 
    data = df_treated,
    regtype = regtype,
    bwmethod = "cv.ls",
    leave.one.out = TRUE,
    nmulti = nmulti
  )
  # Fit model 1
  model1 <- np::npreg(bws = bw1)
  
  # Estimate the bandwidths for model controls with LOOCV to get the optimal bw
  bw0 <- np::npregbw(
    severity ~ male + age,
    data = df_control,
    regtype = regtype,
    bwmethod = "cv.ls",
    leave.one.out = TRUE,
    nmulti = nmulti
  )
  # Fit model 0
  model0 <- np::npreg(bws = bw0)
  
  # Predict outcomes of the scenarios
  yhat1 <- predict(model1, newdata = df)
  yhat0 <- predict(model0, newdata = df)
  
  # Obtain the result from the RI
  delta  <- mean(yhat1 - yhat0)
  
  cat("\nEstimated RD:", delta, "\n")
  
  # Return the estimate
  return(delta = delta)
}

# Run the function for the different datasets (A, B & C)
kernel_A <- run_my_kernel(df_A)
kernel_B <- run_my_kernel(df_B)
kernel_C <- run_my_kernel(df_C)


# Comparission
cat("OLS RD:    ", round(ols_A, 4), round(ols_B, 4), round(ols_C, 4),"\n")
cat("Logit RD:  ", round(logit_A, 4), round(logit_B, 4), round(logit_C, 4), "\n")
cat("Kernel RD: ", round(kernel_A, 4), round(kernel_B, 4), round(kernel_C, 4), "\n")