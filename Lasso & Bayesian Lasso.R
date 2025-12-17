library(rstan)
library(rstanarm)
library(fastDummies)
library(readxl)
library(glmnet)
library(monomvn)
library(tidyverse)

data <- read_excel("AmesHousing.xlsx")
data <- na.omit(data)

data$log_SalePrice <- log(data$SalePrice)

# Create dummy variables for the selected categorical predictors
data_dummies <- dummy_cols(
  data,
  select_columns = c("Neighborhood", "Lot Shape", "House Style", "Roof Style"),
  remove_first_dummy = TRUE,      # drop one category from each to avoid multicollinearity
  remove_selected_columns = TRUE  # remove the original categorical columns
)

# Check the new dummy columns created
names(data_dummies)[grep("Neighborhood_", names(data_dummies))][1:5]
names(data_dummies)[grep("Lot.Shape_", names(data_dummies))]
names(data_dummies)[grep("House.Style_", names(data_dummies))]
names(data_dummies)[grep("Roof.Style_", names(data_dummies))]

# Prepare dataset
data_blasso <- data_dummies
data_blasso$log_SalePrice <- log(data$SalePrice)

# Remove any NA, Inf, or constant columns
data_blasso <- data_blasso %>%
  select(where(~ all(is.finite(.x)))) %>%
  select(where(~ sd(.x) > 0))

# Define outcome and predictors
y <- data_blasso$log_SalePrice
X <- as.matrix(select(data_blasso, -log_SalePrice))

# Standardize predictors for Bayesian Lasso stability
X <- scale(X)
y <- scale(y)

# Simulation setup
set.seed(123)
n <- nrow(X)
M <- 1000           
s <- floor(0.8 * n)
mse_lasso <- numeric(M)
mse_blasso <- numeric(M)

# Beauty contest loop
for (m in 1:M) {
  # 1. Split data
  idx <- sample(1:n, s, replace = FALSE)
  idx2 <- setdiff(1:n, idx)
  X_train <- X[idx, ]
  X_test <- X[idx2, ]
  y_train <- y[idx]
  y_test <- y[idx2]
  
  # 2. Fit Lasso
  cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
  pred_lasso <- predict(cv_lasso, X_test, s = "lambda.min")
  
  # 3. Fit Bayesian Lasso
  # Use a smaller T and remove intercept (X already standardized)
  b_lasso <- blasso(X_train, y_train, T = 500, thin = 2, RJ = FALSE, verb = 0)
  beta_hat <- colMeans(b_lasso$beta)
  pred_blasso <- as.vector(X_test %*% beta_hat)
  
  # 4. Compute MSE
  mse_lasso[m] <- mean((y_test - pred_lasso)^2)
  mse_blasso[m] <- mean((y_test - pred_blasso)^2)
}

# Summarize results
results <- data.frame(
  Model = c("Lasso", "Bayesian Lasso"),
  Mean_MSE = c(mean(mse_lasso), mean(mse_blasso)),
  SD_MSE = c(sd(mse_lasso), sd(mse_blasso))
)
results