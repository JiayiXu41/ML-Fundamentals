# Load package
library(BMS)
library(rstan)
library(rstanarm)
library(fastDummies)
library(readxl)
data <- read_excel("AmesHousing.xlsx") 
data <- na.omit(data)

# Summary statistics
summary(data)
summary(data$SalePrice)

# Log-transform the SalePrice
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
data_bma <- data_dummies
data_bma$log_SalePrice <- log(data$SalePrice)

# Select predictors: include house size, quality, bathrooms, garage, and dummies
predictors <- c(
  "Gr Liv Area", "Total Bsmt SF", "Overall Qual", 
  "Full Bath", "Half Bath", "Garage Cars",
  grep("^Neighborhood_", names(data_bma), value = TRUE),
  grep("^Lot Shape_", names(data_bma), value = TRUE),
  grep("^House Style_", names(data_bma), value = TRUE),
  grep("^Roof Style_", names(data_bma), value = TRUE)
)

# Subset data for BMS
bma_data <- data_bma[, c("log_SalePrice", predictors)]
bma_data <- as.data.frame(lapply(bma_data, function(x) {
  if (is.logical(x) | is.factor(x) | is.character(x)) as.numeric(x) else x
}))

# Run Bayesian Model Averaging with g-prior (g = n)
bma_model <- bms(
  bma_data,
  burn = 10000,             
  iter = 50000,              
  g = nrow(data_bma),        
  mprior = "uniform",        
  nmodel = 5000             
)

# Summarize results
summary(bma_model)