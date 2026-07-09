# ------------------------------------------------------
# EDA FUNCTIONS
# ------------------------------------------------------
# This file contains helper functions for performing
# Exploratory Data Analysis on datasets.
# ------------------------------------------------------

# ------------------------------------------------------
# FUNCTION 1 : DATASET OVERVIEW
# ------------------------------------------------------

dataset_overview <- function(df){
  
  list(
    rows = nrow(df),
    columns = ncol(df),
    column_names = names(df),
    data_types = sapply(df, class)
  )
  
}

# ------------------------------------------------------
# FUNCTION 2 : CENTRAL TENDENCY
# ------------------------------------------------------

calculate_central_tendency <- function(column){
  
  result <- list(
    mean = mean(column, na.rm = TRUE),
    median = median(column, na.rm = TRUE),
    mode = names(sort(table(column), decreasing = TRUE))[1],
    minimum = min(column, na.rm = TRUE),
    maximum = max(column, na.rm = TRUE)
  )
  
  return(result)
  
}

# ------------------------------------------------------
# FUNCTION 3 : DISPERSION
# ------------------------------------------------------

calculate_dispersion <- function(column){
  
  result <- list(
    variance = var(column, na.rm = TRUE),
    standard_deviation = sd(column, na.rm = TRUE),
    range = range(column, na.rm = TRUE)
  )
  
  return(result)
  
}

# ------------------------------------------------------
# FUNCTION 4 : SKEWNESS
# ------------------------------------------------------

calculate_skewness <- function(column){
  
  mean_val <- mean(column, na.rm = TRUE)
  median_val <- median(column, na.rm = TRUE)
  
  if(mean_val > median_val){
    
    return("Positively Skewed")
    
  }else if(mean_val < median_val){
    
    return("Negatively Skewed")
    
  }else{
    
    return("Symmetric Distribution")
    
  }
  
}

# ------------------------------------------------------
# FUNCTION 5 : MISSING VALUES
# ------------------------------------------------------

missing_values_summary <- function(df){
  
  colSums(is.na(df))
  
}

# ------------------------------------------------------
# FUNCTION 6 : CORRELATION MATRIX
# ------------------------------------------------------

calculate_correlation <- function(df){
  
  numeric_data <- df[sapply(df, is.numeric)]
  
  if(ncol(numeric_data) < 2){
    
    return(NULL)
    
  }
  
  cor(numeric_data)
  
}

# ------------------------------------------------------
# FUNCTION 7 : REGRESSION MODEL
# ------------------------------------------------------

calculate_regression <- function(df){
  
  numeric_data <- df[sapply(df, is.numeric)]
  
  if(ncol(numeric_data) < 2){
    
    return(NULL)
    
  }
  
  x <- numeric_data[[1]]
  y <- numeric_data[[2]]
  
  model <- lm(y ~ x)
  
  return(model)
  
}

# ------------------------------------------------------
# FUNCTION 8 : NORMAL DISTRIBUTION DATA
# ------------------------------------------------------

normal_distribution_data <- function(column){
  
  list(
    mean = mean(column, na.rm = TRUE),
    sd = sd(column, na.rm = TRUE)
  )
  
}