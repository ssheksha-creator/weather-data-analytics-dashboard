# ---------------------------------------------------
# DATA LOADER UTILITIES
# ---------------------------------------------------
# This file contains helper functions to load datasets
# used across the Shiny application.
# ---------------------------------------------------

# ---------------------------------------------------
# FUNCTION 1 : LOAD DEFAULT DATASET
# ---------------------------------------------------

load_default_dataset <- function(){
  
  data_path <- "data/weather_dataset_cleaned.csv"
  
  if(file.exists(data_path)){
    
    df <- read.csv(
      data_path,
      stringsAsFactors = FALSE
    )
    
    return(df)
    
  }else{
    
    stop("Default dataset not found in data/ folder")
    
  }
  
}

# ---------------------------------------------------
# FUNCTION 2 : LOAD USER UPLOADED DATASET
# ---------------------------------------------------

load_uploaded_dataset <- function(file_path, header = TRUE){
  
  if(!file.exists(file_path)){
    
    stop("Uploaded file does not exist")
    
  }
  
  df <- read.csv(
    file_path,
    header = header,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}

# ---------------------------------------------------
# FUNCTION 3 : VALIDATE DATASET
# ---------------------------------------------------

validate_dataset <- function(df){
  
  if(is.null(df) || nrow(df) == 0){
    
    stop("Dataset is empty")
    
  }
  
  if(ncol(df) < 2){
    
    stop("Dataset must contain at least two columns")
    
  }
  
  return(TRUE)
  
}

# ---------------------------------------------------
# FUNCTION 4 : GET NUMERIC COLUMNS
# ---------------------------------------------------

get_numeric_columns <- function(df){
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  return(numeric_cols)
  
}

# ---------------------------------------------------
# FUNCTION 5 : GET CATEGORICAL COLUMNS
# ---------------------------------------------------

get_categorical_columns <- function(df){
  
  cat_cols <- names(df)[sapply(df, function(x)
    is.character(x) || is.factor(x))]
  
  return(cat_cols)
  
}

# ---------------------------------------------------
# FUNCTION 6 : DATASET OVERVIEW
# ---------------------------------------------------

dataset_overview <- function(df){
  
  list(
    rows = nrow(df),
    columns = ncol(df),
    column_names = names(df),
    data_types = sapply(df, class)
  )
  
}