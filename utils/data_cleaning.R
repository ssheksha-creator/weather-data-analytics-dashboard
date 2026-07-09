# --------------------------------------------------------
# DATA CLEANING UTILITIES
# --------------------------------------------------------
# This file contains helper functions used to clean and
# preprocess datasets before analysis and visualization.
# --------------------------------------------------------

# --------------------------------------------------------
# FUNCTION 1 : REMOVE MISSING VALUES
# --------------------------------------------------------

remove_missing_values <- function(df){
  
  df_clean <- na.omit(df)
  
  return(df_clean)
  
}

# --------------------------------------------------------
# FUNCTION 2 : REPLACE MISSING VALUES
# --------------------------------------------------------

replace_missing_values <- function(df){
  
  for(col in names(df)){
    
    if(is.numeric(df[[col]])){
      
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      
    }else{
      
      df[[col]][is.na(df[[col]])] <- "Unknown"
      
    }
    
  }
  
  return(df)
  
}

# --------------------------------------------------------
# FUNCTION 3 : REMOVE DUPLICATE ROWS
# --------------------------------------------------------

remove_duplicates <- function(df){
  
  df_unique <- unique(df)
  
  return(df_unique)
  
}

# --------------------------------------------------------
# FUNCTION 4 : DETECT OUTLIERS
# --------------------------------------------------------

detect_outliers <- function(df){
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  outlier_list <- list()
  
  for(col in numeric_cols){
    
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    
    IQR_value <- IQR(df[[col]], na.rm = TRUE)
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    outliers <- df[[col]][df[[col]] < lower_bound |
                            df[[col]] > upper_bound]
    
    outlier_list[[col]] <- outliers
    
  }
  
  return(outlier_list)
  
}

# --------------------------------------------------------
# FUNCTION 5 : CONVERT CHARACTER TO FACTOR
# --------------------------------------------------------

convert_to_factor <- function(df){
  
  char_cols <- names(df)[sapply(df, is.character)]
  
  for(col in char_cols){
    
    df[[col]] <- as.factor(df[[col]])
    
  }
  
  return(df)
  
}

# --------------------------------------------------------
# FUNCTION 6 : STANDARDIZE COLUMN NAMES
# --------------------------------------------------------

standardize_column_names <- function(df){
  
  names(df) <- tolower(names(df))
  
  names(df) <- gsub(" ", "_", names(df))
  
  return(df)
  
}

# --------------------------------------------------------
# FUNCTION 7 : DATA CLEANING PIPELINE
# --------------------------------------------------------

clean_dataset <- function(df){
  
  df <- standardize_column_names(df)
  
  df <- remove_duplicates(df)
  
  df <- replace_missing_values(df)
  
  df <- convert_to_factor(df)
  
  return(df)
  
}