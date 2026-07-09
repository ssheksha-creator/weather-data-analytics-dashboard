# -----------------------------------------------------
# REPORT GENERATOR
# -----------------------------------------------------
# This file generates EDA reports and saves them
# inside reports/eda_reports folder
# -----------------------------------------------------

generate_eda_report <- function(df){
  
  # Ensure report folder exists
  dir.create("reports/eda_reports", recursive = TRUE, showWarnings = FALSE)
  
  report_path <- "reports/eda_reports/eda_report.txt"
  
  sink(report_path)
  
  cat("Exploratory Data Analysis Report\n")
  cat("---------------------------------\n\n")
  
  cat("Dataset Overview\n")
  cat("----------------\n")
  
  cat("Number of Rows:", nrow(df), "\n")
  cat("Number of Columns:", ncol(df), "\n\n")
  
  cat("Column Names:\n")
  print(names(df))
  
  cat("\n\nSummary Statistics\n")
  cat("------------------\n")
  
  print(summary(df))
  
  cat("\n\nMissing Values\n")
  cat("------------------\n")
  
  print(colSums(is.na(df)))
  
  cat("\n\nCorrelation Matrix\n")
  cat("------------------\n")
  
  numeric_data <- df[sapply(df, is.numeric)]
  
  if(ncol(numeric_data) >= 2){
    print(cor(numeric_data))
  }else{
    cat("Not enough numeric columns for correlation\n")
  }
  
  sink()
  
  return(report_path)
  
}