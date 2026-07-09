library(shiny)

unit4_eda_server <- function(input, output, data){
  
  # --------------------------------------
  # DATASET OVERVIEW
  # --------------------------------------
  
  output$dataset_overview <- renderPrint({
    
    req(data())
    
    df <- data()
    
    cat("Rows:", nrow(df), "\n")
    cat("Columns:", ncol(df), "\n\n")
    
    cat("Column Names:\n")
    print(names(df))
    
    cat("\nData Types:\n")
    print(sapply(df,class))
    
  })
  
  # --------------------------------------
  # CENTRAL TENDENCY
  # --------------------------------------
  
  output$central_tendency <- renderPrint({
    
    req(data())
    
    df <- data()
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data)==0){
      cat("No numeric columns available")
      return()
    }
    
    col <- num_data[[1]]
    
    cat("Mean:", mean(col,na.rm=TRUE),"\n")
    cat("Median:", median(col,na.rm=TRUE),"\n")
    
    mode_val <- names(sort(table(col),decreasing=TRUE))[1]
    cat("Mode:", mode_val,"\n")
    
    cat("Minimum:", min(col,na.rm=TRUE),"\n")
    cat("Maximum:", max(col,na.rm=TRUE),"\n")
    
  })
  
  # --------------------------------------
  # DISPERSION
  # --------------------------------------
  
  output$dispersion <- renderPrint({
    
    req(data())
    
    df <- data()
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data)==0){
      cat("No numeric columns available")
      return()
    }
    
    col <- num_data[[1]]
    
    cat("Variance:", var(col,na.rm=TRUE),"\n")
    cat("Standard Deviation:", sd(col,na.rm=TRUE),"\n")
    cat("Range:", range(col,na.rm=TRUE),"\n")
    
  })
  
  # --------------------------------------
  # SKEWNESS
  # --------------------------------------
  
  output$skewness <- renderPrint({
    
    req(data())
    
    df <- data()
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data)==0){
      cat("No numeric columns available")
      return()
    }
    
    col <- num_data[[1]]
    
    mean_val <- mean(col,na.rm=TRUE)
    median_val <- median(col,na.rm=TRUE)
    
    if(mean_val > median_val){
      
      cat("Distribution: Positively Skewed")
      
    }
    
    else if(mean_val < median_val){
      
      cat("Distribution: Negatively Skewed")
      
    }
    
    else{
      
      cat("Distribution: Symmetric")
      
    }
    
  })
  
  # --------------------------------------
  # MISSING VALUES
  # --------------------------------------
  
  output$missing_values <- renderPrint({
    
    req(data())
    
    df <- data()
    
    cat("Missing Values Per Column:\n")
    print(colSums(is.na(df)))
    
  })
  
  # --------------------------------------
  # CORRELATION MATRIX
  # --------------------------------------
  
  output$correlation_plot <- renderPlot({
    
    req(data())
    req(input$show_correlation)
    
    df <- data()
    
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data) < 2){
      plot.new()
      text(0.5,0.5,"Not enough numeric columns")
      return()
    }
    
    cor_matrix <- cor(num_data)
    
    heatmap(cor_matrix)
    
  })
  
  # --------------------------------------
  # REGRESSION MODEL
  # --------------------------------------
  
  output$regression_plot <- renderPlot({
    
    req(data())
    req(input$show_regression)
    
    df <- data()
    
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data) < 2){
      plot.new()
      text(0.5,0.5,"Not enough numeric columns")
      return()
    }
    
    x <- num_data[[1]]
    y <- num_data[[2]]
    
    plot(x,y,
         col="blue",
         main="Regression Model",
         xlab=names(num_data)[1],
         ylab=names(num_data)[2])
    
    abline(lm(y~x),col="red")
    
  })
  
  # --------------------------------------
  # NORMAL DISTRIBUTION
  # --------------------------------------
  
  output$normal_distribution <- renderPlot({
    
    req(data())
    req(input$show_distribution)
    
    df <- data()
    
    num_data <- df[sapply(df,is.numeric)]
    
    if(ncol(num_data)==0){
      plot.new()
      text(0.5,0.5,"No numeric data available")
      return()
    }
    
    hist(num_data[[1]],
         col="skyblue",
         main="Normal Distribution",
         probability=TRUE)
    
    lines(density(num_data[[1]]),
          col="red")
    
  })
  
}