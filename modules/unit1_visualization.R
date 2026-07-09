library(shiny)
library(ggplot2)

unit1_visualization_server <- function(input, output, data){
  
  # ---------------------------------------
  # GRAPH GENERATOR
  # ---------------------------------------
  
  output$unit1_plot <- renderPlot({
    
    req(data())
    req(input$unit1_graph)
    
    df <- data()
    
    col <- df[[input$unit1_column]]
    
    title <- input$unit1_title
    color <- input$unit1_color
    
    # -----------------------
    # HISTOGRAM
    # -----------------------
    
    if(input$unit1_graph == "Histogram"){
      
      hist(col,
           col=color,
           main=title,
           xlab=input$unit1_column)
      
    }
    
    # -----------------------
    # BAR GRAPH
    # -----------------------
    
    else if(input$unit1_graph == "Bar Chart"){
      
      barplot(table(col),
              col=color,
              main=title)
      
    }
    
    # -----------------------
    # PIE CHART
    # -----------------------
    
    else if(input$unit1_graph == "Pie Chart"){
      
      pie(table(col),
          col=rainbow(length(unique(col))),
          main=title)
      
    }
    
    # -----------------------
    # LINE CHART
    # -----------------------
    
    else if(input$unit1_graph == "Line Chart"){
      
      plot(col,
           type="l",
           col=color,
           main=title,
           ylab=input$unit1_column)
      
    }
    
    # -----------------------
    # BOX PLOT
    # -----------------------
    
    else if(input$unit1_graph == "Box Plot"){
      
      boxplot(col,
              col=color,
              main=title)
      
    }
    
    # -----------------------
    # DENSITY PLOT
    # -----------------------
    
    else if(input$unit1_graph == "Density Plot"){
      
      plot(density(col),
           col=color,
           main=title)
      
    }
    
    # -----------------------
    # SCATTER PLOT
    # -----------------------
    
    else if(input$unit1_graph == "Scatter Plot"){
      
      num_cols <- names(df)[sapply(df,is.numeric)]
      
      if(length(num_cols) >= 2){
        
        plot(df[[num_cols[1]]],
             df[[num_cols[2]]],
             col=color,
             main=title,
             xlab=num_cols[1],
             ylab=num_cols[2])
        
      }
      
    }
    
    # -----------------------
    # HEATMAP
    # -----------------------
    
    else if(input$unit1_graph == "Heatmap"){
      
      num_data <- df[sapply(df,is.numeric)]
      
      cor_matrix <- cor(num_data)
      
      heatmap(cor_matrix)
      
    }
    
    # -----------------------
    # BIPLOT
    # -----------------------
    
    else if(input$unit1_graph == "Biplot"){
      
      num_data <- df[sapply(df,is.numeric)]
      
      pca <- prcomp(num_data, scale=TRUE)
      
      biplot(pca)
      
    }
    
    # -----------------------
    # MOSAIC PLOT
    # -----------------------
    
    else if(input$unit1_graph == "Mosaic Plot"){
      
      cat_cols <- names(df)[sapply(df,is.factor)]
      
      if(length(cat_cols)>=2){
        
        mosaicplot(table(df[[cat_cols[1]]],
                         df[[cat_cols[2]]]),
                   color=TRUE)
        
      }
      
    }
    
  })
  
}