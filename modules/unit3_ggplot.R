library(shiny)
library(ggplot2)

unit3_ggplot_server <- function(input, output, data){
  
  output$ggplot_output <- renderPlot({
    
    req(data())
    req(input$generate_ggplot)
    
    df <- data()
    
    p <- NULL
    
    # -----------------------------------
    # GEOMETRIC OBJECTS
    # -----------------------------------
    
    if(input$geom_type == "Scatter Plot"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var,
                             y=input$y_var,
                             color=input$color_var)) +
        geom_point(size=input$point_size)
      
    }
    
    else if(input$geom_type == "Line Plot"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var,
                             y=input$y_var,
                             color=input$color_var)) +
        geom_line()
      
    }
    
    else if(input$geom_type == "Histogram"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var)) +
        geom_histogram(fill="skyblue")
      
    }
    
    else if(input$geom_type == "Bar Plot"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var,
                             fill=input$color_var)) +
        geom_bar()
      
    }
    
    else if(input$geom_type == "Box Plot"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var,
                             y=input$y_var,
                             fill=input$color_var)) +
        geom_boxplot()
      
    }
    
    else if(input$geom_type == "Density Plot"){
      
      p <- ggplot(df,
                  aes_string(x=input$x_var,
                             fill=input$color_var)) +
        geom_density(alpha=0.5)
      
    }
    
    # -----------------------------------
    # STATISTICAL TRANSFORMATIONS
    # -----------------------------------
    
    if(input$add_regression){
      
      p <- p + geom_smooth(method="lm")
      
    }
    
    if(input$add_smooth){
      
      p <- p + geom_smooth()
      
    }
    
    # -----------------------------------
    # FACETS
    # -----------------------------------
    
    if(input$facet_var != "None"){
      
      p <- p + facet_wrap(as.formula(paste("~",input$facet_var)))
      
    }
    
    # -----------------------------------
    # COORDINATE SYSTEM
    # -----------------------------------
    
    if(input$coord_type == "Flip"){
      
      p <- p + coord_flip()
      
    }
    
    else if(input$coord_type == "Polar"){
      
      p <- p + coord_polar()
      
    }
    
    # -----------------------------------
    # GRAPHICS FOR COMMUNICATION
    # -----------------------------------
    
    p <- p +
      labs(
        title=input$plot_title,
        subtitle=input$plot_subtitle,
        caption=input$plot_caption,
        x=input$x_label,
        y=input$y_label
      )
    
    # -----------------------------------
    # THEMES
    # -----------------------------------
    
    if(input$plot_theme == "Minimal"){
      
      p <- p + theme_minimal()
      
    }
    
    else if(input$plot_theme == "Classic"){
      
      p <- p + theme_classic()
      
    }
    
    else if(input$plot_theme == "Dark"){
      
      p <- p + theme_dark()
      
    }
    
    else if(input$plot_theme == "Light"){
      
      p <- p + theme_light()
      
    }
    
    print(p)
    
  })
  
}