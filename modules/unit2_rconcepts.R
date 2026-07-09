library(shiny)

unit2_rconcepts_server <- function(input, output, data){
  
  # -----------------------------------------
  # VARIABLES EXAMPLE
  # -----------------------------------------
  
  output$variables_output <- renderPrint({
    
    temperature <- 30
    humidity <- 70
    
    cat("Example Variables:\n")
    cat("temperature =", temperature,"\n")
    cat("humidity =", humidity,"\n")
    
  })
  
  # -----------------------------------------
  # DATA TYPES
  # -----------------------------------------
  
  output$datatype_output <- renderPrint({
    
    req(data())
    
    df <- data()
    
    cat("Column Data Types:\n\n")
    
    print(sapply(df,class))
    
  })
  
  # -----------------------------------------
  # ARITHMETIC OPERATIONS
  # -----------------------------------------
  
  output$arithmetic_output <- renderPrint({
    
    a <- 10
    b <- 5
    
    cat("Addition:", a+b,"\n")
    cat("Subtraction:", a-b,"\n")
    cat("Multiplication:", a*b,"\n")
    cat("Division:", a/b,"\n")
    cat("Modulus:", a%%b,"\n")
    
  })
  
  # -----------------------------------------
  # BOOLEAN OPERATORS
  # -----------------------------------------
  
  output$boolean_output <- renderPrint({
    
    x <- 10
    y <- 5
    
    cat("x > y :", x > y,"\n")
    cat("x < y :", x < y,"\n")
    cat("x == y :", x == y,"\n")
    
    cat("AND (x>5 & y>2):", x>5 & y>2,"\n")
    cat("OR (x>5 | y>20):", x>5 | y>20,"\n")
    cat("NOT !(x>5):", !(x>5),"\n")
    
  })
  
  # -----------------------------------------
  # DATA STRUCTURES
  # -----------------------------------------
  
  output$structure_output <- renderPrint({
    
    req(data())
    
    df <- data()
    
    if(input$structure_type == "Vector"){
      
      vec <- as.vector(df[[1]])
      print(vec)
      
    }
    
    else if(input$structure_type == "List"){
      
      lst <- as.list(df)
      print(lst)
      
    }
    
    else if(input$structure_type == "Matrix"){
      
      mat <- as.matrix(df)
      print(mat)
      
    }
    
    else if(input$structure_type == "Array"){
      
      arr <- array(unlist(df))
      print(arr)
      
    }
    
    else if(input$structure_type == "Data Frame"){
      
      print(df)
      
    }
    
    else if(input$structure_type == "Factor"){
      
      fac <- as.factor(df[[1]])
      print(fac)
      
    }
    
    else if(input$structure_type == "String"){
      
      str <- as.character(df[[1]])
      print(str)
      
    }
    
  })
  
}