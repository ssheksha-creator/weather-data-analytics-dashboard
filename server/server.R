library(shiny)
library(bs4Dash)
library(DT)

source("utils/data_loader.R")
source("utils/data_cleaning.R")
source("utils/eda_functions.R")
source("utils/visualization_functions.R")
source("utils/report_generator.R")

source("modules/unit1_visualization.R")
source("modules/unit2_rconcepts.R")
source("modules/unit3_ggplot.R")
source("modules/unit4_eda.R")

dir.create("plots/saved_graphs", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/eda_reports", recursive = TRUE, showWarnings = FALSE)

default_dataset <- load_default_dataset()

server <- function(input, output, session){
  
  data <- reactive({
    
    if(!is.null(input$file)){
      df <- load_uploaded_dataset(input$file$datapath, input$header)
    } else {
      df <- default_dataset
    }
    
    df <- clean_dataset(df)
    validate_dataset(df)
    df
  })
  
  # VALUE BOXES
  output$total_rows <- renderbs4ValueBox({
    bs4ValueBox(nrow(data()), "Rows", icon=icon("database"), color="primary")
  })
  
  output$total_columns <- renderbs4ValueBox({
    bs4ValueBox(ncol(data()), "Columns", icon=icon("table"), color="success")
  })
  
  output$numeric_columns <- renderbs4ValueBox({
    bs4ValueBox(sum(sapply(data(), is.numeric)), "Numeric", icon=icon("chart-bar"), color="warning")
  })
  
  # DATA
  output$dataset_info <- renderPrint({ summary(data()) })
  
  output$data_preview <- DT::renderDataTable({
    data()
  }, options = list(scrollX=TRUE, pageLength=5))
  
  # UPDATE INPUTS
  observe({
    df <- data()
    updateSelectInput(session,"unit1_column",choices=names(df))
    updateSelectInput(session,"x_var",choices=names(df))
    updateSelectInput(session,"y_var",choices=names(df))
    updateSelectInput(session,"color_var",choices=names(df))
    updateSelectInput(session,"facet_var",choices=c("None",names(df)))
  })
  
  # REPORT
  observe({
    req(data())
    generate_eda_report(data())
  })
  
  # MODULES
  unit1_visualization_server(input, output, data)
  unit2_rconcepts_server(input, output, data)
  unit3_ggplot_server(input, output, data)
  unit4_eda_server(input, output, data)
  
}