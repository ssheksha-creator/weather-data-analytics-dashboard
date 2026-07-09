library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(DT)

ui <- bs4DashPage(
  
  title = "Weather Dashboard",
  
  # ---------------- HEADER ----------------
  header = bs4DashNavbar(
    title = div(
      style = "width:100%; display:flex; justify-content:center; align-items:center; gap:12px;",
      
      img(src = "logo.jpg", height = "35px"),
      
      span(
        "Capstone",
        style = "font-weight:600; font-size:20px;"
      )
    )
  ),
  
  # ---------------- SIDEBAR ----------------
  sidebar = bs4DashSidebar(
    skin = "light",   # IMPORTANT (for light/dark switching)
    
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Visualization", tabName = "unit1", icon = icon("chart-bar")),
      bs4SidebarMenuItem("R Concepts", tabName = "unit2", icon = icon("code")),
      bs4SidebarMenuItem("ggplot2", tabName = "unit3", icon = icon("chart-line")),
      bs4SidebarMenuItem("EDA", tabName = "unit4", icon = icon("chart-pie"))
    )
  ),
  
  # ---------------- BODY ----------------
  body = bs4DashBody(
    
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css")
    ),
    
    bs4TabItems(
      
      # ================= HOME =================
      bs4TabItem(tabName = "home",
                 
                 fluidRow(
                   
                   bs4Card(
                     title = "Upload Dataset",
                     width = 4,
                     status = "primary",
                     solidHeader = TRUE,
                     
                     fileInput("file", "Upload CSV", accept = ".csv"),
                     checkboxInput("header", "Header", TRUE)
                   ),
                   
                   bs4ValueBoxOutput("total_rows"),
                   bs4ValueBoxOutput("total_columns"),
                   bs4ValueBoxOutput("numeric_columns")
                 ),
                 
                 fluidRow(
                   bs4Card(title="Dataset Info", width=6, status="info", solidHeader=TRUE,
                           verbatimTextOutput("dataset_info")),
                   
                   bs4Card(title="Dataset Preview", width=6, status="primary", solidHeader=TRUE,
                           DT::dataTableOutput("data_preview"))
                 )
      ),
      
      # ================= UNIT 1 =================
      bs4TabItem(tabName = "unit1",
                 
                 fluidRow(
                   
                   bs4Card(title="Controls", width=4, status="warning", solidHeader=TRUE,
                           
                           selectInput("unit1_column","Column",choices=NULL),
                           
                           selectInput("unit1_graph","Graph Type",
                                       choices=c("Histogram","Bar Chart","Pie Chart","Line Chart","Box Plot",
                                                 "Density Plot","Scatter Plot","Heatmap","Biplot","Mosaic Plot")
                           ),
                           
                           selectInput("unit1_color","Color",
                                       choices=c("blue","red","green","orange","purple")
                           ),
                           
                           textInput("unit1_title","Title","Visualization"),
                           
                           actionButton("unit1_generate","Generate")
                   ),
                   
                   bs4Card(title="Output", width=8, status="primary", solidHeader=TRUE,
                           plotOutput("unit1_plot"))
                 )
      ),
      
      # ================= UNIT 2 =================
      bs4TabItem(tabName = "unit2",
                 
                 fluidRow(
                   
                   bs4Card(title="Structure", width=4, status="info", solidHeader=TRUE,
                           
                           selectInput("structure_type","Structure",
                                       choices=c("Vector","List","Matrix","Array","Data Frame","Factor","String")
                           )
                   ),
                   
                   bs4Card(title="Outputs", width=8, status="primary", solidHeader=TRUE,
                           verbatimTextOutput("variables_output"),
                           verbatimTextOutput("datatype_output"),
                           verbatimTextOutput("arithmetic_output"),
                           verbatimTextOutput("boolean_output"),
                           verbatimTextOutput("structure_output")
                   )
                 )
      ),
      
      # ================= UNIT 3 =================
      bs4TabItem(tabName = "unit3",
                 
                 fluidRow(
                   
                   bs4Card(title="Controls", width=4, status="warning", solidHeader=TRUE,
                           
                           selectInput("x_var","X",choices=NULL),
                           selectInput("y_var","Y",choices=NULL),
                           selectInput("color_var","Color",choices=NULL),
                           
                           sliderInput("point_size","Size",1,10,3),
                           
                           selectInput("geom_type","Graph",
                                       choices=c("Scatter Plot","Line Plot","Histogram","Bar Plot","Box Plot","Density Plot")
                           ),
                           
                           selectInput("facet_var","Facet",choices=c("None")),
                           
                           checkboxInput("add_regression","Regression"),
                           checkboxInput("add_smooth","Smooth"),
                           
                           selectInput("coord_type","Coordinate",choices=c("Cartesian","Flip","Polar")),
                           
                           selectInput("plot_theme","Theme",
                                       choices=c("Minimal","Classic","Dark","Light")
                           ),
                           
                           actionButton("generate_ggplot","Generate")
                   ),
                   
                   bs4Card(title="Output", width=8, status="primary", solidHeader=TRUE,
                           plotOutput("ggplot_output"))
                 )
      ),
      
      # ================= UNIT 4 =================
      bs4TabItem(tabName = "unit4",
                 
                 fluidRow(
                   
                   bs4Card(title="EDA Controls", width=4, status="info", solidHeader=TRUE,
                           checkboxInput("show_correlation","Correlation"),
                           checkboxInput("show_regression","Regression"),
                           checkboxInput("show_distribution","Distribution")
                   ),
                   
                   bs4Card(title="EDA Results", width=8, status="primary", solidHeader=TRUE,
                           verbatimTextOutput("dataset_overview"),
                           verbatimTextOutput("central_tendency"),
                           verbatimTextOutput("dispersion"),
                           verbatimTextOutput("skewness"),
                           verbatimTextOutput("missing_values"),
                           plotOutput("correlation_plot"),
                           plotOutput("regression_plot"),
                           plotOutput("normal_distribution")
                   )
                 )
      )
      
    )
  )
)