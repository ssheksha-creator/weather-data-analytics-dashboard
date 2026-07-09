# -------------------------------------------------------
# VISUALIZATION FUNCTIONS
# -------------------------------------------------------
# This file contains reusable visualization functions
# used across Unit-1 and Unit-3 modules.
# -------------------------------------------------------

library(ggplot2)

# -------------------------------------------------------
# HISTOGRAM
# -------------------------------------------------------

create_histogram <- function(column, title="Histogram", color="skyblue"){
  
  hist(
    column,
    col=color,
    main=title,
    xlab="Values"
  )
  
}

# -------------------------------------------------------
# BAR CHART
# -------------------------------------------------------

create_bar_chart <- function(column, title="Bar Chart", color="orange"){
  
  barplot(
    table(column),
    col=color,
    main=title
  )
  
}

# -------------------------------------------------------
# PIE CHART
# -------------------------------------------------------

create_pie_chart <- function(column, title="Pie Chart"){
  
  pie(
    table(column),
    col=rainbow(length(unique(column))),
    main=title
  )
  
}

# -------------------------------------------------------
# LINE CHART
# -------------------------------------------------------

create_line_chart <- function(column, title="Line Chart", color="blue"){
  
  plot(
    column,
    type="l",
    col=color,
    main=title,
    ylab="Values"
  )
  
}

# -------------------------------------------------------
# BOX PLOT
# -------------------------------------------------------

create_box_plot <- function(column, title="Box Plot", color="green"){
  
  boxplot(
    column,
    col=color,
    main=title
  )
  
}

# -------------------------------------------------------
# SCATTER PLOT
# -------------------------------------------------------

create_scatter_plot <- function(x, y, title="Scatter Plot", color="red"){
  
  plot(
    x,
    y,
    col=color,
    main=title,
    xlab="X",
    ylab="Y"
  )
  
}

# -------------------------------------------------------
# HEATMAP
# -------------------------------------------------------

create_heatmap <- function(df){
  
  numeric_data <- df[sapply(df, is.numeric)]
  
  if(ncol(numeric_data) < 2){
    return(NULL)
  }
  
  cor_matrix <- cor(numeric_data)
  
  heatmap(cor_matrix)
  
}

# -------------------------------------------------------
# BIPLOT (PCA)
# -------------------------------------------------------

create_biplot <- function(df){
  
  numeric_data <- df[sapply(df, is.numeric)]
  
  if(ncol(numeric_data) < 2){
    return(NULL)
  }
  
  pca <- prcomp(numeric_data, scale=TRUE)
  
  biplot(pca)
  
}

# -------------------------------------------------------
# MOSAIC PLOT
# -------------------------------------------------------

create_mosaic_plot <- function(col1, col2){
  
  mosaicplot(
    table(col1, col2),
    color=TRUE,
    main="Mosaic Plot"
  )
  
}

# -------------------------------------------------------
# GGPLOT SCATTER
# -------------------------------------------------------

gg_scatter_plot <- function(df, x, y){
  
  ggplot(df, aes_string(x=x, y=y)) +
    geom_point(color="blue")
  
}

# -------------------------------------------------------
# GGPLOT LINE
# -------------------------------------------------------

gg_line_plot <- function(df, x, y){
  
  ggplot(df, aes_string(x=x, y=y)) +
    geom_line(color="blue")
  
}

# -------------------------------------------------------
# GGPLOT HISTOGRAM
# -------------------------------------------------------

gg_histogram <- function(df, x){
  
  ggplot(df, aes_string(x=x)) +
    geom_histogram(fill="skyblue")
  
}

# -------------------------------------------------------
# GGPLOT BOXPLOT
# -------------------------------------------------------

gg_boxplot <- function(df, x, y){
  
  ggplot(df, aes_string(x=x, y=y)) +
    geom_boxplot(fill="orange")
  
}

# -------------------------------------------------------
# GGPLOT BAR CHART
# -------------------------------------------------------

gg_bar_chart <- function(df, x){
  
  ggplot(df, aes_string(x=x)) +
    geom_bar(fill="green")
  
}