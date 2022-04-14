library(shiny)
library(shinythemes)
library(shinyWidgets)     # custom input widgets for shiny
library(shinycssloaders)  # add loading animations
library(markdown)
library(plotly)
library(DT)               # visualization of data tables
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(visNetwork)       # visualization of networks
library(org.Mm.eg.db)
library(scales)           # graphical scales (map data to colors etc.)
library(RColorBrewer)

source("utilities_network.R")


# function necessary to display column name information when hovering
callHeaderCallback <- function(hover_info){
  
  headerCallback <- c(
    "function(thead, data, start, end, display){",
    sprintf("  var tooltips = [%s];", toString(paste0("'", hover_info, "'"))),
    "  for(var i = 1; i <= tooltips.length; i++){",
    "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
    "  }",
    "}"
  )
  
  return(headerCallback)
  
}