library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(
  
  fileInput(inputId = "filename", label = "Select ECO file"),
  mainPanel(
    tableOutput(outputId = "data")
  )
)