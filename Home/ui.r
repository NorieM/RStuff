library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(
  sidebarPanel(

	  fileInput(inputId = "filename", label = "Select ECO file"),
	  width = 2
	),
  mainPanel(
	
    tabsetPanel(
	tabPanel("Primary", textOutput("primary")),
	tabPanel("Secondary", textOutput("secondary")),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)