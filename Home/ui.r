library(DT)
library(lubridate)
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
	tabPanel("Survey Details", dataTableOutput("directions")),
	tabPanel("Classes", dataTableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)