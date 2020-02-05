library(DT)
library(lubridate)
library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(
  sidebarPanel(

	fileInput(inputId = "filename", label = "Select ECO file"),
	uiOutput("direction_dropdown"),
	width = 2
	),
  mainPanel(
	
    tabsetPanel(
	tabPanel("Survey Details", tableOutput("directions")),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)