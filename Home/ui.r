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
	
    tabsetPanel(id="Tabs",
	tabPanel("Dashboard", tableOutput("dashboard")),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)