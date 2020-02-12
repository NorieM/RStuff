library(DT)
library(leaflet)
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
	tabPanel("Dashboard",
		fluidPage( 
			fluidRow(
				column(4, tableOutput("aveSpeeds")),
				column(4, tableOutput("limitSummary")),
				column(4, leafletOutput("map")),
				column(4, plotOutput("piechart"))
			)
		)
	),
	#tabPanel("Map", leafletOutput("map")),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)