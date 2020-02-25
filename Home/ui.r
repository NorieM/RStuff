library(DT)
library(hms)
library(leaflet)
library(lubridate)
library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(theme="style.css",

#  tags$head(
#	tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

  sidebarPanel(
	fileInput(inputId = "filename", label = "Select ECO file"),
	radioButtons(inputId =  "interval", label = "Interval (mins)", c(15, 60)), 
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
				column(4, plotOutput("piechart")),
				column(4, tableOutput("classedSummary"))
			)
		)
	),
	tabPanel("Volume", tableOutput("classedVolume")),
	tabPanel("Speed", tableOutput("speedClassed")),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart")),
	tabPanel("Map", leafletOutput("map"))
	)
  )
)