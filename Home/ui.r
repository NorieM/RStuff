library(DT)
library(hms)
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
	
    tags$style(type="text/css", "#speedClassed th: {background-color:#ff0000 ;color:white;text-align: center;}"),

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
	tabPanel("Speed 15min", tableOutput("speedClassed")
		),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)