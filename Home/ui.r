library(DT)
library(hms)
library(leaflet)
library(lubridate)
library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(theme="style.css",

  sidebarPanel(
	fileInput(inputId = "filename", label = "Select ECO file"),
	radioButtons(inputId =  "interval", label = "Interval (mins)", c(5,15, 60), selected=15), 
	uiOutput("direction_dropdown"),
	width = 2
	),

  mainPanel(
	
    tabsetPanel(id="Tabs",
	tabPanel("Dashboard",
		fluidPage( 
			fluidRow(
				column(4, plotOutput("piechart")),
				column(4, h3("Average speeds"), tableOutput("aveSpeeds")),
				column(4, h3("On a 7-day average"), tableOutput("limitSummary"), uiOutput("Abbreviations"))
			)
		)
	),
	tabPanel("Volume", h2(uiOutput("VolumeHeader")), tableOutput("classedVolume")),
	tabPanel("Speed", h2(uiOutput("SpeedHeader")), tableOutput("speedClassed")),
	tabPanel("Classes", h2(uiOutput("ClassesHeader")), tableOutput("data")),
	tabPanel("Classed", h2(uiOutput("ClassedHeader")), tableOutput("classedSummary")),
	tabPanel("Chart", plotOutput("chart")),
	tabPanel("Map", leafletOutput("map"))
	)
  )
)