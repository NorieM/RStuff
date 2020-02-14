library(DT)
library(hms)
library(leaflet)
library(lubridate)
library(shiny)
library(tidyverse)
library(textreadr)

ui <- fluidPage(

  tags$head(
	tags$style(
	  	HTML("table {
  				border: 1px solid #1C6EA4;
				background-color: #EEEEEE;
  				width: 100%;
  				text-align: center;
  				border-collapse: collapse;
				}
			td, table. th {
  				border: 1px solid #AAAAAA;
  				padding: 2px 2px;
				}
			tbody td {
  				font-size: 12px;
  				}
			tr:nth-child(even) {
  				background: #D0E4F5;
				}
			thead {
  				background: #1C6EA4;
  				background: -moz-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
  				background: -webkit-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
  				background: linear-gradient(to bottom, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
  				border-bottom: 2px solid #444444;
				}
			thead th {
	  			font-size: 12px;
  				font-weight: bold;
  				color: #FFFFFF;
		  		text-align: center;
  				border-left: 2px solid #D0E4F5;
				}
			thead th:first-child {
  				border-left: none;
				}

			")  
	)
  ),

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
	tabPanel("Speed 15min", tableOutput("speedClassed")
		),
	tabPanel("Classes", tableOutput("data")),
	tabPanel("Chart", plotOutput("chart"))
	)
  )
)