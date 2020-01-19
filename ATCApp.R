library(shiny)
library(tidyverse)

ui <- fluidPage(

  fileInput(inputId = "filename", label = "Select ECO file"),
  mainPanel(
    tableOutput(outputId = "data")
  )
)

server <- function(input, output){
  output$data <- renderTable({
    inFile <- input$filename
    
    if (is.null(inFile))
      return(NULL)
    
    rawData <- as.data.frame(readLines(inFile$datapath, warn = FALSE))
    
    names(rawData)<- c("Row")
    
    
    start <- 10 # which(startsWith(rawData$Row, "\\ulnone"), rawData$Row)
    end <- 120 # which(startsWith(rawData$Row, "\\par"), rawData$Row)-1
    
    delimitedData <- rawData %>%
      separate(rawData, sep=" ")

  })
  
  
}

shinyApp(ui = ui, server = server)