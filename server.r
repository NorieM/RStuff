server <- function(input, output){

  options(shiny.maxRequestSize=30*1024^2) 

  theData = reactive({

    inFile <- input$filename
    
    if (is.null(inFile))
      return(NULL)

    rawData <- as.data.frame(readLines(inFile$datapath, warn = FALSE), stringsAsFactors = FALSE)
  
    names(rawData)<- c("Row")
  
    start <-  which(startsWith(rawData$Row, "\\ulnone") )
    end <-  max(which(startsWith(rawData$Row, "\\par")))-4
  
    rawData$Row[start]<-substring(rawData$Row[start],12)
  
    data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)
  
    names(data) <- c("Row")
  
    parsedData <- data %>%
      mutate(Date = substring(Row,16,25), Time = substring(Row,27,34), Direction = substring(Row, 36, 37)) %>%
      mutate(Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79))) %>%
      select(Date, Time, Direction, Speed, Class)

  })

  output$primary <- renderText({unique(theData()$Direction[1])})
  output$secondary <- renderText({unique(theData()$Direction[3])})

  output$data <- renderTable({
  
    classCount <- theData() %>%
      group_by(Direction) %>%
      count(Class) 
    
  })
    
}