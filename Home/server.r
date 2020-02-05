# can be run from command line with following
#
#   r -e "shiny::runApp('C:/test/projects/rstuff/home', launch.browser=TRUE)"

server <- function(input, output, session){

  options(shiny.maxRequestSize=30*1024^2) 
  
  tblDirections <- data.frame("Dir"= c("N", "E", "W", "S"), "Direction" = c("Northbound", "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)

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
	mutate(Date = as.POSIXct(substring(Row,16,25)), Time = format(floor_date(as.POSIXct(substring(Row, 27, 34 ), format="%H:%M:%S"), "15 mins"), "%H:%M"), Dir = substring(Row, 36, 36)) %>%
	mutate(Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79))) %>%
	left_join(tblDirections) %>%
	select(Date, Time, Direction, Speed, Class)
              
  })

  output$directions <- renderTable({
	dirA <- unique(theData()$Direction)[1]
	dirB <- unique(theData()$Direction)[2]

	dirs <- data.frame("Directions" = c(dirA, dirB))
	})

  output$data <- renderTable({
  
    classCount <- theData() %>%
      group_by(Date,Direction) %>%
      count(Class) 
    
  })

  output$chart <- renderPlot({
	chartData<-theData() %>%
	filter(if(input$direction != "Both") Direction == input$direction else TRUE) %>%
	mutate(Day = wday(Date, label=TRUE, abbr=FALSE)) %>%
	group_by(Day, Time) %>%
	count(Time) %>%
	select(Day, Time, n)	

	ggplot(data = chartData, aes(x = Time, y = n)) + 
	  geom_line(aes(group=Day, color = Day))+
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
	  theme(plot.title = element_text(hjust = 0.5 ,face = "bold")) + 
	  scale_x_discrete(breaks = chartData$Time[seq(1, 96, by = 4)]) +
	  ggtitle(paste0("Volume by day - ", input$direction))

  })  

  output$direction_dropdown <- renderUI({
    selectInput("direction", 
            "Select Direction",
            choices = c("Both", unique(theData()$Direction))
            )
})
}