# can be run from command line with following
#
#   r -e "shiny::runApp('C:/test/projects/rstuff/home', launch.browser=TRUE)"

server <- function(input, output, session){

    options(shiny.maxRequestSize=30*1024^2) 

    hideTab("Tabs", "Dashboard")
    hideTab("Tabs", "Classes")
    hideTab("Tabs", "Chart")

    tblClassSummary <- data.frame("Class" = seq(1,12), "Description" = c("PC/MC","CAR/LGV","CAR/LGV","OGV1 & PSV 2 Axle","OGV1 & PSV 3 Axle","OGV2","OGV1 & PSV 3 Axle","OGV2","OGV2","OGV2","OGV2","OGV2"
))

    tblDirections <- data.frame("Dir"= c("N", "E", "W", "S"), "Direction" = c("Northbound", "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)

    observeEvent(input$filename, {
        if(!is.null(input$filename)){
	    showTab("Tabs", "Dashboard")
	    showTab("Tabs", "Classes")
	    showTab("Tabs", "Chart")
        }
    })

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
	inner_join(tblClassSummary) %>%
	select(Date, Time, Direction, Speed, Class, Description)
              
  })

  output$dashboard <- renderTable({
	average_speeds <- theData() %>%
		group_by(Direction)  %>%
		summarize(Average = mean(Speed), Percentile = quantile(Speed, probs=0.85, na.rm=TRUE)) %>%
      	rbind(c("Both", mean(theData()$Speed), quantile(theData()$Speed, probs=0.85, na.rm=TRUE))) %>%
		mutate(Average = as.numeric(Average), Percentile = as.numeric(Percentile))

	})

  output$dashboard2 <- renderTable({
	speed_limit <- 50

	speedsNorth <- theData() %>% filter(Direction=="Eastbound")
	speedsSouth <- theData() %>% filter(Direction=="Westbound")

	speeds <- theData()$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summaryBoth <- c(PSL, APO, DFT)

	speeds <- 	speedsNorth$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summaryNorth <- c(PSL, APO, DFT)

	speeds <- 	speedsSouth$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summarySouth <- c(PSL, APO, DFT)

	summary<-as.data.frame(rbind(summaryNorth, summarySouth, summaryBoth))
	
	names(summary)<-c("PSL", "APO", "DFT")

      row.names(summary) <-c("Eastbound", "Westbound", "Both")

	summary

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

  output$piechart <-renderPlot({

	
	classCount <- theData() %>%
		  count(Description) 

	classCount <- classCount %>%
		  mutate(prop = n / sum(classCount$n) *100) %>%
		  mutate(ypos = cumsum(prop)- 0.5*prop )

	bp <- ggplot(classCount, aes(x= "",y=prop, fill=Description)) + 
		geom_bar(width = 1, stat = "identity")
		pie <- bp +	coord_polar("y", start=0) +
	      theme_void() +
		theme(plot.title = element_text(hjust = 0.5 ,face = "bold")) + 
		ggtitle("Traffic Composition by  Class - Total Surveyed Vehicles")

	pie

	})  

  output$direction_dropdown <- renderUI({
    selectInput("direction", 
            "Select Direction",
            choices = c("Both", unique(theData()$Direction))
            )
})
}