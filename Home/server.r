# can be run from command line with following
#
#   r -e "shiny::runApp('C:/test/projects/rstuff/home', launch.browser=TRUE)"

server <- function(input, output, session){

    options(shiny.maxRequestSize=30*1024^2) 

    hideTab("Tabs", "Dashboard")
    hideTab("Tabs", "Chart")
    hideTab("Tabs", "Classes")
    hideTab("Tabs", "Speed")
    hideTab("Tabs", "Map")
    hideTab("Tabs", "Volume")
    tblClassSummary <- data.frame("Class" = seq(1,12), "Description" = c("PC/MC","CAR/LGV","CAR/LGV","OGV1 & PSV 2 Axle","OGV1 & PSV 3 Axle","OGV2","OGV1 & PSV 3 Axle","OGV2","OGV2","OGV2","OGV2","OGV2"
))

    tblDirections <- data.frame("Dir"= c("N", "E", "W", "S"), "Direction" = c("Northbound", "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)

    observeEvent(input$filename, {
        if(!is.null(input$filename)){
	    showTab("Tabs", "Dashboard")
	    showTab("Tabs", "Chart")
	    showTab("Tabs", "Classes")
          showTab("Tabs", "Speed")	
          showTab("Tabs", "Map")
	    showTab("Tabs", "Volume")
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
	mutate(Date = as.POSIXct(substring(Row,16,25)), Time = format(floor_date(as.POSIXct(substring(Row, 27, 34 ), format="%H:%M:%S"), paste0(input$interval, " mins")), "%H:%M"), Dir = substring(Row, 36, 36)) %>%
	mutate(Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79))) %>%
	mutate(Day = weekdays(Date)) %>%
	left_join(tblDirections) %>%
	inner_join(tblClassSummary) %>%
	select(Date, Time, Direction, Speed, Class, Description, Day)
              
  })

  output$map <- renderLeaflet({
	dfLatLng <- data.frame(lat = 55.9004, lng = -3.5969)
	m <- leaflet() %>% setView(lat = 55.9004, lng = -3.5969, zoom = 16) %>%
		addMarkers(data = dfLatLng) %>% 
		addTiles()
	m
  	}
  )

  output$aveSpeeds <- renderTable({
	req(theData())
	average_speeds <- theData() %>%
		group_by(Direction)  %>%
		summarize(Average = mean(Speed), Percentile = quantile(Speed, probs=0.85, na.rm=TRUE)) %>%
      	rbind(c("Both", mean(theData()$Speed), quantile(theData()$Speed, probs=0.85, na.rm=TRUE))) %>%
		mutate(Average = as.numeric(Average), Percentile = as.numeric(Percentile))
	average_speeds
	})

  output$limitSummary <- renderTable({
	req(theData())
	speed_limit <- 50

	psoSummary <- theData() %>%
		filter(Speed>speed_limit)%>%
		group_by(Direction)%>%
		count(Direction)

	apoSummary <-  theData() %>%
		filter(Speed>speed_limit*1.1+2)%>%
		group_by(Direction)%>%
		count(Direction)

	dftSummary <-  theData() %>%
		filter(Speed>speed_limit+15)%>%
		group_by(Direction)%>%
		count(Direction)

	speedSummary <-data.frame(psoSummary$n, apoSummary$n, dftSummary$n)

	names(speedSummary) = c("PSO", "APO", "DFT")

	speedSummary <- add_row(speedSummary, PSO = theData() %>% filter(Speed>speed_limit) %>% count(),
							  APO = theData() %>% filter(Speed>speed_limit*1.1+2) %>% count(),
							  DFT = theData() %>% filter(Speed>speed_limit+15) %>% count())	

	row.names(speedSummary) = c(psoSummary$Direction[1], psoSummary$Direction[2], "Both")

	speedSummary %>%
		mutate_at(names(speedSummary), as.character)

      cbind(Direction = c("Northbound", "Southbound", "Both"), speedSummary)

	})

  output$data <- renderTable({
  
	classCount <- theData() %>%
		filter(if(input$direction != "Both") Direction == input$direction else TRUE) %>%
		select(Date, Time, Direction, Class) %>%
	      arrange(Date, Time, Direction)%>%
		mutate(Date = format(Date,format="%d/%m/%Y"))%>%
      	group_by(Time,Direction) %>%
		pivot_wider(names_from=Class, 
			values_from = Class, 
	      	values_fn=list(Class=length)) 

	classCount <-as.data.frame(classCount)

	classCount[is.na(classCount)]=0

	classCount<-classCount[, c("Date", "Time", "Direction", seq(1, max(theData()$Class),1))] %>%
		mutate_at(as.character(seq(1, max(theData()$Class),1)), as.character)

	classCount
    
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
		ggtitle("Traffic Composition by Class\nTotal Surveyed Vehicles")

	pie

	})  

  output$classedVolume <- renderTable({
	dayCount <- theData() %>%
		select(Day, Time, Direction) %>%
		group_by(Day, Time) %>%
		count() %>%
		pivot_wider(names_from=Day,
				values_from=n,
				values_fn=list(Time=length))

	dayCount <- as.data.frame(dayCount)
	
	dayCount <- dayCount[, c("Time", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")]
	
	})

  output$speedClassed <- renderTable({

	req(theData())

	speed15min <- theData()%>%
		filter(if(input$direction != "Both") Direction == input$direction else TRUE) %>%
  		mutate(SpeedBin = cut(Speed, c(seq(0, 70, 5),999),labels=seq(0,70,5))) %>%
		arrange(Time, SpeedBin, Speed) %>%
		select(Class, SpeedBin, Time) %>%
 		pivot_wider(names_from=SpeedBin, 
	            values_from = Class, 
	       	values_fn=list(Class=length))


	# create data frame from pivot
	speed15min <- as.data.frame(speed15min)

	# replace NA with 0
	speed15min[is.na(speed15min)]=0

	# re-order columns
	missingCols <- setdiff(as.character(seq(0,70,5)), names(speed15min)[-1])

	speed15min[missingCols] <- 0

	speed15min <- speed15min[, c("Time", seq(0,70,5))]
	
	speed15min <- speed15min %>% mutate_at(names(speed15min)[-1], as.character)

	speed15min

})

  output$direction_dropdown <- renderUI({
    selectInput("direction", 
            "Select Direction",
            choices = c("Both", unique(theData()$Direction))
            )
})
}