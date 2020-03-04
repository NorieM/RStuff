# can be run from command line with following r -e
# 'shiny::runApp('C:/test/projects/rstuff/home', launch.browser=TRUE)'

server <- function(input, output, session) {
    
    options(shiny.maxRequestSize = 30 * 1024^2)
    
    hideTab("Tabs", "Dashboard")
    hideTab("Tabs", "Chart")
    hideTab("Tabs", "Classed")
    hideTab("Tabs", "Classes")
    hideTab("Tabs", "Speed")
    hideTab("Tabs", "Map")
    hideTab("Tabs", "Volume")

    speed_limit <- 50

    tblClassSummary <- data.frame(Class = seq(1, 12), Description = c("PC/MC", 
        "CAR/LGV", "CAR/LGV", "OGV1 & PSV 2 Axle", "OGV1 & PSV 3 Axle", 
        "OGV2", "OGV1 & PSV 3 Axle", "OGV2", "OGV2", "OGV2", "OGV2", "OGV2"))
    
    tblDirections <- data.frame(Dir = c("N", "E", "W", "S"), Direction = c("Northbound", 
        "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)
    
    observeEvent(input$filename, {
        if (!is.null(input$filename)) {
            showTab("Tabs", "Dashboard")
            showTab("Tabs", "Chart")
		showTab("Tabs", "Classed")
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
        
        rawData <- as.data.frame(readLines(inFile$datapath, warn = FALSE), 
            stringsAsFactors = FALSE)
        
        names(rawData) <- c("Row")
        
        start <- which(startsWith(rawData$Row, "\\ulnone"))
        end <- max(which(startsWith(rawData$Row, "\\par"))) - 4
        
        rawData$Row[start] <- substring(rawData$Row[start], 12)
        
        data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)
        
        names(data) <- c("Row")
        
        parsedData <- data %>% mutate(Date = as.POSIXct(substring(Row, 
            16, 25)), Time = format(floor_date(as.POSIXct(substring(Row, 
            27, 34), format = "%H:%M:%S"), paste0(if(length(input$interval) == 0) 15 else input$interval , " mins")), 
            "%H:%M"), Dir = substring(Row, 36, 36)) %>% mutate(Speed = as.numeric(substring(Row, 
            39, 44)), Class = as.integer(substring(Row, 78, 79))) %>% mutate(Day = weekdays(Date)) %>% 
            left_join(tblDirections, by=c("Dir")) %>% inner_join(tblClassSummary, by=c("Class")) %>% 
            select(Date, Time, Direction, Speed, Class, Description, Day)
        
    })
    
    output$map <- renderLeaflet({

        dfLatLng <- data.frame(lat = 55.9004, lng = -3.5969)

        m <- leaflet() %>% 
             addTiles()%>% 
             setView(lat = 55.9004, lng = -3.5969, zoom = 16)%>% 
		 addMarkers(data = dfLatLng, label = "Somewhere over the rainbow")        
	  m
    })
    
    output$aveSpeeds <- renderTable({

        req(theData())

        average_speeds <- theData() %>% group_by(Direction) %>% summarize(Average = mean(Speed), 
            Percentile = quantile(Speed, probs = 0.85, na.rm = TRUE)) %>% 
            rbind(c("Both", mean(theData()$Speed), quantile(theData()$Speed, 
                probs = 0.85, na.rm = TRUE))) %>% mutate(Average = as.numeric(Average), 
            Percentile = as.numeric(Percentile))
        average_speeds
    })
    
    output$limitSummary <- renderTable({

        req(theData())
        
        psoSummary <- theData() %>% filter(Speed > speed_limit) %>% group_by(Direction) %>% 
            count(Direction)
        
        apoSummary <- theData() %>% filter(Speed > speed_limit * 1.1 + 
            2) %>% group_by(Direction) %>% count(Direction)
        
        dftSummary <- theData() %>% filter(Speed > speed_limit + 15) %>% 
            group_by(Direction) %>% count(Direction)
        
        speedSummary <- psoSummary %>% merge(apoSummary, by="Direction") %>% merge(dftSummary, by="Direction")
        
        names(speedSummary) = c("Direction", "PSO", "APO", "DFT")
        
        speedSummary <- add_row(speedSummary, Direction="Both", PSO = theData() %>% filter(Speed > 
            speed_limit) %>% count(), APO = theData() %>% filter(Speed > 
            speed_limit * 1.1 + 2) %>% count(), DFT = theData() %>% filter(Speed > 
            speed_limit + 15) %>% count())

        speedSummary[1, 2:4]<-paste0(round(unlist(speedSummary[1, 2:4])/length(which(theData()$Direction=="Eastbound"))*100,1), "%")
        speedSummary[2, 2:4]<-paste0(round(unlist(speedSummary[2, 2:4])/length(which(theData()$Direction=="Westbound"))*100,1), "%")
        speedSummary[3, 2:4]<-paste0(round(unlist(speedSummary[3, 2:4])/length(theData()$Direction)*100,1), "%")
        
        speedSummary %>% mutate_at(names(speedSummary), as.character)
        
    })
    
    output$AverageTraffic <- renderTable({
	req(theData())
		WeekdayAverage <- theData() %>% group_by(Direction) %>% summarize(Weekday= sum(wday(Date) %in% c(2,3,4,5,6)))

		WeeklyAverage <- theData() %>% group_by(Direction) %>% summarize(Weekly = sum(wday(Date) %in% c(1,2,3,4,5,6,7)))

		TotalWeekdayAverage <- theData() %>% summarize(Total = sum(wday(Date) %in% c(2,3,4,5,6)))

		TotalWeeklyAverage <- theData() %>% summarize(Total = sum(wday(Date) %in% c(1,2,3,4,5,6,7)))

		Averages <-  WeekdayAverage %>% inner_join(WeeklyAverage, by=c("Direction"))

		Averages <- add_row(Averages,Direction = "Both", Weekday=TotalWeekdayAverage[,1], Weekly=TotalWeeklyAverage[,1])

		Averages$Weekday <- as.character(round(Averages$Weekday/5,0))

		Averages['7 Day Average'] <- as.character(round(Averages$Weekly/7,0))

		Averages<- Averages[, c(1,2,4,3)]

		#names(Averages) <- c("Direction", "Weekday Average Total Traffic", "7-Day Average Traffic", "Weekly Traffic Total")

		#Averages
	})    

    output$data <- renderTable({
	req(theData())
        
        classCount <- theData() %>% filter(if (input$direction != "Both") 
            Direction == input$direction else TRUE) %>% select(Date, Time, Direction, Class) %>% arrange(Date, 
            Time, Direction) %>% mutate(Date = format(Date, format = "%d/%m/%Y")) %>% 
            group_by(Time, Direction) %>% pivot_wider(names_from = Class, 
            values_from = Class, values_fn = list(Class = length))
        
        classCount <- as.data.frame(classCount)
        
        classCount[is.na(classCount)] = 0
        
        classCount <- classCount[, c("Date", "Time", seq(1, 
            max(theData()$Class), 1))] %>% mutate_at(as.character(seq(1, 
            max(theData()$Class), 1)), as.character)
        
        classCount
        
    })
    
    output$chart <- renderGvis({

	#session$clientData$output_trend_width

	req(theData())
        
        chartData <- theData() %>% filter(if (input$direction != "Both") 
            Direction == input$direction else TRUE) %>% mutate(Day = wday(Date, label = TRUE, abbr = FALSE, week_start= getOption("lubridate.week.start", 1))) %>% 
            group_by(Day, Time) %>% count(Time) %>% select(Day, Time, n)
        
        chartData<-chartData %>% pivot_wider(names_from=Day, values_from=n, values_fn = list(Time=n))

        gvisLineChart(chartData, options=list(curveType='function', height=500, title=paste0("Volume by day - ", input$direction)))  

       #ggplot(data = chartData, aes(x = Time, y = n)) + geom_line(aes(group = Day, 
        #    color = Day)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         #   panel.background = element_blank()) + theme(plot.title = element_text(hjust = 0.5, 
          #  face = "bold")) + scale_x_discrete(breaks = chartData$Time[seq(1, 
           # 96, by = 4)]) + ggtitle(paste0("Volume by day - ", input$direction))
        
    })
    
    output$piechart <- renderPlot({
        
	req(theData())

        classCount <- as.data.frame(summary(theData()$Description))
        
        colnames(classCount) <- c("Count")
        
        classCount$prop = classCount$Count/sum(classCount$Count) * 100
        
        classCount$ypos = cumsum(classCount$prop) - 0.5 * classCount$prop
        
        classCount$Description = row.names(classCount)
        
        bp <- ggplot(classCount, aes(x = "", y = prop, fill = Description)) + 
            geom_bar(width = 1, stat = "identity")
        
        pie <- bp + coord_polar("y", start = 0) + theme_void() + theme(plot.title = element_text(hjust = 0.5, 
            face = "bold")) + ggtitle("Traffic Composition by Class\nTotal Surveyed Vehicles")
        
        pie
        
    })
    
    output$classedVolume <- renderTable({

	req(theData())

        dayCount <- theData() %>% filter(if (input$direction != "Both") 
            Direction == input$direction else TRUE) %>% select(Day, Time, Direction) %>% group_by(Day, 
            Time) %>% count() %>% pivot_wider(names_from = Day, values_from = n, 
            values_fn = list(Time = length))
        
        dayCount <- as.data.frame(dayCount)
        
        dayCount <- dayCount[, c("Time", "Monday", "Tuesday", "Wednesday", 
            "Thursday", "Friday", "Saturday", "Sunday")]
        
    })
    
    output$speedClassed <- renderTable({
        
        req(theData())
        
        speed15min <- theData() %>% filter(if (input$direction != "Both") 
            Direction == input$direction else TRUE) %>% mutate(SpeedBin = cut(Speed, c(seq(0, 70, 5), 999), 
            labels = seq(0, 70, 5))) %>% arrange(Time, SpeedBin, Speed) %>% 
            select(Direction,Class, SpeedBin, Time) %>% pivot_wider(names_from = SpeedBin, 
            values_from = Class, values_fn = list(Class = length))
                
        # create data frame from pivot
        speed15min <- as.data.frame(speed15min)
        
        # replace NA with 0
        speed15min[is.na(speed15min)] = 0
        
        # re-order columns
        missingCols <- setdiff(as.character(seq(0, 70, 5)), names(speed15min)[-1])
        
        speed15min[missingCols] <- 0
        
        speed15min <- speed15min[, c("Time", "Direction", seq(0, 70, 5))]
        
        speed15min <- speed15min %>% mutate_at(names(speed15min)[-1], as.character)

	  aveSpeed <- theData() %>% 
		group_by(Direction, Time) %>% 
		summarize(Average = mean(Speed), "85%" = quantile(Speed, probs = 0.85, na.rm = TRUE), "95%" = quantile(Speed, probs = 0.85, na.rm = TRUE)) %>%
		ungroup()


        countSpeed <- theData() %>% 
		group_by(Direction, Time) %>% 
		summarize("PSL" = sum(Speed> speed_limit), "APO" = sum(Speed> speed_limit*1.1+2), "DFT"=sum(Speed> speed_limit+15)) %>%
		ungroup()

        speed15min <- speed15min  %>% left_join(countSpeed) %>% left_join(aveSpeed)

	  speed15min
        
    })
    
    output$classedSummary <- renderTable({
        req(theData())
        
        classSummary <- theData() %>% arrange(Date) %>% inner_join(tblClassSummary) %>% 
	      filter(if (input$direction != "Both") 
            Direction == input$direction else TRUE) %>%
            group_by(Date, Day, Description, Direction) %>% count() %>% 
		ungroup() %>%
            select(Day, Description, Direction, n)
        
        classSummary <- classSummary %>% pivot_wider(names_from = Description, 
            values_from = n, values_fn = list(Day = length)) 

        classSummary <- classSummary[, c(1,2,7,3,4,5,6)]
    })
    
    output$direction_dropdown <- renderUI({
        req(theData())

	  currentTab <- input$Tabs
	  if(!length(currentTab)==0){
	  currentDir <- input$direction
	  if (!(currentTab  %in% c("Dashboard", "Map"))){
	   selectInput("direction", "Select direction", choices = c("Both", unique(theData()$Direction)), selected = currentDir)
	  }
	}
    })

	
    output$download_button <- renderUI({
	  req(theData())
        downloadButton("downloadData", "Download parsed data")
	})

    output$day_dropdown <- renderUI({
	  req(theData())

	  currentTab <- input$Tabs 
	  if(!length(currentTab)==0){
	  currentDay <- input$day
        if(currentTab %in% c("Classes", "Speed")){
		selectInput("day", "Select day", choices = c("All", unique(theData()$Day)), selected = currentDay)
	  }
	}
	})
 
    output$interval_selector <- renderUI({
	req(theData())

	  currentTab <- input$Tabs 
	  if(!length(currentTab)==0){
	        currentInterval = input$interval
      	  if(currentTab %in% c("Classes", "Speed", "Volume")){
			radioButtons(inputId =  "interval", label = "Interval (mins)", c(5,15, 60), selected=if(length(currentInterval)==0) 15 else currentInterval)
	  }
	}

	})

    output$VolumeHeader <- renderUI({
		paste0("Volume counts - ", input$direction," ", input$interval ," mins")
	})

    output$SpeedHeader <- renderUI({
		paste0("Speed counts - ", input$direction," ", input$interval ," mins")
	})

    output$ClassesHeader <- renderUI({
		paste0("Classes counts - ", input$direction," ", input$interval ," min")
	})

    output$ClassedHeader <- renderUI({
		paste0("Classed Summary")
	})

    output$Abbreviations <- renderUI({   
	abbr<-c("PSO - Posted speed limit","APO - Association of Chief Police Officers - 110% of PSL + 2mph",					
		  "DFT - Department for Transport - PSL + 15mph")

 	HTML(paste0("<ul><li>", paste0(paste0(abbr, collpase = ""), collapse = "</li><li>"),"</li></ul>"))

 	})

	
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ParsedData.csv", sep = "")
    },
    content = function(file) {
      write.csv(theData(), file, row.names = FALSE)
    }
  )

}