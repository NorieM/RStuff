    tblClassSummary <- data.frame(Class = seq(1, 12), Description = c("PC/MC", 
        "CAR/LGV", "CAR/LGV", "OGV1 & PSV 2 Axle", "OGV1 & PSV 3 Axle", 
        "OGV2", "OGV1 & PSV 3 Axle", "OGV2", "OGV2", "OGV2", "OGV2", "OGV2"))
    
    tblDirections <- data.frame(Dir = c("N", "E", "W", "S"), Direction = c("Northbound", 
        "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)
        
        inFile <- "C:/Test/Projects/RStuff/Home/ATCReport.rtf"
        
        rawData <- as.data.frame(readLines(inFile, warn = FALSE), 
            stringsAsFactors = FALSE)
        
        names(rawData) <- c("Row")
        
        start <- which(startsWith(rawData$Row, "\\ulnone"))
        end <- max(which(startsWith(rawData$Row, "\\par"))) - 4
        
        rawData$Row[start] <- substring(rawData$Row[start], 12)
        
        data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)
        
        names(data) <- c("Row")
        
        parsedData <- data %>% mutate(Date = as.POSIXct(substring(Row, 
            16, 25)), Time = format(floor_date(as.POSIXct(substring(Row, 
            27, 34), format = "%H:%M:%S"), paste0("15", " mins")), 
            "%H:%M"), Dir = substring(Row, 36, 36)) %>% mutate(Speed = as.numeric(substring(Row, 
            39, 44)), Class = as.integer(substring(Row, 78, 79))) %>% mutate(Day = weekdays(Date)) %>% 
            left_join(tblDirections) %>% inner_join(tblClassSummary) %>% 
            select(Date, Time, Direction, Speed, Class, Description, Day)
        
