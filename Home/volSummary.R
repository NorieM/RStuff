totalVol <- parsedData %>% 
	group_by(Direction) %>%
	count(Direction) 

weeklyAve <- parsedData %>% 
	mutate(Day = wday(Date, label=TRUE, abbr=FALSE)) %>%
	group_by(Direction) %>%
	count(Direction)

weeklyAve$n <- round(weeklyAve$n/7,0)

weekdayAve <- parsedData %>% 
	mutate(Day = wday(Date, label=TRUE, abbr=FALSE)) %>%
	filter(str_detect(Day, "S")!=TRUE) %>%
	group_by(Direction) %>%
	count(Direction)

weekdayAve$n <- round(weekdayAve$n/5,0)

volSummary <-data.frame(weekdayAve$n, weeklyAve$n, totalVol$n)

names(volSummary) = c("Weekday Average", "7 Day Average", "Weekly Traffic Total")

volSummary <- add_row(volSummary, "Weekday Average" = round(parsedData %>% mutate(Day = wday(Date, label=TRUE, abbr=FALSE)) %>% filter(str_detect(Day, "S")!=TRUE) %>% count()/5,0),
					    "7 Day Average" = round(length(parsedData$Speed)/7,0), 
                                  "Weekly Traffic Total" = length(parsedData$Speed))

row.names(volSummary) = c(weekdayAve$Direction[1],weekdayAve$Direction[2], "Both")

volSummary
