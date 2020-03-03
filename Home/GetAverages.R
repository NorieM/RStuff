
WeekdayAverage <- parsedData %>% group_by(Direction) %>% summarize(Weekday= sum(wday(Date) %in% c(2,3,4,5,6)))

WeeklyAverage <- parsedData %>% group_by(Direction) %>% summarize(Weekly = sum(wday(Date) %in% c(1,2,3,4,5,6,7)))

TotalWeekdayAverage <- parsedData %>% summarize(Total = sum(wday(Date) %in% c(2,3,4,5,6)))
TotalWeeklyAverage <- parsedData %>% summarize(Total = sum(wday(Date) %in% c(1,2,3,4,5,6,7)))

Averages <-  WeekdayAverage %>% inner_join(WeeklyAverage)

Averages <- add_row(Averages,Direction = "Both", Weekday=TotalWeekdayAverage[,1], Weekly=TotalWeeklyAverage[,1])

Averages$Weekday <- round(Averages$Weekday/5,0)

Averages['7 Day Average'] <- round(Averages$Weekly/7,0)

Averages<- Averages[, c(1,2,4,3)]

names(Averages) <- c("Direction", "Weekday Average Total Traffic", "7-Day Average Traffic", "Weekly Traffic Total")

Averages