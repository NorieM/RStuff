classCount <- parsedData %>%
	select(Date, Time, Direction, Class) %>%
      arrange(Date, Time, Direction)%>%
      group_by(Time,Direction) %>%
	pivot_wider(names_from=Class, 
		values_from = Class, 
	      values_fn=list(Class=length)) 

classCount <-as.data.frame(classCount)

classCount[is.na(classCount)]=0

classCount<-classCount[, c("Date", "Time", "Direction", seq(1, max(parsedData$Class),1))]

classCount