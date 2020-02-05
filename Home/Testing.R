library(ggplot2)
library(hms)
library(lubridate)

getDirection <- function(dir){
	switch(dir, "N" = "Northbound", "E" = "Eastbound", "S" = "Southbound", "W" = "WestBound") 
}

tblDirections <- data.frame("Dir"= c("N", "E", "W", "S"), "Direction" = c("Northbound", "Eastbound", "Westbound", "Southbound"), stringsAsFactors = FALSE)


rawData <- as.data.frame(readLines("C:/Test/Projects/RStuff/Home/Site 12_14th Feb - 15th Feb.rtf", warn = FALSE), stringsAsFactors = FALSE)

names(rawData)<- c("Row")

start <-  which(startsWith(rawData$Row, "\\ulnone") )
end <-  max(which(startsWith(rawData$Row, "\\par")))-4

rawData$Row[start]<-substring(rawData$Row[start],12)

data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)

names(data) <- c("Row")

parsedData <- data %>%
  mutate(Date = as.POSIXct(substring(Row,16,25)), Time = format(floor_date(as.POSIXct(substring(Row, 27, 34 ), format="%H:%M:%S"), "15 mins"), "%H:%M"), Dir = substring(Row, 36, 36)) %>%
  mutate(Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79))) %>%
  inner_join(tblDirections) %>%
  select(Date, Time, Direction, Speed, Class)

classCount <- parsedData %>%
  count(Class) 

classCount <- classCount %>%
  mutate(prop = n / sum(classCount$n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

bp <- ggplot(classCount, aes(x= "",y=prop, fill=Class)) + 
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0) +
      theme_void()

pie

map(parsedData, class)

