library(ggplot2)
library(hms)

rawData <- as.data.frame(readLines("C:/projects/R/Site 12_14th Feb - 15th Feb.rtf", warn = FALSE), stringsAsFactors = FALSE)

names(rawData)<- c("Row")

start <-  which(startsWith(rawData$Row, "\\ulnone") )
end <-  max(which(startsWith(rawData$Row, "\\par")))-4

rawData$Row[start]<-substring(rawData$Row[start],12)

data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)

names(data) <- c("Row")

parsedData <- data %>%
  mutate(Date = as.POSIXct(substring(Row,16,25)), Time = as.hms(substring(Row, 27, 34 )), Direction = substring(Row, 36, 37)) %>%
  mutate(Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79))) %>%
  select(Date, Time, Direction, Speed, Class)

classCount <- parsedData %>%
  count(Class) 

classCount