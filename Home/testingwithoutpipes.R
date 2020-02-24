library(hms)
library(lubridate)
library(dplyr)

rawData <- as.data.frame(readLines("C:/Test/Projects/RStuff/Home/ATCReport.rtf", warn = FALSE), stringsAsFactors = FALSE)

names(rawData)<- c("Row")

start <-  which(startsWith(rawData$Row, "\\ulnone") )
end <-  max(which(startsWith(rawData$Row, "\\par")))-4

rawData$Row[start]<-substring(rawData$Row[start],12)

data <- as.data.frame(rawData$Row[start:end], stringsAsFactors = FALSE)

names(data) <- c("Row")

separate(data, data$Row, into=NA, sep=" ")

parsedData <- mutate(data, Date = as.POSIXct(substring(Row,16,25)), Time = floor_date(as.POSIXct(substring(Row, 27, 34 ), format="%H:%M:%S"), "15 mins"), Direction = substring(Row, 36, 37))

parsedData <- mutate(parsedData, Speed = as.numeric(substring(Row,39,44)), Class = as.integer(substring(Row,78,79)))
parsedData <- select(parsedData, Date, Time, Direction, Speed, Class)


head(parsedData)
classCount <- parsedData %>%
  count(Class) 

classCount$Class <- as.factor(classCount$Class)

classCount <- classCount %>%
  mutate(prop = n / sum(classCount$n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

bp <- ggplot(classCount, aes(x= "",y=prop, fill=Class)) + 
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0) +
      theme_void() + 
      geom_text(aes(y=ypos,label=Class), color="red", size = 6 )

pie

speedCat <- function(speed){
  cat <- case_when(
    speed<=10 ~ "0-10",
    speed<=20 ~ "11-20",
    speed<=30 ~ "21-30",
    speed<=40 ~ "31-40",
    speed<=50 ~ "41-50",
    speed<=60 ~ "51-60",
    speed<=70 ~ "61-70",
    TRUE ~ ">70"
    
  )
  return(cat)
}

map(parsedData, class)

class(floor_date(parsedData$Time, "15 mins"))

parsedData %>%
  mutate(SpeedBin = speedCat(Speed)) %>%
  arrange(Time, SpeedBin, Speed) %>%
  select(Class, SpeedBin, Time) %>%
  pivot_wider(names_from=SpeedBin, 
              values_from = Class,
              values_fn=list(count))
