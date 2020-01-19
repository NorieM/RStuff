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

print(speedCat(20))

parsedData %>%
  mutate(SpeedBin = speedCat(Speed)) %>%
  select(Class, SpeedBin) %>%
  pivot_wider(names_from=SpeedBin, 
              values_from = Class,
              values_fn=list(count))
