
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

rtfFile <- file.choose()

res <- readLines(rtfFile, warn = FALSE)
start <- which(startsWith(res, "\\ul\\b DS Trig"))+1
end <- max(which(startsWith(res, "\\par")))-1

print(start)
print(end)

data <- as.data.frame(res[start:end], stringsAsFactors = FALSE)

names(data)<-c("Row")

print(head(data))

data <- data %>%
  mutate(newRow = str_replace_all(Row,"\\\\ulnone\\\\b0 ","" )) %>%
  mutate(date = substring(newRow, 16, 26))  %>%
  mutate(time = substring(newRow, 28, 35)) %>%
  mutate(direction = substring(newRow, 36,38)) %>%
  mutate(speed = substring(newRow,40,46)) %>%
  mutate(class = substring(newRow, 78,80)) %>%
  select(newRow, date, time, direction, speed, class)

names(data)<-c("OriginalData", "Date", "Time", "Direction", "Speed", "Class")

print(head(data))

