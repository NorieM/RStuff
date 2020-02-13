library(hms)
library(lubridate)
library(dplyr)
library(tidyverse)

speed15min <- parsedData %>%
  mutate(SpeedBin = cut(Speed, c(seq(0, 70, 5),999),labels=seq(0,70,5)  )) %>%
  arrange(Time, SpeedBin, Speed) %>%
  select(Class, SpeedBin, Time) %>%
	pivot_wider(names_from=SpeedBin, 
	            values_from = Class,
	       	values_fn=list(Class=length))

# create data frame from pivot
speed15min <- as.data.frame(speed15min)

# replace NA with 0
speed15min[is.na(speed15min)]=0

# re-order columns
speed15min <- speed15min[, c("Time", seq(0, 70, 5))]

speed15min <- speed15min %>% mutate_at(names(speed15min)[-1], as.character)

