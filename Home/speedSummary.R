speed_limit <- 50

psoSummary <- parsedData %>%
	filter(Speed>speed_limit)%>%
	group_by(Direction)%>%
	count(Direction)

apoSummary <-  parsedData %>%
	filter(Speed>speed_limit*1.1+2)%>%
	group_by(Direction)%>%
	count(Direction)

dftSummary <-  parsedData %>%
	filter(Speed>speed_limit+15)%>%
	group_by(Direction)%>%
	count(Direction)

speedSummary <-data.frame(psoSummary$n, apoSummary$n, dftSummary$n)

names(speedSummary) = c("PSO", "APO", "DFT")

speedSummary <- add_row(speedSummary, PSO = parsedData %>% filter(Speed>speed_limit) %>% count(),
						  APO = parsedData %>% filter(Speed>speed_limit*1.1+2) %>% count(),
						  DFT = parsedData %>% filter(Speed>speed_limit+15) %>% count())	


row.names(speedSummary) = c(psoSummary$Direction[1], psoSummary$Direction[2], "Both")

speedSummary

average_speeds <- parsedData %>%
	group_by(Direction)  %>%
	summarize(Average = mean(Speed), Percentile = quantile(Speed, probs=0.85, na.rm=TRUE)) %>%
      rbind(c("Both", mean(parsedData$Speed), quantile(parsedData$Speed, probs=0.85, na.rm=TRUE))) %>%
	mutate(Average = as.numeric(Average), Percentile = as.numeric(Percentile))
average_speeds

speedSummary <- parsedData %>%
	group_by(Direction) %>%
      mutate( PSO = as.numeric(Speed>speed_limit)) %>%
      mutate( APO = as.numeric(Speed>speed_limit*1.1+2)) %>%
      mutate( DFT = as.numeric(Speed>speed_limit+15)) %>%
		summarize(PSO = sum(PSO), APO = sum(APO), DFT = sum(DFT)) %>%
      rbind(c("Both", length(which(parsedData$Speed > speed_limit)), 
   				length(which(parsedData$Speed > speed_limit*1.1+2)),
				length(which(parsedData$Speed > speed_limit+15)))) 


speedSummary