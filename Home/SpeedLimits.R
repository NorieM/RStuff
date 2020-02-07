	speed_limit <- 50

	speedsNorth <-parsedData %>% filter(Direction=="Eastbound")
	speedsSouth <-parsedData %>% filter(Direction=="Westbound")

	speeds <- parsedData$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summaryBoth <- c(PSL, APO, DFT)

	speeds <- 	speedsNorth$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summaryNorth <- c(PSL, APO, DFT)


	speeds <- 	speedsSouth$Speed

	PSL <- length(which(speeds<=50))
	APO <- length(which(speeds>50))
 	DFT <- length(which(speeds>65))

	summarySouth <- c(PSL, APO, DFT)

	summary<-as.data.frame(rbind(summaryNorth, summarySouth, summaryBoth))

	names(summary)<-c("PSL", "APO", "DFT")

      row.names(summary) <-c("Eastbound", "Westbound", "Both")

class(summary)

summary