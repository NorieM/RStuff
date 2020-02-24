	speedLimit <- 50

      dirPrimary <- unique(parsedData$Direction)[1]
      dirSecondary <- unique(parsedData$Direction)[2]

	dirs <- c(dirPrimary, dirSecondary, "Both")

	speedsPrimary <- parsedData %>% filter(Direction==dirPrimary)
	speedsSecondary <- parsedData %>% filter(Direction==dirSecondary)

	speeds <- parsedData$Speed
	totalVol <- length(speeds)

	PSL <- length(which(speeds>speedLimit))
	APO <- length(which(speeds>speedLimit*1.1+2))
 	DFT <- length(which(speeds>speedLimit+15))

	summaryBoth <- paste0(round(c(PSL, APO, DFT)/totalVol * 100,2), "%")

	speeds <- 	speedsPrimary$Speed
	totalVol <- length(speeds)

	PSL <- length(which(speeds>speedLimit))
	APO <- length(which(speeds>speedLimit*1.1+2))
 	DFT <- length(which(speeds>speedLimit+15))

	summaryPrimary <- paste0(round(c(PSL, APO, DFT)/totalVol * 100,2), "%")
	
	speeds <- 	speedsSecondary$Speed
	totalVol <- length(speeds) 

	PSL <- length(which(speeds>speedLimit))
	APO <- length(which(speeda>speedLimit*1.1+2))
 	DFT <- length(which(speeds>speedLimit+15))

	summarySecondary <- paste0(round(c(PSL, APO, DFT)/totalVol * 100,2), "%")

      summary<-rbind(summaryPrimary, summarySecondary,summaryBoth, deparse.level=0)

	dirs <- c(dirPrimary, dirSecondary, "Both")

	summary<-cbind(dirs, summary) 

	summary<- as.data.frame(summary)

      names(summary) <-c("Direction", "PSL", "APO", "DFT")

	summary