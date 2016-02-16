NYC_Jobs <- read.csv("/Volumes/64GB/NYC/project02/NYC_Jobs.csv", stringsAsFactors=FALSE)
   View(NYC_Jobs)
NYC_Jobs$PostingYear <- substr(NYC_Jobs$Posting.Date, 7, 10)
NYC_Jobs$PostingMonth <- substr(NYC_Jobs$Posting.Date, 1, 2)
NYC_Jobs$PostingDate <- substr(NYC_Jobs$Posting.Date, 4, 5)
NYC_Jobs$PrecessYear <- substr(NYC_Jobs$Process.Date, 7, 10)
NYC_Jobs$ProcessMonth <- substr(NYC_Jobs$Posting.Date, 1, 2)
NYC_Jobs$ProcessDate <- substr(NYC_Jobs$Posting.Date, 4, 5)

NYCjobYYMMDD$PostingMonthChar <- as.character(NYCjobYYMMDD$PostingMonth)
NYCjobYYMMDD$Posting.YYMM <- paste(NYCjobYYMMDD$PostingYear, NYCjobYYMMDD$PostingMonthChar, sep = "/")

write.csv(NYCjobYYMMDD, file = "NYCjobYYMMDD.csv")

library(dplyr)
salaryBy <- group_by(NYCjobYYMMDD, Salary.Frequency) %>% summarise(., total= n())
NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Daily','Salary.From.Year'] <-
	NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Daily','Salary.Range.From']*5*52
NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Hourly','Salary.From.Year'] <- 
	NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Hourly','Salary.Range.From']*8*5*52
NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Annual','Salary.From.Year'] <- 
	NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Annual','Salary.Range.From']

NYCjobYYMMDD$Salary.To.Year <- NYCjobYYMMDD$Salary.Range.To
NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Hourly','Salary.To.Year'] <- NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Hourly','Salary.Range.To']*8*5*52
NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Daily','Salary.To.Year'] <- NYCjobYYMMDD[NYCjobYYMMDD$Salary.Frequency=='Daily','Salary.Range.To']*5*52



totalYYMM <- group_by(NYCjobYYMMDD, YYMM) %>% summarise(., total = n())
groupByYear <- group_by(dateData, year) %>% summarise(., total = n())

totalYYMM <- group_by(NYCjobYYMMDD, Posting.YYMM) %>% summarise(., total = sum(X..Of.Positions) )
View(totalYYMM)

#levelPlotData for histogram by level
levelPlotDa <- NYCjobYYMMDD[NYCjobYYMMDD$Salary.Ave > 0, c("Level" ,"X..Of.Positions",  "Salary.From.Year",  "Salary.To.Year" , "Salary.Ave" )]
class((levelPlotDa$Level))
[1] "character"
levelPlotDa$Level <- as.factor(levelPlotDa$Level)
colnames(levelPlotDa)[2] <- "NPositions"
# Expand rows for histogram becuase of N. of positions.
endLine <- nrow(levelPlotDa)
for (i in 1: endLine) {
	if (levelPlotDa$NPositions[i] != 1) {
		for (j in 1: (levelPlotDa$NPositions[i]-1)) {
			levelPlotDa <- rbind(levelPlotDa, levelPlotDa[i,])
		}
	}
}

levelPlotDa <- levelPlotDa[levelPlotDa$Salary.From.Year > 0, ]
levelPlotDa$Level <- as.factor(levelPlotDa$Level)

write.csv(levelPlotDa, file = "/media/wan/64GB/NYC/project02/levelPlotDa.csv")

####Compare Outliar
outliarSala <- NYCjobYYMMDD[NYCjobYYMMDD$Salary.From.Year > 100000 & NYCjobYYMMDD$Level %in% c("00", "01", "02", "03", "04"),]
NoOutliarSala <- NYCjobYYMMDD[(NYCjobYYMMDD$Salary.From.Year < 60000 & NYCjobYYMMDD$Salary.From.Year > 40000 )& NYCjobYYMMDD$Level %in% c("00", "01", "02", "03", "04"),]

outliarByTitle <- group_by(outliarSala, Business.Title) %>% summarise(., total = n(), average = mean(Salary.From.Year)) %>% arrange(.,desc(total))
NoOutliarByTitle <- group_by(NoOutliarSala, Business.Title) %>% summarise(., total = n(), average = mean(Salary.From.Year)) %>% arrange(., desc(total))

OTitle <- outliarByTitle$Business.Title[1:10 ]
OTotal <- outliarByTitle$total[1:10]
OAver <- outliarByTitle$average[1:10]
compareOut <- data.frame(OTitle, OTotal, OAver)
compareOut$NTitle <- NoOutliarByTitle$Business.Title[1:10]
compareOut$NTotal <- NoOutliarByTitle$total[1:10]
compareOut$NAver <- NoOutliarByTitle$average[1:10]

##### tiem series data
selecColum <- c("X..Of.Positions", "Posting.YYMM", "Salary.From.Year", "Salary.To.Year", "Salary.Ave" )
timeJob <- NYCjobYYMMDD[, selecColum]
View(timeJob)
timeJob$Posting.YYMM <- as.factor(timeJob$Posting.YYMM)
timeJob$NumPositions <- timeJob$X..Of.Positions
timeJob$X..Of.Positions <- NULLtime
timeJob$Salary.From.Total <- timeJob$Salary.From.Year * timeJob$NumPositions
timeJob$Salary.To.Total <- timeJob$Salary.To.Year * timeJob$NumPositions
timeJob$Salary.Ave.Total <- timeJob$Salary.Ave * timeJob$NumPositions

 salaryByYYMM <- group_by(timeJob, Posting.YYMM) %>% summarise(., total= sum(NumPositions), 
+ SalaryFromAve = sum(Salary.From.Total)/sum(NumPositions),
+ SalaryToAve = sum(Salary.To.Total)/sum(NumPositions),
+ SalaryAveAve = sum(Salary.Ave.Total)/sum(NumPositions))

 write.csv(salaryByYYMM, file = "/media/wan/64GB/NYC/project02/salaryByYYMM.csv")

graphYYMM <- qplot(Posting.YYMM, SalaryFromAve, data = salaryByYYMM)
graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
 		panel.background = element_blank())


# Expand rows for histogram becuase of N. of positions.
endLine <- nrow(newNYC)
for (i in 1: endLine) {
	if (newNYC$NPositions[i] != 1) {
		for (j in 1: (newNYC$NPositions[i]-1)) {
			newNYC <- rbind(newNYC, newNYC[i,])
		}
	}
}