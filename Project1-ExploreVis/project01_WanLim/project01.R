
Consumer_Complaints <- read.csv("/media/wan/64GB/R_CLASS/R_Project_Data/Consumer_Complaints.csv", stringsAsFactors=FALSE)

#separate into Year and Month
Consumer_Complaints$year <- substr(Consumer_Complaints$Date.received, 7, 10)
Consumer_Complaints$month <- substr(Consumer_Complaints$Date.received, 1, 2)

#For By month and year
library(dplyr)
dateData <- select(Consumer_Complaints, year, month)
View(dateData)
groupByYear <- group_by(dateData, year) %>% summarise(., total = n())
ByYearMonth <- group_by(dateData, year, month) %>% summarise(., total = n())
ByYearMonth$seq <- c(1:nrow(ByYearMonth))

regre <- lm(total ~ seq, data=ByYearMonth)
summary(regre)
plot(total ~ seq, data = ByYearMonth, ylab = "Number of reports", xlab = "Time series  by month from 2011/12 ~ 2015/11")
abline(regre)

#top10 company
Consumer_complainYearMonth <- read.csv("/media/wan/64GB/Documents/DataScience/porject/porject01/Consumer_complainYearMonth.csv")
companyRank <- group_by(Consumer_complainYearMonth, Company) %>% summarise(., total = n()) %>% arrange(., desc(total) )
companyRank$percentage <- companyRank$total/nrow(Consumer_complainYearMonth)*100
companyRank$SumOfPercent[1] <- 10.6135
for(i in 2: nrow(companyRank)) {
	companyRank$SumOfPercent[i] <- companyRank$SumOfPercent[i-1] +companyRank$percentage[i]	
}
companyRank


top10Company <- companyRank[1:10,]
top10Company$Company <- factor(top10Company$Company, levels = top10Company$Company)
top10Company$percentage <- top10Company$total/nrow(Consumer_complainYearMonth)*100

library(ggplot2)
barCompany <- ggplot(top10Company, aes(x=Company, y=percentage)) + geom_bar(stat="identity")
barCompany + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + ylab("%")


#Sort by Porduct

byProduct <- group_by(Consumer_complainYearMonth, Product) %>% summarise(., total= n() ) %>% arrange(., desc(total))

	# barGraph
byProduct$Product <- factor(byProduct$Product, levels = byProduct$Product)
byProduct$Percentage <- byProduct$total/nrow(Consumer_complainYearMonth)*100
barPro <- ggplot(byProduct, aes(x=Product, y=Percentage)) + geom_bar(stat="identity")
barPro + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank()) + ylab("%")


#ByTop10CompanyGroup
CcTop10Company <- filter(Consumer_complainYearMonth, Company %in% top10Company$Company)
CcTop10Company$Company <- factor(CcTop10Company$Company, levels = top10Company$Company)
byCompanyProduct <- group_by(CcTop10Company, Company, Product) 
byCompanyProductTotal <- summarise(byCompanyProduct, total = n())
byCompanyProductArrange <- arrange(byCompanyProductTotal, desc(total))

library(lattice)
groupXYplot <- xyplot(byCompanyProductArrange$total ~ byCompanyProductArrange$Product | byCompanyProductArrange$Company, data=byCompanyProductArrange, xlab = list(label = "Product", cex=1), scales=list(x=list(rot=45)), groups = Product)
groupXYplot

#By State
onlyState <- select(Consumer_complainYearMonth, State)
byState <- group_by(onlyState, State) %>% summarise(., total = n())
byState <- byState[-1,]


onlyState <- select(Consumer_complainYearMonth, State)
byState <- group_by(onlyState, State) %>% summarise(., total = n())
byState <- byState[-1,]

stateAbb <- read.csv("/media/wan/64GB/R_CLASS/Project/data/stateAbb.csv", stringsAsFactors=FALSE)
fullName <- c()
total <- c()
byStateFullname <- data.frame(fullName, total)
for (i in 1: nrow(byState)) {
	posi <- match(byState$State[i], stateAbb$abbreviation)
	if (!is.na(posi)) {
		lastPosi <- length(fullName) + 1
		fullName[lastPosi] <- stateAbb$state.name[posi]
		total[lastPosi] <- byState$total[i]
	} 
}

byState <- data.frame(fullName, total)

library(maps)
library(ggplot2)
all_state <-map_data("state")
byState$region <- byState$fullName
Total <- merge(all_state, byState, by="region")
Total <- Total[Total$region!="district of columbia",]
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$total),colour="white"
      ) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p1 <- p + theme_bw()  + labs(fill = "Total Reports" 
                            ,title = "Total Reports by State", x="", y="")
p1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())

#via group by year month
byMediaYearMonth <- group_by(Consumer_complainYearMonth, year, month, Submitted.via) %>% summarise(., total = n())
byMediaYearMonth <- arrange(byMediaYearMonth, total)

byMediaYearMonth$month <- as.character(byMediaYearMonth$month)
for (i in 1: nrow(byMediaYearMonth)) {
	if ( nchar(byMediaYearMonth$month[i], type = "chars", allowNA = FALSE, keepNA = NA) == 1) {
		byMediaYearMonth$month[i] <- paste(0, byMediaYearMonth$month[i], sep = "" )
	}
}

byMediaYearMonth$yymm <- paste(byMediaYearMonth$year, byMediaYearMonth$month, sep = "/")
byMediaYearMonth <- filter(byMediaYearMonth, yymm != "2015/12")
	
	#graph
graph <- qplot(yymm, total, data = byMediaYearMonth, color = Submitted.via)
graph45 <- graph +  theme(axis.text.x = element_text(angle = 45,size = 14, hjust = 1), panel.background = element_blank())

		#ticks interval 5
yymm <- byMediaYearMonth$yymm
yymm <- unique(yymm)
YearMonth <- data.frame(yymm, stringsAsFactors=FALSE)

YearMonth$yymm <- as.character(YearMonth$yymm)
ticks5 <- c()
lastRow <- nrow(YearMonth)
for (i in 1: lastRow) {
 if (i %% 5 == 0) {
 	ticks5[i] <- YearMonth$yymm[i]
 	cat("yymm", YearMonth$yymm[i])
 } else {
 	ticks5[i] <- ""
 }
}

graph45five <- graph45 + scale_x_discrete(breaks = ticks5,labels =ticks5)
graph45five + theme(axis.text.x = element_text(angle = 45, hjust = 1))
