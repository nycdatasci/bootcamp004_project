
require(scales)

### READ IN ATP (MEN) MATCHES ####
setwd("C:/Users/bamdur/Documents/RTennis/data/")
combinedMatches <- read.csv('./atp_matches_1980.csv')
for(i in 1981:2015){ 
	filename <- paste('./atp_matches_', i, '.csv', sep = "")
	matchesHold <- read.csv(filename)
	combinedMatches <- rbind(combinedMatches, matchesHold)
}

### READ IN WTA (WOMEN) MATCHES ####
# NOTE: there are fewer total women's matches in the data set vs. men's 
# (123k men's matches and 96k women's matches)
setwd("C:/Users/bamdur/Documents/RTennis/data/wta/tennis_wta-master/")
combinedMatches <- read.csv('./wta_matches_1980.csv')
for(i in 1981:2015){
	filename <- paste('./wta_matches_', i, '.csv', sep = "")
	matchesHold <- read.csv(filename)
	if(i >= 2003) matchesHold <- matchesHold[ , 1:30]
	combinedMatches <- rbind(combinedMatches, matchesHold)
}
# build df of winner rank, loser rank, and difference in ranks
ranksCombined <- combinedMatches[, c("winner_rank", "loser_rank")] # use this when just adding player names
# play with next line for variables required for other analyses
#ranksCombined <- combinedMatches[, c("winner_rank", "loser_rank", "winner_name", "loser_name", "tourney_date", "tourney_name", "surface")]
ranksCombined$difference <- ranksCombined$loser_rank - ranksCombined$winner_rank # negative numbers indicate upset
ranksCombinedClean <- ranksCombined[complete.cases(ranksCombined), ]

limitRanks <- function (rankdf, limitno, minfreq, plotStartRank = 1, plotStopRank = 100){
# Takes in a 3 column df built above, plots according to other arguments, and returns
# 1) a matrix with each row depicting the winning percentage of each rank against every other rank, and
# 2) a matrix showing the number of matches each rank played against each other rank.
	# limit analysis to players ranked higher (lower ranking no)
	# than limitno
	rankdf <- rankdf[rankdf$winner_rank <= limitno 
		& rankdf$loser_rank <= limitno, ]
	
	# build a matrix of frequency counts, with winner rank as the
	# x axis and loser rank as the y axis
	wlMatrix <- matrix(0,limitno,limitno)
	for(i in 1:nrow(rankdf)){
		windex <- rankdf$winner_rank[i]
		lindex <- rankdf$loser_rank[i]
		wlMatrix[windex, lindex] <- wlMatrix[windex, lindex] + 1
	}
	
	winsDF <- as.data.frame(matrix(0,limitno,limitno))
	freqDF <- as.data.frame(matrix(0,limitno,limitno))
	# determine win percentage for each ranking against each 
	# other ranking
	for(i in 1:nrow(wlMatrix)){
		for(j in 1:nrow(wlMatrix)){
			if(i != j){
				wins <- wlMatrix[i,j]
				losses <- wlMatrix[j,i]
				if (wins + losses != 0){
					winpct <- round(wins / (wins + losses), 2)
					winsDF[i,j] <- winpct
					freqDF[i,j] <- wins + losses
				}
				else{
					winsDF[i,j] <- "DNP"
				}
			}
			else(
				winsDF[i,j] <- "X"
			)
		}
	}
	names(winsDF) <- as.integer(c(1:limitno))
	
	# plot the graph of all matches
	#par(mfrow = c(1,2))
	pal <- colorRampPalette(c("yellow", "black"))
	colors <- pal(limitno)
	plot(x = 1:limitno, ylim=c(0,1), type="n", las = 1,
	     xlab="Opponent's Rank", ylab="Win Percentage")

	for(i in plotStartRank:plotStopRank){ # each i is a row / winner ranking
		for(j in 1:limitno){ # each j is a value in the row (i.e. a win pct against another ranking)
			
			if(is.na(as.numeric(winsDF[i,j])) == FALSE & freqDF[i,j] > minfreq){
				points(jitter(j, 2), winsDF[i,j], pch=19, col=alpha(colors[i], 0.6), cex=2.8)
			}
		}
		
	}
	return(list(winsDF, freqDF))
}