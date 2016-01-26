#### Finding Upsets ####
# generates plot of upset percentages by year
combinedMatches <- mutate(combinedMatches, matchYear = substr(tourney_date, 1, 4))
ranksCombined <- combinedMatches[, c("winner_rank", "loser_rank", "winner_name", "loser_name", "matchYear")] # use this when just adding player names
ranksCombined <- mutate(ranksCombined, difference = loser_rank - winner_rank) # create difference column
ranksCombined <- filter(ranksCombined, winner_rank <= 200 & loser_rank <= 200) # subset to matches of top 200 players
ranksCombined_1 <- mutate(ranksCombined, upsetTrue = difference < 0, upsetFalse = difference > 0)
ranksCombined_1g <- group_by(ranksCombined_1, matchYear)


# plot pct of upsets by year
upsetPercents <- summarise(ranksCombined_1g, 
			   upsets = sum(upsetTrue), expecteds = sum(upsetFalse)) %>% 
		 mutate(., upsetPct = upsets / (upsets + expecteds)) # create upset % column
ggplot(upsetPercents, aes(x=matchYear, y = upsetPct)) + geom_bar(stat="identity")

# plot of average upset difference by year
upsetsOnly <- filter(ranksCombined_1g, upsetTrue == TRUE)
upsetDifByYear <- group_by(upsetsOnly, matchYear) %>% summarise(. , aveDiff = mean(difference))
ggplot(upsetDifByYear, aes(x=matchYear, y = aveDiff)) + 
	# geom_bar(stat = "identity") 
	geom_point()		   



