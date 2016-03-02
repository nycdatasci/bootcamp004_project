library(dplyr)#for reshaping
library(reshape2)
library(ggplot2)
library(dygraphs)
library(xts)
library(lubridate)

marvel=read.csv("marvel_top5.csv",fileEncoding='utf16',stringsAsFactors = FALSE)
marvel=select(marvel,character,title,date)

#give characters reasonable names
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Steven_Rogers_%28Earth-616%29/Appearances"]="Captain_America" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Peter_Parker_%28Earth-616%29/Appearances"]="Spider_Man" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:James_Howlett_%28Earth-616%29/Appearances"]="Wolverine" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Anthony_Stark_%28Earth-616%29/Appearances"]="Iron_Man" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Thor_Odinson_%28Earth-616%29/Appearances"]="Thor" 


#clean up the dates

#remove issues with no dates
marvel=filter(marvel,date!="")#dates with empty strings

#throw away url bits we don't need

convert_dates=function(date){
  return (strsplit(strsplit(date,":")[[1]][3],'"')[[1]][1] ) 
}
marvel$date=sapply(marvel$date,convert_dates)
#convert to dates, arbirtarily assigining each to the first of the month, just so it can convert to a date time.
marvel$date=as.Date(paste("1,",marvel$date),format='%d, %Y, %B')

#clean up dates that filed to convert well
marvel=filter(marvel,!is.na(date))#NA dates

#now that I have the data in the basic form I want I can start to play with it



#look at appearances per month over time
marvel2=marvel#make a copy before i mess things up
marvel2=group_by(marvel,character,date)
marvel2=summarise(marvel2,appearances=n())

marvel3=dcast(marvel2, date~character,value.var="appearances")
marvel3[is.na(marvel3)]=0# replace NA with zero 


#convert to xts
Captain_America=xts(marvel3$Captain_America,order.by=marvel3$date)
Spider_Man=xts(marvel3$Spider_Man,order.by=marvel3$date)
Wolverine=xts(marvel3$Wolverine,order.by=marvel3$date)
Iron_Man=xts(marvel3$Iron_Man,order.by=marvel3$date)
Thor=xts(marvel3$Thor,order.by=marvel3$date)


marvelTS=cbind(Captain_America,Spider_Man,Wolverine,Iron_Man,Thor)
colnames(marvelTS) =c("Captain_America","Spider_Man","Wolverine","Iron_Man","Thor")

dygraph(marvelTS)%>% 
  dySeries("Captain_America", stepPlot = TRUE, color = "blue",fillGraph=TRUE) %>%
  dySeries("Spider_Man", stepPlot = TRUE, color = "red",fillGraph=TRUE)%>%
  dySeries("Wolverine", stepPlot = TRUE, color = "brown",fillGraph=TRUE)%>%
  dySeries("Iron_Man", stepPlot = TRUE, color = "gold",fillGraph=TRUE)%>%
  dySeries("Thor", stepPlot = TRUE, color = "gray",fillGraph=TRUE)%>%
  dyLegend(width = 800)%>%
  dyRangeSelector()

#want to show when characters meet
#first step is table of issues and characters, to see when they coexist
marvel4=marvel#make a copy before playing
marvel4$appears=1#before reshaping all characters should always appear in the issues they are listed for
marvel4=dcast(marvel4, title~character,value.var="appears")
marvel4[is.na(marvel4)]=0# replace NA with zero 
marvel4$appear_sum=marvel4$Captain_America+marvel4$Spider_Man+marvel4$Wolverine+marvel4$Iron_Man+marvel4$Thor
marvel4$binary=paste(marvel4$Captain_America,marvel4$Spider_Man,marvel4$Wolverine,marvel4$Iron_Man,marvel4$Thor,sep="")
marvel5=marvel4 #im gunna need another copy soon
marvel4=group_by(marvel4,appear_sum,binary)
marvel4=summarise(marvel4,n())#aggregate on anything
marvel4=arrange(marvel4,appear_sum)
marvel4$state=index(marvel4)*15##state number unique identifies which characters appear at same time
marvel5=left_join(marvel5,marvel4, by="binary")#now i have the "state" of each issue
marvel5=select(marvel5,title,state)#get rid of stuff I don't need
#now need to make a table of dates and characters showing all states of characters
marvel6=left_join(marvel,marvel5,by="title")
#recast the data
marvel7=dcast(marvel6, title+date~character,value.var="state")
#now need to offset dates for character so points don't overlap, do this in the y direction (state)
marvel7$Iron_Man=marvel7$Iron_Man+2
marvel7$Captain_America=marvel7$Captain_America+8
marvel7$Spider_Man=marvel7$Spider_Man+6
marvel7$Wolverine=marvel7$Wolverine+4
#now convert to a a dygraph plot like the last one

#convert to xts
Captain_America=xts(marvel7$Captain_America,order.by=marvel7$date)
Spider_Man=xts(marvel7$Spider_Man,order.by=marvel7$date)
Wolverine=xts(marvel7$Wolverine,order.by=marvel7$date)
Iron_Man=xts(marvel7$Iron_Man,order.by=marvel7$date)
Thor=xts(marvel7$Thor,order.by=marvel7$date)
title=xts(marvel7$title,order.by=marvel7$date)

marvelTS2=cbind(Captain_America,Spider_Man,Wolverine,Iron_Man,Thor)
colnames(marvelTS2) =c("Captain_America","Spider_Man","Wolverine","Iron_Man","Thor")

dygraph(marvelTS2)%>% 
  dySeries("Captain_America", stepPlot = FALSE, color = "blue",fillGraph=FALSE,strokeWidth=0.0,drawPoints=TRUE) %>%
  dySeries("Spider_Man", stepPlot = FALSE, color = "red",fillGraph=FALSE,strokeWidth=0.0,drawPoints=TRUE)%>%
  dySeries("Wolverine", stepPlot = FALSE, color = "brown",fillGraph=FALSE,strokeWidth=0.0,drawPoints=TRUE)%>%
  dySeries("Iron_Man", stepPlot = FALSE, color = "gold",fillGraph=FALSE,strokeWidth=0.0,drawPoints=TRUE)%>%
  dySeries("Thor", stepPlot = FALSE, color = "gray",fillGraph=FALSE,strokeWidth=0.0,drawPoints=TRUE)%>%
  dyLegend(width = 800)%>%
  dyRangeSelector()





#also want to look at team membership, need another scrapped data set to do this
marvel_team=read.csv("marvel_teams.csv",fileEncoding='utf16',stringsAsFactors = FALSE)
marvel_team=select(marvel_team,date,Captain_America=cap,Iron_Man=ironman,Thor=thor,Spider_Man=spidey,Wolverine=logan)


#clean up the dates, same procedure as for the last frame

#remove issues with no dates
marvel_team=filter(marvel_team,date!="")#dates with empty strings

#throw away url bits we don't need

marvel_team$date=sapply(marvel_team$date,convert_dates)
#convert to dates, arbirtarily assigining each to the first of the month, just so it can convert to a date time.
marvel_team$date=as.Date(paste("1,",marvel_team$date),format='%d, %Y, %B')

#clean up dates that filed to convert well
marvel_team=filter(marvel_team,!is.na(date))#NA dates

#want to convert team appearances into  running team membership 
marvel_team2=marvel_team#first make a copy, in case I want to use membership per issue for some reason

#aggregate over dates first, so i can get running total at each date
marvel_team2=aggregate(. ~ date, data=marvel_team2, FUN=sum)

marvel_team2=mutate(marvel_team2,Captain_America=cumsum(Captain_America))
marvel_team2=mutate(marvel_team2,Iron_Man=cumsum(Iron_Man))
marvel_team2=mutate(marvel_team2,Thor=cumsum(Thor))
marvel_team2=mutate(marvel_team2,Spider_Man=cumsum(Spider_Man))
marvel_team2=mutate(marvel_team2,Wolverine=cumsum(Wolverine))

marvel_team2$total_membership=marvel_team2$Captain_America+marvel_team2$Iron_Man+marvel_team2$Thor+marvel_team2$Spider_Man+marvel_team2$Wolverine

#now follow similiar procedure as before to make it a dygraph

#convert to xts
Captain_America=xts(marvel_team2$Captain_America,order.by=marvel_team2$date)
Spider_Man=xts(marvel_team2$Spider_Man,order.by=marvel_team2$date)
Wolverine=xts(marvel_team2$Wolverine,order.by=marvel_team2$date)
Iron_Man=xts(marvel_team2$Iron_Man,order.by=marvel_team2$date)
Thor=xts(marvel_team2$Thor,order.by=marvel_team2$date)
title=xts(marvel_team2$title,order.by=marvel_team2$date)

marvelTS3=cbind(Captain_America,Spider_Man,Wolverine,Iron_Man,Thor)
colnames(marvelTS3) =c("Captain_America","Spider_Man","Wolverine","Iron_Man","Thor")

dygraph(marvelTS3)%>% 
  dySeries("Captain_America", stepPlot = FALSE, color = "blue",fillGraph=FALSE,drawPoints=FALSE) %>%
  dySeries("Spider_Man", stepPlot = FALSE, color = "red",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Wolverine", stepPlot = FALSE, color = "brown",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Iron_Man", stepPlot = FALSE, color = "gold",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Thor", stepPlot = FALSE, color = "gray",fillGraph=FALSE,drawPoints=FALSE)%>%
  dyLegend(width = 800)%>%
  dyRangeSelector()


#also want to look at team membership that is normalized
marvel_team3=marvel_team2#make another copy
marvel_team3$Captain_America=marvel_team3$Captain_America/marvel_team3$total_membership
marvel_team3$Iron_Man=marvel_team3$Iron_Man/marvel_team3$total_membership
marvel_team3$Thor=marvel_team3$Thor/marvel_team3$total_membership
marvel_team3$Spider_Man=marvel_team3$Spider_Man/marvel_team3$total_membership
marvel_team3$Wolverine=marvel_team3$Wolverine/marvel_team3$total_membership

#now follow similiar procedure as before to make it a dygraph

#convert to xts
Captain_America=xts(marvel_team3$Captain_America,order.by=marvel_team3$date)
Spider_Man=xts(marvel_team3$Spider_Man,order.by=marvel_team3$date)
Wolverine=xts(marvel_team3$Wolverine,order.by=marvel_team3$date)
Iron_Man=xts(marvel_team3$Iron_Man,order.by=marvel_team3$date)
Thor=xts(marvel_team3$Thor,order.by=marvel_team3$date)
title=xts(marvel_team3$title,order.by=marvel_team3$date)

marvelTS4=cbind(Captain_America,Spider_Man,Wolverine,Iron_Man,Thor)
colnames(marvelTS4) =c("Captain_America","Spider_Man","Wolverine","Iron_Man","Thor")

dygraph(marvelTS4)%>% 
  dySeries("Captain_America", stepPlot = FALSE, color = "blue",fillGraph=FALSE,drawPoints=FALSE) %>%
  dySeries("Spider_Man", stepPlot = FALSE, color = "red",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Wolverine", stepPlot = FALSE, color = "brown",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Iron_Man", stepPlot = FALSE, color = "gold",fillGraph=FALSE,drawPoints=FALSE)%>%
  dySeries("Thor", stepPlot = FALSE, color = "gray",fillGraph=FALSE,drawPoints=FALSE)%>%
  dyLegend(width = 800)%>%
  dyRangeSelector()
