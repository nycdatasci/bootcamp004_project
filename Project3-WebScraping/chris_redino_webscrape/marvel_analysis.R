library(dplyr)#for reshaping
library(reshape2)
#library(ggplot2)
library(dygraphs)
library(xts)

marvel=read.csv("marvel_top5.csv",fileEncoding='utf16',stringsAsFactors = FALSE)
marvel=select(marvel,character,title,date)

#give characters reasonable names
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Steven_Rogers_%28Earth-616%29/Appearances"]="Captain_America" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Peter_Parker_%28Earth-616%29/Appearances"]="Spider_Man" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:James_Howlett_%28Earth-616%29/Appearances"]="Wolverine" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Anthony_Stark_%28Earth-616%29/Appearances"]="Iron_Man" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Thor_Odinson_%28Earth-616%29/Appearances"]="Thor" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Benjamin_Grimm_%28Earth-616%29/Appearances"]="Thing" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Reed_Richards_%28Earth-616%29/Appearances"]="Mr._Fantastic" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Robert_Bruce_Banner_%28Earth-616%29/Appearances"]="Hulk" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Scott_Summers_%28Earth-616%29/Appearances"]="Cyclops" 
marvel$character[marvel$character=="http://marvel.wikia.com/wiki/Category:Jonathan_Storm_%28Earth-616%29/Appearances"]="Human_Torch" 

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
Hulk=xts(marvel3$Hulk,order.by=marvel3$date)
Cyclops=xts(marvel3$Cyclops,order.by=marvel3$date)
Mr._Fantastic=xts(marvel3$Mr._Fantastic,order.by=marvel3$date)
Human_Torch=xts(marvel3$Human_Torch,order.by=marvel3$date)
Thing=xts(marvel3$Thing,order.by=marvel3$date)

marvelTS=cbind(Captain_America,Spider_Man,Wolverine,Iron_Man,Thor,Hulk,Cyclops,Mr._Fantastic,Human_Torch,Thing)
colnames(marvelTS) =c("Captain_America","Spider_Man","Wolverine","Iron_Man","Thor")

dygraph(marvelTS)%>% 
  dySeries("Captain_America", stepPlot = TRUE, color = "blue",fillGraph=TRUE) %>%
  dySeries("Spider_Man", stepPlot = TRUE, color = "red",fillGraph=TRUE)%>%
  dySeries("Wolverine", stepPlot = TRUE, color = "brown",fillGraph=TRUE)%>%
  dySeries("Iron_Man", stepPlot = TRUE, color = "gold",fillGraph=TRUE)%>%
  dySeries("Thor", stepPlot = TRUE, color = "gray",fillGraph=TRUE)%>%
  dyRangeSelector()

#want to get a "xkcd narrative chart" to show when characters meet, can adapt a sankey diagram?
