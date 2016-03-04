 
#Visualising Chess ratings data by FIDE


#installing packages

install.packages('rworldmap')
install.packages('RColorBrewer')
install.packages('classInt')




library(dplyr)
library(ggplot2)
library(rworldmap)
library(RColorBrewer)
library(classInt)

setwd('C://dataset')
chess=read.csv('ratingsfinal.csv',header=TRUE)
chess=tbl_df(chess)

chess_all=read.csv('allyears.csv',header=TRUE)
chess_all$Year=as.factor(chess_all$Year)
chess$age=2016-chess$B.day;

chess_clean= chess[chess$B.day>1900,]

chess_clean=tbl_df(chess_clean)


chess_active=filter(chess_clean, Flag=='' | Flag=='w')
titled= subset(chess,Title=='GM'|Title=='IM'|Title=='FM'|Title=='CM'|Title=='WGM'|Title=='WIM'|Title=='WFM'|Title=='WCM',)

title_clean= titled[titled$B.day>1900,]
labels=c('Under 10 years','10 to 20 yrs','20 to 40 yrs','40 to 60','60+')


title_clean$age_group = cut(title_clean$age, breaks=c(0,10,20,40,60,100),labels=labels,lowest=TRUE)
title_active=filter(title_clean, Flag=='' | Flag=='w')


chess_clean$age_group = cut(chess_clean$age, breaks=c(0,10,20,40,60,100),labels=labels,lowest=TRUE)


group1=group_by(chess_clean,Fed.Full.name)
y=summarise(group1,total=n(),F=sum(Sex=='F'),M=sum(Sex=='M'),inactive=sum(Flag=='i'| Flag=='wi'), avrating=mean(Rating), avage=mean(age),pop=max(Population),totgames=(sum(Gms)),
 CM=sum(Title=='CM'),WCM=sum(Title=='WCM'),FM=sum(Title=='FM'),WFM=sum(Title=='WFM'),IM=sum(Title=='IM'),WIM=sum(Title=='WIM'),
GM=sum(Title=='GM'),WGM=sum(Title=='WGM'))

y$femaleratio=y$F/y$total
y$inactivity=y$inactive/y$total
y$probabofgm=y$GM/y$pop
y$masters=y$CM+y$WCM+y$FM+y$WFM+y$WIM+y$IM+y$GM+y$WGM
y$probabofmaster=y$masters/y$pop

group2=group_by(title_active,Fed.Full.name,Title)
titlefed=summarise(group2, count=n(),rating=mean(Rating))

chess_clean_inactive=chess_clean[chess_clean$Flag=='wi'| chess_clean$Flag=='i',]



bw =diff(range(chess$Rating)) / (2 * IQR(chess$Rating) / length(chess$Rating)^(1/3))

#How does the rating distribution look like

x1=qplot(Rating,data=chess_active,fill=Sex,binwidth=20)+geom_histogram(colour="black")+ggtitle("Rating distribution at a glance")


#Rating distribution across years

ggplot(chess_all[chess_all$Rating>2500,],aes(x=Rating,fill=Year))+geom_density(alpha=0.6)

)




qplot(class, hwy, data = mpg,geom = "boxplot")
qplot(Year,Rating, data=chess_all[chess_all$Rating>2500,],geom="boxplot")


#What about between Male and Female

x2=ggplot(chess_active,aes(x=Rating,fill=Sex))+geom_density(alpha=0.3)+ggtitle("Rating Density plots across Sex")
x3=ggplot(chess,aes(x=Rating,fill=Sex))+geom_density(alpha=0.3)


#between inactive players
ggplot(chess_clean_inactive,aes(x=Rating,fill=Sex))+geom_density(alpha=0.3)+ggtitle("Rating Density plots across inactive players")




#Who are Grandmasters.
View(title_clean)
qplot(Rating,data=title_clean[title_clean$Rating>2000,],fill=Title,binwidth=40)+ggtitle("Distribution of Masters")



#You walk into a Chess club finding a twelve year old battling it out against an aged Veteran. Who is more likely to win?

 qplot(age,Rating, data =chess_clean, color=Sex)+geom_point()+ggtitle("Universal across age")


 # Between two women Masters?
 qplot(age,Rating, data = filter(title_active,Sex=='F' & Rating>2200),color=Title)+geom_point()
 
 
 # What about a game between Grandmasters?

 x=qplot(age,Rating, data = chess_active[chess_active$Title=='GM',],color=Sex)+geom_point()+stat_smooth(method=lm,se=FALSE,colour="Grey")+ggtitle("Grandmaster Ratings across age")
 
 x+annotate("text",x=30,y=2250,label="y= 2675.8 - 3.84x  R^2=0.3")

 qplot(age,Rating, data = filter(chess_active,Rating>2600),color=Sex)+geom_point()+stat_smooth(method=lm,se=FALSE,colour="Grey")
 
                                                                                                      
 # how is age distributed ,especially between Males and Females
 
 
   qplot(age,data=chess_active,binwidth=2,fill=Sex)
   ggplot(chess_active,aes(x=age,fill=Sex))+geom_density(alpha=0.3)+ggtitle("Age distribution discrepancy")
 
 # Men are able to devote more time into Competitive chess after the age of 25. Perhaps this 
 #also explains the lower ratings observed for women in Competitive chess. 
   

 # Map data 
   
   colourPalette <- brewer.pal(5,'RdPu')
   
   #brewerList <- c("Greens","Greys","Oranges","OrRd" ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOr","Rd")
   
   mapDevice('x11')
   aravind=joinCountryData2Map(y,joinCode="NAME",nameJoinColumn = "Fed.Full.name")
   aravind1=joinCountryData2Map(y[y$total>100,],joinCode="NAME",nameJoinColumn = "Fed.Full.name")
  
   
   y1=y[y$total>100,]
   
   

   
   y1_counttotal=arrange(y1,desc(total))
   output_tree=y1_counttotal[1:25,]
   
  #map.market(id=output_tree$Fed.Full.name,area=output_tree$total,group=output_tree$Fed.Full.name,color=output_tree$avage,main="Tree map of Big Players")

  #treemap(output_tree,index=output_tree$Fed.Full.name, vSize=output_tree$total, vColor=output_tree$avage,  title='Tree map of Leading Federations' , palette='RdBu' )
   
   
    mapCountryData(aravind1,nameColumnToPlot ="avage",mapTitle = 'Average Player age across the World',oceanCol = "Black")
    
    y1_age=arrange(y1,desc(avage))
    y1_age[1:10,c(1,2,7)]
  
    
    y1_ageasc=arrange(y1,avage)
    y1_ageasc[1:10,c(1,2,7)]
    
    
   mapCountryData(aravind1,nameColumnToPlot ="probabofgm",mapTitle = 'Probability of running into a GrandMaster',colourPalette = colourPalette)
   y1_probofgm=arrange(y1,desc(probabofgm))
   y1_probofgm[1:10,c(1,2,20)]
   
   
   mapCountryData(aravind1,nameColumnToPlot ="femaleratio",mapTitle = 'Percentage of Females ',colourPalette =brewer.pal(7,"Greens"))
   y1_femaleratio=arrange(y1,desc(femaleratio))
   y1_femaleratio[1:10,c(1,2,18)]
   
   y1_femaleratiosasc=arrange(y1,femaleratio)
   y1_femaleratiosasc[1:10,c(1,2,18)]
   
   
   

   mapCountryData(aravind1,nameColumnToPlot ="inactivity",mapTitle = 'Countries with most Active Players',colourPalette =brewer.pal(7,"OrRd"),oceanCol = "Black")
   y1_active=arrange(y1,desc(activity))
   y1_active[1:10,c(1,2,23)]
   
   
   y1_counttotal=arrange(y1,desc(total))
   y1_counttotal[1:10,c(1,2)]
   
   
   y1_countmaster=arrange(y1,desc(masters))
   
   output=y1_countmaster[1:10,c(1,2,10:17,20)]
   
   outputn=y1_countmaster[1:10,1]
   powerhouse=filter(title_active, Fed.Full.name %in% outputn$Fed.Full.name)
  
   stacked=qplot(Fed.Full.name,data =powerhouse, geom = "bar", fill = Title)+coord_flip()+ylab("Number of Active Titled Players")+ xlab("")+ ggtitle("The Chess Powerhouses")
  
   
   mapCountryData(aravind1,nameColumnToPlot ="avrating",mapTitle = 'Countries with Strongest Players',colourPalette =brewer.pal(7,"OrRd"),oceanCol = "Black")
   
   
   
   
   
