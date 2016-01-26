library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(maps)
library(ggthemes)
library(leaflet)
library(sp)
library(dplyr)
library(scales)
library(RColorBrewer)
library(plotly)
library(devtools)
library(gridExtra)
setwd("C:/Users/Matt/Dropbox/RClass/Data/data/MassShootings")

shootings <- read.csv("USMassShootings.csv",header=TRUE)

#Plotly Map

library(plotly)

df <- read.csv('USMassShootings.csv')
df$hover <- paste(df$Case,'<br>',
                  df$City,', ', df$State,'<br>', 
                  df$Date,'<br>',
                  'Fatalities: ',df$Fatalities, '<br>',
                  'Injured: ',df$Injured)

df$q <- with(df, cut(Total.Victims, quantile(Total.Victims)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

plot_ly(df, lon = longitude, lat = latitude, text = hover,
        marker = list(size = sqrt(Total.Victims) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'US Mass Shootings: 1982 - 2015', geo = g)

#Total Victims Histogram

a = ggplot(shootings,aes(Total.Victims))+geom_histogram(binwidth=5)+theme_bw()  +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Fatalities Histogram

b = ggplot(shootings,aes(Fatalities))+geom_histogram(binwidth=5)+theme_bw()  +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Injured Histogram

inj.hist = ggplot(shootings,aes(Injured))+geom_histogram(binwidth=5)+theme_bw()  +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Number of Events - Bar Chart

events.yr = ggplot(shootings,aes(Year))+geom_bar(fill="#ff9966",colour="black")+ ylab("Events") +
  ggtitle("Events")

#Victims by Year

vic.yr.hist = ggplot(shootings,aes(Year,Total.Victims))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#9999CC",show.legend = FALSE)+
  ggtitle(expression(atop("Victims",atop("(Injured + Fatalities)","")))) + labs(y="Victims")

vic.yr.hist2 = ggplot(shootings,aes(Year,Total.Victims))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#9999CC",show.legend = FALSE)+
  ggtitle("Victims") + labs(y="Victims")



#Fatalities by Year

fat.yr.hist = ggplot(shootings,aes(Year,Fatalities))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#66CC99",show.legend = FALSE) +scale_y_continuous(limits=c(0,70))+
  ggtitle("Fatalities")

#Injured by Year

inj.yr.hist = ggplot(shootings,aes(Year,Injured))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#CC6666",show.legend = FALSE) +scale_y_continuous(limits=c(0,70))+
  ggtitle("Injured")

#Mental Health Issues (advanced warning) - Bar Chart

ggplot(shootings,aes(Mental.Health.Issues))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")


#Age Histogram

g = ggplot(shootings,aes(Age))+geom_histogram(binwidth=5,fill="grey",colour="black") + theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Age Density

ggplot(shootings,aes(Age))+geom_density(color='blue', fill='blue')+scale_fill_hue(c=45,l=20)

#combined density histogram test

ggplot(shootings,aes(Age))+geom_dotplot()

#Male/Female

ggplot(shootings,aes(Gender))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Race

ggplot(shootings,aes(Race))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Prior Signs

ggplot(shootings,aes(PriorSignsMentalIllness))+geom_bar()+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Type

ggplot(shootings,aes(Type))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#age vs. victims

ggplot(shootings,aes(Age,Total.Victims))+ geom_point()


#leaflet

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10,alpha=NULL, stroke=FALSE))


#TEST

ggplot(shootings,aes(x=Year,y= Fatalities))+geom_bar(stat="identity",fill="#66CC99",colour="black")+
  ggtitle("Fatalities")

summary(shootings)

grid.arrange(events.yr,vic.yr.hist2 , ncol=2, nrow=2) # Events and Total Victims Combined Chart

grid.arrange(inj.yr.hist,fat.yr.hist , ncol=2, nrow=2) # Injured and Fatalities Combined Chart

ggplot(shootings,aes(Year,Injured))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#CC6666",show.legend = FALSE)+
  ggtitle("Injured")
