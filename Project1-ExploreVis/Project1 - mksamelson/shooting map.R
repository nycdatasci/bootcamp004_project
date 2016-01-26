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
setwd("C:/Users/Matt/Dropbox/RClass/Data/data/MassShootings")

#Sys.setenv("plotly_username"="MattSamelson")
#Sys.setenv("plotly_api_key"="msdjq82czg")
#set_credentials_file(username=MattSamelson,api_key=msdjq82czg)


shootings <- read.csv("USMassShootings.csv",header=TRUE)

mapPoints <- ggmap(map='state')+
  geom_point(aes(x=longitude,y=latitude),data=shootings,alpha=.5, 
             colour="blue",size = sqrt(shootings$Total.Victims))
mapPoints

#Crappy Map using maps
map('state')
points(x=shootings$longitude, y=shootings$latitude, col="blue", size=sqrt(shootings$Total.Victims))


#Map with Google Maps

map <- get_map(location='USA', zoom = 4)
mapPoints <- ggmap(map)+
  geom_point(aes(x=longitude,y=latitude),data=shootings,alpha=.5, 
             colour="blue",size = sqrt(shootings$Total.Victims))+geom_jitter(aes(x=longitude,y=latitude),data=shootings)
mapPoints + ggtitle("US Mass Shootings: 1982 - 2015") + xlab("") + ylab("")



#Number of Events - Bar Chart

ggplot(shootings,aes(Year))+geom_bar(fill="grey",colour="black")+theme_bw()+ ylab("Events") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Victims

ggplot(shootings,aes(x=Year,y= Total.Victims))+geom_bar(stat="identity",fill="grey",colour="black")+theme_bw()  +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Fatalities

ggplot(shootings,aes(x=Year,y= Fatalities))+geom_bar(stat="identity",fill="grey",colour="black")+theme_bw()  +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Mental Health Issues (advanced warning) - Bar Chart

ggplot(shootings,aes(Mental.Health.Issues))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")


#Age Histogram

ggplot(shootings,aes(Age))+geom_histogram(binwidth=5,fill="grey",colour="black") + theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Male/Female

ggplot(shootings,aes(Gender))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Race

p = ggplot(shootings,aes(Race))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Prior Signs

ggplot(shootings,aes(PriorSignsMentalIllness))+geom_bar()+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")

#Type

ggplot(shootings,aes(Type))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1980 - 2015")



#leaflet

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10,alpha=NULL, stroke=FALSE))


summary(shootings)
