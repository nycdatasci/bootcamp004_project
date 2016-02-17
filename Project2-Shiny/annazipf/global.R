library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
library(data.table)
library(leaflet)
library(rgdal)
library(raster)
library(rvest)
library(reshape2)
library(DT)
library(sp)
if (!(exists('drscity') & 
      exists('spec') & 
      exists('school') & 
      exists('time') &
      exists('states') &
      exists('subgender') &
      exists('timesp') &
      exists('usa') &
      exists('change'))) 
{ #setwd("Documents/DataSci/bootcamp004_project/Project2-Shiny/annazipf/")
  print('loading data')
  #print(getwd())
  drscity = read.csv('drscity.csv.bz2')
  states = read.csv("states.csv")
  spec = read.csv("spec.csv")
  school = read.csv("school.csv")
  time = read.csv("time.csv")
  timesp = read.csv('timesp.csv')
  subgender = read.csv("subgender.csv")
  change = read.csv("change.csv")
  usa = readOGR("usa.shp", layer = "usa")
    drscity$X = NULL
    timesp$X = NULL
    subgender$X = NULL
    timesp = na.omit(timesp)
    drscity = na.omit(drscity)
    change = na.omit(change)
    
    #create the density map only once
    polygon_popup = paste0("<strong>", usa$NAME_1, "</strong>", "</br>", usa$percent)  
    pal = colorQuantile("Blues", NULL, n = 10)
    
#     densitymap = leaflet(usa) %>%
#       addTiles() %>%
#       setView(lat = 39.82, lng = -98.58, zoom = 4) %>%
#       addPolygons(data = usa, fillColor = ~colorQuantile("Blues", NULL, n = 10)(colornum), 
#                   fillOpacity = 0.6,
#                   weight = 2,
#                   color = "white",
#                   popup = polygon_popup )  %>%
#       addLegend("bottomright", pal = colorNumeric( palette = pal, domain = usa$percent ), values = ~percent,
#                 title = "State doctors per 100 people",
#                 opacity = 1)  
      
       print('done loading data')
}

subset = c("ADDICTION MEDICINE",
           "GENERAL PRACTICE",
           "NUCLEAR MEDICINE",
           "CARDIAC SURGERY",
           "PREVENTATIVE MEDICINE",
           "ALLERGY/IMMUNOLOGY",
           "THORACIC SURGERY",
           "CARDIOVASCULAR DISEASE (CARDIOLOGY)",
           "PSYCHIATRY",
           "GERIATRIC PSYCHIATRY")

changefilter = dplyr::filter(change, Primary.specialty %in% subset)
changefilter$NA. = NULL

#input names
names = unique(as.character(drscity$Primary.specialty))
stateselect = unique(as.character(drscity$State))
gender = unique(as.character(drscity$Gender))
cities = unique(as.character(drscity$NAME_2))
medschool = unique(as.character(drscity$Medical.school.name))






#cleaning the database
cleantable <- tbl_df(drscity) %>%
  dplyr::select(First.Name,
         Last.Name,
    City = NAME_2,
    State = State,
    Specialty = 7,
    Address = address,
    Clinic = 9,
    Medical.school = 5,
    Years.experience = experience
  ) 



#Plots

plot= ggplot(spec[1:20,], aes(x=reorder(Specialty,Number), y= Number)) + geom_bar(stat='identity',
aes( fill = Specialty)) + coord_flip()
plot = plot+ ggtitle("20 most popular medical disciplines") +xlab("Field") +ylab("Number of doctors")
plot = plot +theme(legend.position="none")

schoolp= ggplot(school[1:40,], aes(x=reorder(Medical.school.name,Count), y= Count)) + geom_bar(stat='identity',
aes( fill = Gender), position = "stack") + coord_flip()
schoolp = schoolp+ ggtitle("20 most popular medical schools") +xlab("Schools of medicine") +ylab("Number of graduates since 1951")
schoolp= schoolp+theme(legend.position="right")

timep=ggplot(data=time, aes(x=age, y = num)) +geom_point() + geom_line() +xlab("age") +ylab("Number")
timep = timep+ ggtitle("Age distribution of medicare staff") + 
  theme(axis.text = element_text(size = 14),
        legend.key = element_rect(fill = "navy"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) 

genderp = ggplot(data = subgender, aes(x = reorder(Primary.specialty,percent), y = as.numeric(percent)))  + 
  geom_bar(stat='identity', aes(fill=Gender), position ='stack') + coord_flip()
genderp = genderp + ggtitle("Most disparate male-female medical disciplines") +
  ylab("Percent of male/female") + xlab("Field") + theme_bw()

changep = ggplot(data=change, aes(x=experience, y = grads)) +geom_line(aes(color=Primary.specialty)) +
  xlab("Experience") +ylab("Number of professionals") +theme(legend.position="none")
changep = changep + ggtitle("Experience distribution of 77 sectors")  

changepisolate = ggplot(data=changefilter, aes(x=experience, y = grads)) +
  geom_line(aes(color=Primary.specialty)) +
  xlab("Experience") +ylab("Number of professionals") +theme(legend.position="right")
changepisolate = changepisolate + ggtitle("Top 10 highest experience fields")  +
  theme(axis.text = element_text(size = 14),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) + guides(col=guide_legend(title="Field"))

timespp = ggplot(data=timesp, aes(x=age, y = num)) +geom_point() + geom_line(aes(col=Gender)) +xlab("Age") +ylab("Number")
timespp = timespp+ ggtitle("Age distribution of medicare professionals by gender") + 
  theme(axis.text = element_text(size = 14),
        legend.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks=pretty(timesp$age, n=10)) + 
  scale_y_continuous(breaks=pretty(timesp$num, n=10))


