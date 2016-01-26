library(ggplot2)
library(dplyr)
library(vcd)
library(gridExtra)


setwd("C:/Users/Matt/Dropbox/RClass/Data/data/MassShootings")

shootings <- read.csv("USMassShootings.csv",header=TRUE)

str(shootings)

#weapons obtained legally

ggplot(shootings,aes(Weapons.obtained.legally))+geom_bar(color='black', fill='blue')  +
  ggtitle("Weapons Obtained Legally") + xlab("")

#total victims by weapon legality

ggplot(shootings,aes(Weapons.obtained.legally,Total.Victims))+geom_bar(stat="identity",color='black', fill='blue')  +
  ggtitle("Weapons Obtained Legally") + xlab("")



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
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#66CC99",show.legend = FALSE) +scale_y_continuous(limits=c(0,80))+
  ggtitle("Killed") + ylab("Killed")

#Injured by Year

inj.yr.hist = ggplot(shootings,aes(Year,Injured))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#CC6666",show.legend = FALSE) +scale_y_continuous(limits=c(0,80))+
  ggtitle("Wounded")+ ylab("Wounded")

#Mental Health Issues (advanced warning) - Bar Chart

ggplot(shootings,aes(Mental.Health.Issues))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")


#Age Histogram

age.hist = ggplot(shootings,aes(Age))+geom_histogram(binwidth=5,fill="#3366ff",colour="black") + ylab("Number of Perpetrators") +
  ggtitle("Perpetrator Age")


shootings_m_r_a = shootings_m[,c("Age","Race")] #get a df with only race and age

ggplot(shootings_m_r_a,aes(Age,fill=Race)) +geom_histogram(binwidth=5,position='dodge',color='black')+
    ylab("Number of Perpetrators") + ggtitle("Perpetrators by Age and Race")

perp_race_age = ggplot(shootings_m_r_a,aes(Age,fill=Race)) +geom_histogram(binwidth=5,position='stack',color='black')+
  ylab("Number of Perpetrators") + ggtitle("Male Perpetrators by Age and Race")

#Age Density


ggplot(shootings_m[shootings_m$Race=='White',],aes(Age)) + 
  geom_histogram(aes(y=..density..),binwidt=5,fill='#FE2EF7',color='black')+
  geom_density(size = 1)+
  ggtitle("White Male Perpetrators")



#Density calculations binning males into 3 groups by ethnicity

#Carve out Men

shootings_m = shootings[shootings$Gender == 'Male',]  #create dataframe with only Males

#Add another column for the "fresh binning"


shootings_m$race_group = shootings_m$Race

#iterate through data and make only three bins: White, Black, and Other

for (i in 1:nrow(shootings_m)){
  if (shootings_m$Race[i] != "White" & shootings_m$Race[i] != "Black"){
     shootings_m$race_group[i] = "Other"
  } 
}

#Overlayed density plots White Black other
ggplot()+geom_density(aes(Age), colour = "red", data=shootings_m[shootings_m$race_group == 'White',])+
  geom_density(aes(Age), colour = "black", data=shootings_m[shootings_m$race_group == 'Black',])+
  geom_density(aes(Age), colour = "green", data=shootings_m[shootings_m$race_group == 'Other',])
 
qplot(data=shootings_m, x=Age,geom='density',color=race_group)+theme_bw()

ggtitle("White Male Perpetrators")


#Male/Female

sex.hist = ggplot(shootings,aes(Gender))+geom_bar(fill="#3366ff",colour="black")+ ylab("Number of Perpetrators") +
  ggtitle("Perpetrator Sex")

#Race

race.hist = ggplot(shootings,aes(Race))+geom_bar(fill="#3366ff",colour="black")+ ylab("Number of Perpetrators") +
  ggtitle("Perpetrator Ethnicity")+ theme(axis.text.x = element_text(angle = 30, hjust = 1))

#Prior Signs

ggplot(shootings,aes(PriorSignsMentalIllness))+geom_bar()+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Type

ggplot(shootings,aes(Type))+geom_bar(fill="grey",colour="black")+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#age vs. victims

ggplot(shootings,aes(Age,Total.Victims))+ geom_point()




#TEST



ggplot(shootings,aes(x=Year,y= Fatalities))+geom_bar(stat="identity",fill="#66CC99",colour="black")+
  ggtitle("Fatalities")

ggplot(shootings,aes(x=Race,y= Age))+geom_point(stat="identity",fill="#66CC99",colour="black")+
  ggtitle(expression(atop("Perpetrators",atop("(Ethnicity and Age)",""))))

ggplot(shootings_m,aes(x=Age,y= Race))+geom_point(stat="identity",fill="#66CC99",colour="black")+
  ggtitle(expression(atop("Perpetrators",atop("(Ethnicity and Age)",""))))

ScatterHist(shootings_m,Age,Race)


summary(shootings)

grid.arrange(events.yr,vic.yr.hist2 , ncol=2, nrow=2) # Events and Total Victims Combined Chart

grid.arrange(inj.yr.hist,fat.yr.hist , ncol=2, nrow=2) # Injured and Fatalities Combined Chart

grid.arrange(age.hist,sex.hist , ncol=2, nrow=2) # Age Sex Combined Chart

grid.arrange(race.hist, ncol=2, nrow=2) # race Chart

grid.arrange(age.hist,perp_race_age, nrow=2) # Age and Age by Race

ggplot(shootings,aes(Year,Injured))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#CC6666",show.legend = FALSE)+
  ggtitle("Injured")

perp_race_age

#groupby race and age for total events

shootings_m_tbl = tbl_df(shootings_m)


by_race_age = group_by(shootings_m_tbl,Race,Age)
by_race_age
summarise(by_race_age, sum = sum(Total.Victims))

#Counts of events by State

shootings_tbl = tbl_df(shootings)
by_state = group_by(shootings_tbl,State)
by_state_incidents = summarise(by_state, incidents = n())
top_14_states=head(arrange(by_state_incidents,desc(incidents)),8)

top_states = ggplot(top_14_states,aes(x=reorder(State,-incidents),incidents))+
  geom_bar(colour="black",fill="#ff9966",stat="identity")+ 
  ylab("Incidents") +
  xlab("")+
  scale_y_continuous(limit = c(0, 15))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Incidents by State")


#Victim Count by State

shootings_tbl = tbl_df(shootings)
by_state = group_by(shootings_tbl,State)
by_state_victims = summarise(by_state, victims = sum(Total.Victims))
by_state_victims = arrange(by_state_victims,desc(victims))
top_9_states = head(by_state_victims,9)

top_victims = ggplot(top_9_states,aes(x=reorder(State,-victims),victims))+
  geom_bar(colour="black",fill="#ff9966",stat="identity")+ 
  ylab("Victims") +
  xlab("")+
  #scale_y_continuous(limit = c(0, 15))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Victims by State")

grid.arrange(top_states,top_victims, ncol=1, nrow=2)

levels = c(10,30,50,75)
labels = c("10-29","30-49","50-74")
shootings_m$Age_Group = cut(shootings_m$Age, levels, labels = labels)

#mosaic

#mosaic_df = shootings[,c("Race","Gender","Age_Group")]
mosaic_tbl = table(shootings_m$race_group,shootings_m$Age_Group) #just put column 1 and column 2 in other df
head(mosaic_df,5)
mosaic(mosaic_tbl, shade=TRUE, legend=TRUE)
mosaic(~Gender + Age_Group + Race,data=mosaic_df, shade=TRUE, legend=TRUE)
str(mosaic_df)

mosaic(with(shootings,table(Race,Gender)),shade=TRUE, legend=TRUE)
class(shootings_m$Age_Group)

mosaic(shootings_m$race_group~shootings_m$Age_Group,shade=TRUE, legend=TRUE)
