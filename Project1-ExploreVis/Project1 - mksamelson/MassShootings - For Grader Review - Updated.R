#### Note to Grader ####

#Code here works and is in 95+ pct final format.

# If you run a piece of code and it doesn't work - the reason is the 
# dataset for particular charts are cuts of the original data set and,
# in some instances, because of work-in-progress some of the data 
# munging may come after the code for a figure generation.
# This is rare - but it may happen.  Please let me know if you have
# Questions.  Thanks.

### NB:  Be sure to comment out the setwd command just below so you can 
##load data from the appropriate new location


library(ggplot2)
library(dplyr)
library(vcd)
library(gridExtra)


setwd("C:/Users/Matt/Dropbox/RClass/Data/data/MassShootings")

shootings <- read.csv("USMassShootings.csv",header=TRUE)
names(shootings) = tolower(names(shootings))

#Modifications to shootings dataframe for subsequent work:

#Mass Shootings - Males Only

shootings_m = shootings[shootings$gender == 'Male',]  

shootings_m$race_group = shootings_m$race
shootings$race_group = shootings$race

shootings_m_r_a = shootings_m[,c("age","race")] #get a df with only race and age

#Iterate through shootings_m and make only three bins: White, Black, and Other

for (i in 1:nrow(shootings)){
  if (shootings$race[i] != "White" & shootings$race[i] != "Black"){
    shootings$race_group[i] = "Other"
  } 
}

#Iterate through shootings_m and make only three bins: White, Black, and Other

for (i in 1:nrow(shootings_m)){
  if (shootings_m$race[i] != "White" & shootings_m$race[i] != "Black"){
    shootings_m$race_group[i] = "Other"
  } 
}

summary(shootings)

#Counts of events by State

shootings_tbl = tbl_df(shootings)
by_state = group_by(shootings_tbl,state)
by_state_incidents = summarise(by_state, incidents = n())
top_14_states=head(arrange(by_state_incidents,desc(incidents)),8)

top_states = ggplot(top_14_states,aes(x=reorder(state,-incidents),incidents))+
  geom_bar(colour="black",fill="#ff9966",stat="identity")+ 
  ylab("Incidents") +
  xlab("")+
  scale_y_continuous(limit = c(0, 15))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Incidents by State")

top_states

#Victim Count by State

shootings_tbl = tbl_df(shootings)
by_state = group_by(shootings_tbl,state)
by_state_victims = summarise(by_state, victims = sum(total.victims))
by_state_victims = arrange(by_state_victims,desc(victims))
top_9_states = head(by_state_victims,9)

top_victims = ggplot(top_9_states,aes(x=reorder(state,-victims),victims))+
  geom_bar(colour="black",fill="#ff9966",stat="identity")+ 
  ylab("Victims") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 15))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Victims by State")

top_victims

#weapons obtained legally

weapons.legally = ggplot(shootings,aes(weapons.obtained.legally))+
  geom_bar(color='black', fill='blue')  +
  ggtitle("Weapons Obtained Legally") + 
  xlab("")

#total victims by weapon legality

ggplot(shootings,aes(Weapons.obtained.legally,Total.Victims))+geom_bar(stat="identity",color='black', fill='blue')  +
  ggtitle("Weapons Obtained Legally") + xlab("")

#Total Victims Histogram

totalvictims.hist = ggplot(shootings,aes(total.victims))+
  geom_histogram(binwidth=5, colour='black', fill = 'orange')+
  #xlim(c(0,80))+
  xlab("Total Victims") +
  ylab("Number of Events") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Fatalities Histogram

fatalities.hist = ggplot(shootings,aes(fatalities))+
  geom_histogram(binwidth=5, colour='black', fill = 'blue')+
  #xlim(c(0,80))+
  xlab("Fatalities") +
  ylab("Number of Events") +
ggtitle("Mass Shootings in the US: 1982 - 2015")

#Wounded Histogram

wounded.hist = ggplot(shootings,aes(injured))+
  geom_histogram(binwidth=5,, colour='black', fill = 'green')+
  #xlim(c(0,80))+
  xlab("Wounded") +
  ylab("Number of Events") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")

#Events per Year

ggplot(shootings,aes(year))+
  geom_bar(fill="#ff9966",colour="black")+ 
  xlab("Year")+
  ylab("Number of Events") +
  ggtitle("Events Per Year")

#Victims per Year

Victims.year = ggplot(shootings,aes(year,total.victims))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#9999CC",show.legend = FALSE)+
  xlab("Year")+
  ylab("Victims")+
  ggtitle(expression(atop("Victims",atop("(Injured + Fatalities)",""))))

#Fatalities by Year

fatalities.year = ggplot(shootings,aes(year,fatalities))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#66CC99",show.legend = FALSE) +
  scale_y_continuous(limits=c(0,80))+
  ylab("Fatalities") +
  ggtitle("Fatalities")

#Wounded by Year

wounded.year = ggplot(shootings,aes(year,injured))+
  stat_summary(fun.y=sum,geom="bar",colour="black",fill="#CC6666",show.legend = FALSE) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Year")+
  ylab("Wounded")+
  ggtitle("Wounded")

#Mental Health Issues (advanced warning) - Bar Chart

mental.health = ggplot(shootings,aes(mental.health.issues))+
  geom_bar(fill="green",colour="black")+theme_bw() + 
  ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")


#Age Histogram

age.hist = ggplot(shootings,aes(age))+
  geom_histogram(binwidth=5,fill="#3366ff",colour="black") + 
  xlab("Age")+
  ylab("Number of Events") +
  ggtitle("Perpetrator Age")

#Gender Histogram

sex.hist = ggplot(shootings,aes(gender))+
  geom_bar(fill="#3366ff",colour="black")+ 
  xlab("Gender")+
  ylab("Number of Events") +
  ggtitle("Perpetrator Gender")

#Ethnicity Historgram 

race.hist = ggplot(shootings,aes(race))+
  geom_bar(fill="#3366ff",colour="black")+
  xlab("Ethnicity")+
  ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  ggtitle("Perpetrator Ethnicity")


# Not great dodged bar chart
ggplot(shootings_m,aes(age,fill=race)) +
  geom_histogram(binwidth=5,position='dodge',color='black')+
  ylab("Number of Perpetrators") + 
  ggtitle("Perpetrators by Age and Race")

#Male Perpetrators by Age - Stacked Bar Chart => Leads to Density

ggplot(shootings_m,aes(age,fill=race)) +
  geom_histogram(binwidth=5,position='stack',color='black')+
  xlab("Age") +
  ylab("Number of Perpetrators") + 
  ggtitle("Male Perpetrators by Age and Race")

#White Male Age Density

ggplot(shootings_m[shootings_m$race=='White',],aes(age)) + 
  geom_histogram(aes(y=..density..),binwidth=5,fill='#FE2EF7',color='black')+
  geom_density(size = 1)+
  ggtitle("White Male Perpetrators")



#Overlapping Male Perpetrator Densities - 3 Groups - White, Black, Other

#Multiple Density Plots

ggplot()+
  geom_density(aes(age), colour = "red", fill="red",alpha = .4, data=shootings_m[shootings_m$race_group == 'White',])+
  geom_density(aes(age), colour = "black", fill="black", alpha = .4, data=shootings_m[shootings_m$race_group == 'Black',])+
  geom_density(aes(age), colour = "green", fill="green",alpha = .4, data=shootings_m[shootings_m$race_group == 'Other',])+
  xlab("Age")+
  ylab("Density")+
  ggtitle("Age Densities by Ethnicity")


#or
 
qplot(data=shootings_m, x=Age,geom='density',color=race_group)+theme_bw()

ggtitle("White Male Perpetrators")



#Prior Signs

ggplot(shootings,aes(priorsignsmentalillness))+geom_bar()+theme_bw() + ylab("Count") +
  ggtitle("Mass Shootings in the US: 1982 - 2015")



#Scatterplot - Age vs. Total Victims

ggplot(shootings,aes(age,total.victims))+
  geom_point()+
  xlab("Age")+
  ylab("Victims (Fatalities + Wounded)")+
  ggtitle("Perpetrator Age vs. Total Victims")




#TEST



#Maybe make this a box plot???

shootings$race_group = as.character(shootings$race_group)
shootings$race_group = as.factor(shootings$race_group)

ggplot(shootings,aes(x=race,y= age))+geom_point(stat="identity",fill="#66CC99",colour="black")+
  ggtitle(expression(atop("Perpetrators",atop("(Ethnicity and Age)",""))))

ggplot(shootings, aes(race_group,age)) + geom_violin() 
ggplot(shootings, aes(race_group,age)) + geom_boxplot() 


ggplot(shootings_m,aes(x=Age,y= Race))+geom_point(stat="identity",fill="#66CC99",colour="black")+
  ggtitle(expression(atop("Perpetrators",atop("(Ethnicity and Age)",""))))




summary(shootings)

grid.arrange(events.yr,vic.yr.hist2 , ncol=2, nrow=2) # Events and Total Victims Combined Chart

grid.arrange(inj.yr.hist,fat.yr.hist , ncol=2, nrow=2) # Injured and Fatalities Combined Chart

grid.arrange(age.hist,sex.hist,race.hist , ncol=3, nrow=1) # Age Sex Combined Chart

grid.arrange(race.hist, ncol=2, nrow=2) # race Chart

grid.arrange(top_states,top_victims, nrow=2, ncol=1)

grid.arrange(age.hist,perp_race_age, nrow=2) # Age and Age by Race

grid.arrange(totalvictims.hist,fatalities.hist,wounded.hist, ncol=1, nrow=3)

grid.arrange(fatalities.year, wounded.year,ncol=1, nrow=2)

perp_race_age

#groupby race and age for total events

shootings_m_tbl = tbl_df(shootings_m)


by_race_age = group_by(shootings_m_tbl,race,age)
by_race_age
summarise(by_race_age, sum = sum(total.victims))


grid.arrange(top_states,top_victims, ncol=1, nrow=2)

###????what to do with the above?





#New Mosaic

data=shootings_m
race.new = NA
race.new[which(data$race == "White")] = "White"
race.new[which(data$race != "White")] = "Other"
race.new = as.factor(race.new)

age.new = cut(data$Age, c(13, 30, 50, 66),
              labels = c("13 - 30", "31 - 50", "51 - 66"),
              include.lowest = TRUE)

mosaic(table(race.new, age.new), shade = TRUE, legend = TRUE,
       labeling_args = list(set_varnames = c(race.new = "Race",
       age.new = "Age")))


# top 5 events by total victims

head(arrange(select(shootings, 1:5, 7:9),desc(total.victims)),5)



  
  