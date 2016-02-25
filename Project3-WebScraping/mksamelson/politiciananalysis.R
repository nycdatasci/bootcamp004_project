library(ggplot2)
library(dplyr)
library(maps)
library(USAboundaries)
library(sp)
library(DT)
library(RColorBrewer)

setwd('c:/Users/Matt/Dropbox/DataScienceBootcamp/projects/WebScrape')

politicians = read.csv("politiciansummary.csv", header=TRUE)

#Structure of the data

str(politicians)


#Convert dataframe column names to lowercase for easier processing

colnames(politicians) = tolower(colnames(politicians))

#Convert numeric variables scraped in character format and processed
#as factors to numeric

politicians$raised = as.numeric(gsub('[$,]','',politicians$raised))
politicians$spent = as.numeric(gsub('[$,]','',politicians$spent))
politicians$cash.on.hand = as.numeric(gsub('[$,]','',politicians$cash.on.hand))
politicians$debts = as.numeric(gsub('[$,]','',politicians$debts))

#Separate Congress and Senate

politicians_h = filter(politicians,chamber == "H")
politicians_s = filter(politicians,chamber == "S")

# get a list of states

states = unique(politicians$state)
rows_to_drop = c("American Samoa","Guam","Virgin Islands","Puerto Rico","District of Columbia")
states = subset(states,!(states %in% rows_to_drop))

#Remove Non-Voting Representatives from Guam Puerto Rico American Samoa and the Virgin Islands

rows_to_drop = c("American Samoa","Guam","Virgin Islands","Puerto Rico","District of Columbia")
politicians_s = subset(politicians_s, !(state %in% rows_to_drop))


#Legislator Numbers

#Senate

legislators_s = group_by(politicians_s,party) %>%
  summarise(.,count = n())

legislators_s

#Congress

legislators_h = group_by(politicians_h,party) %>%
  summarise(.,count = n())

legislators_h

#Cash on Hand by Party in the Senate and House

parties.cash.s = group_by(politicians_s,party) %>%
  summarise(.,cash = sum(cash.on.hand))

parties.cash.s

# ggplot(parties.cash.s, aes(party,cash))+ 
#   geom_bar(stat="identity")+
#   theme_bw()+
#   scale_y_continuous(labels = dollar)
  

parties.cash.h = group_by(politicians_h,party) %>%
  summarise(.,cash = sum(cash.on.hand))

parties.cash.h

#Cash Raised by Party in the Senate and House

group_by(politicians_s,party) %>%
  summarise(.,cash_raised = sum(raised))

group_by(politicians_h,party) %>%
  summarise(.,cash_raised = sum(raised))

#10 Top Cash Flush Republican Senators

cash.SR = group_by(politicians_s,party,member) %>%
  summarise(.,state,cash_avail = sum(cash.on.hand)) %>%
  filter(.,party=="R") %>%
  arrange(.,desc(cash_avail))

cash.SR = head(cash.R,10)

cash.SR

#10 Top Cash Flush Democratic Senators

cash.SD = group_by(politicians_s,party,member) %>%
  summarise(.,state,cash_avail = sum(cash.on.hand)) %>%
  filter(.,party=="D") %>%
  arrange(.,desc(cash_avail))
  
cash.SD = head(cash.D,10)

cash.SD

#10 Top Cash Flush Republican Congressmen

cash.RH = group_by(politicians_h,party,member) %>%
  summarise(.,state,cash_avail = sum(cash.on.hand)) %>%
  filter(.,party=="R") %>%
  arrange(.,desc(cash_avail))

cash.RH = head(cash.RH,10)

cash.RH

#10 Top Cash Flush Democratic Congressmen

cash.DH = group_by(politicians_h,party,member) %>%
  summarise(.,state,cash_avail = sum(cash.on.hand)) %>%
  filter(.,party=="D") %>%
  arrange(.,desc(cash_avail))

cash.DH = head(cash.DH,10)

cash.DH

#plot party in power by state - Senate

politicians_s_power = select(politicians_s,party,state)
powertable = table(politicians_s_power)
powertable = data.frame(powertable)
powertable = filter(powertable, Freq!=0)
powertable$grouping=NA
powertable = filter(powertable,((Freq==1 & party=="R")|(Freq==2)))


for (i in 1:nrow(powertable)){
  if (powertable$party[i]=="R" & powertable$Freq[i]==2){
    powertable$grouping[i]= "R"
  }
  else if (powertable$party[i]=="D" & powertable$Freq[i]==2){
    powertable$grouping[i]= "D"
  }else{
    powertable$grouping[i]="S"
  }
}


data=data.frame(grouping = powertable$grouping, state = tolower(powertable$state))
us = map_data("state")
  
aa <- ggplot(data=data,aes(fill=grouping))+
  geom_map(aes(map_id=state),map=us, color="black")+
  expand_limits(x = us$long, y = us$lat)+
  scale_fill_manual(values=c("blue", "red", "grey"),
                    name="Power Split",
                    labels=c("Democrats", "Republicans", "Split"))+
  coord_map("albers", 39, 42)+
  theme(legend.position = "right",
        panel.background= element_rect(fill = "white"),
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  ggtitle("US Senate: Republican/Democratic Power Balance")

aa


#plot party in power by state - House

politicians_h_power = select(politicians_h,party,state)
powertable_h = table(politicians_h_power)
powertable_h = data.frame(powertable_h)
powertable_h = filter(powertable_h, Freq!=0)
powertable_h$grouping=NA
powertable_h = filter(powertable_h,((party !="I")))

for (i in 1:nrow(powertable_h)){
  if (powertable_h$party[i]=="R"){
    powertable_h$grouping[i]= powertable_h$Freq[i]
  }
  if (powertable_h$party[i]=="D"){
    powertable_h$grouping[i]= -1*powertable_h$Freq[i]
  }
}

powertable_h = select(powertable_h, state, grouping)
powertable_h = group_by(powertable_h,state) %>%
     summarise(., Freq_h = sum(grouping))

data=data.frame(grouping = powertable_h$Freq_h, state = tolower(powertable_h$state))
us = map_data("state")

bb <- ggplot()+
  geom_map(data=us, map=us, 
           aes(x=long, y=lat, map_id=region),
           color="#7f7f7f", size=0.15, fill="white")+
  geom_map(data=data, map=us,aes(fill=grouping, map_id=state),color="black")+
  scale_fill_gradient2(name="Balance",midpoint=0,high = "#EF8A62", mid ="#F7F7F7",low = "#67A9CF",guide="colourbar")+
  coord_map("albers", 39, 42)+
  theme(legend.position = "right",
        panel.background= element_rect(fill = "white"),
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  ggtitle("US Congress: Republican/Democratic Power Balance")

bb


#Senate - Republican cash on hand by state

cash.s.r = filter(politicians_s,party=="R") %>%
  select(.,state,cash.on.hand) %>%
  group_by(.,state)%>%
  summarise(., cash = sum(cash.on.hand))

data=data.frame(cash_on_hand = cash.s.r$cash, state = tolower(cash.s.r$state))
us = map_data("state")

cc <- ggplot()+
  geom_map(data=us, map=us, 
           aes(x=long, y=lat, map_id=region),
           color="#7f7f7f", size=0.15, fill="white")+
  geom_map(data=data, map=us,
           aes(fill=cash_on_hand, map_id=state),color="black")+
  scale_fill_distiller(name="Cash on Hand",palette=18,direction=1)+
  coord_map("albers", 39, 42)+
  theme(legend.position = "right",
        panel.background= element_rect(fill = "white"),
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  ggtitle("US Senate: Republican Cash on Hand")

cc

#Senate - Democrat cash on hand by state

cash.s.d = filter(politicians_s,party=="D") %>%
  select(.,state,cash.on.hand) %>%
  group_by(.,state)%>%
  summarise(., cash = sum(cash.on.hand))

data=data.frame(cash_on_hand = cash.s.d$cash, state = tolower(cash.s.d$state))
us = map_data("state")

dd <- ggplot()+
  geom_map(data=us, map=us, 
           aes(x=long, y=lat, map_id=region),
           color="#7f7f7f", size=0.15, fill="white")+
  geom_map(data=data, map=us,
           aes(fill=cash_on_hand, map_id=state),color="black")+
  scale_fill_distiller(name="Cash on Hand",palette=9,label=dollar,direction=1)+
  coord_map("albers", 39, 42)+
  theme(legend.position = "right",
        panel.background= element_rect(fill = "white"),
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  ggtitle("US Senate: Democratic Cash on Hand")

dd

#House of Representatives

#Congress - Republican cash on hand by state

cash.h.r = filter(politicians_h,party=="R") %>%
  select(.,state,cash.on.hand) %>%
  group_by(.,state)%>%
  summarise(., cash = sum(cash.on.hand))

data=data.frame(cash_on_hand = cash.h.r$cash, state = tolower(cash.h.r$state))
us = map_data("state")

ee <- ggplot()+
  geom_map(data=us, map=us, 
           aes(x=long, y=lat, map_id=region),
           color="#7f7f7f", size=0.15, fill="white")+
  geom_map(data=data, map=us,
           aes(fill=cash_on_hand, map_id=state),color="black")+
  scale_fill_distiller(name="Cash on Hand",palette=18,label=dollar,direction=1)+
  coord_map("albers", 39, 42)+
  theme(legend.position = "right",
        panel.background= element_rect(fill = "white"),
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  ggtitle("US Congress: Republican Cash on Hand")

ee




#Congress - Democrat cash on hand by state


cash.h.d = filter(politicians_h,party=="D") %>%
  select(.,state,cash.on.hand) %>%
  group_by(.,state)%>%
  summarise(., cash = sum(cash.on.hand))

data=data.frame(cash_on_hand = cash.h.d$cash, state = tolower(cash.h.d$state))
us <- map_data("state")
  
  hh <- ggplot()+
        geom_map(data=us, map=us, 
                      aes(x=long, y=lat, map_id=region),
                      color="#7f7f7f", size=0.15, fill="white")+
        geom_map(data=data, map=us,
                      aes(fill=cash_on_hand, map_id=state),color="black")+
        scale_fill_distiller(name="Cash on Hand",palette=9,label=dollar,direction=1)+
        coord_map("albers", 39, 42)+
        theme(legend.position = "right",
              panel.background= element_rect(fill = "white"),
              axis.ticks = element_blank(), 
              axis.title = element_blank(), 
              axis.text =  element_blank())+
        ggtitle("US Congress: Democratic Cash on Hand")

  hh



