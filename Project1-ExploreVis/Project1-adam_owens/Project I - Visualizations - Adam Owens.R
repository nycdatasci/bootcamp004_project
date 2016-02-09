
library(dplyr)
library(ggplot2)
library(reshape)
library(ggthemes)
library(googleVis)
library(gcookbook)
library(RColorBrewer)
library(Quandl)
library(plyr)

##-----------------------------------------------------
#Load and prepare line chart of venture capital by year
##-----------------------------------------------------

startup_data = read.csv('Desktop/Data Science/Crunchbase/funding rounds.csv',header = TRUE) %>%
  filter(., country_code == "USA", funded_year > 2004 & funded_year <= 2012)

rounds = as.data.frame(group_by(startup_data, round_code, funded_year) %>%
  summarise(., count = n(), sum = sum(raised_amount)) %>%
  arrange(.,desc(sum)))

colnames(rounds) = c("Funding_Round", "Funding_Year", "Transactions", "Capital_Invested")
head(rounds_all)
rounds_all = mutate(rounds, Capital = round(Capital_Invested/1000000,2)) %>%
  filter(., Funding_Round %in% c("a","b","c","d","e","f","g","debt_round","private equity")) %>%
  arrange(., Funding_Year, Funding_Round, Capital)

View(startup_data)
##too much missing data in "unattributed" and "angel" -- Filtered out 

#----------------
#Graph by Capital
#----------------

ggplot(rounds_all, aes(x=Funding_Year, y=Capital, fill=Funding_Round, fontSize = 15)) +
  geom_area(colour="grey", size = .2, alpha=.8) +
  scale_fill_brewer(palette = "Greens",breaks=rev(levels(rounds_all$Funding_Round)))

#---------------------
#Graph by Transactions
#---------------------
ggplot(rounds_all, aes(x=Funding_Year, y=Transactions, fill=Funding_Round)) +
  geom_area(colour="grey", size = .3, alpha=.8) +
  scale_fill_brewer(palette = "Greens",breaks=rev(levels(rounds_all$Funding_Round)))

#--------------------------
#Graph startups by industry
#--------------------------

####  Yes, I know this is WILDLY INEFFICIENT!!! ......but, I was tired and it was late, so lots of copy&paste seemed like a good solution

startup_ind = as.data.frame(Quandl("NVCA/VENTURE_3_18") %>%
  filter(., Date > "2004/12/31" & Date < "2013/12/31")) %>%
  rename(., c("Business Products and Services"="Business_Products_and_Services","Computers and Peripherals"="Computers_and_Peripherals","Consumer Products and Services"="Consumer_Products_and_Services",
              "Financial Services"="Financial_Services","Healthcare Services"="Healthcare_Services","IT Services"="IT_Services","Media and Entertainment"="Media_and_Entertainment","Medical Devices and Equipment"="Medical_Devices_and_Equipment",
              "Networking and Equipment"="Networking_and_Equipment","Electronics/Instrumentation"="Electronics_Instrumentation","Industrial/Energy"="Industrial_Energy","Retailing/Distribution"="Retailing_Distribution"))

st_bio = select(startup_ind, Date, Biotechnology) %>%
  mutate(., Industry= "Biotech") %>%
  rename(., c("Biotechnology" = "Amount"))

st_cp = select(startup_ind, Date, Computers_and_Peripherals) %>%
  mutate(., Industry= "Computers_and_Peripherals") %>%
  rename(., c("Computers_and_Peripherals" = "Amount"))

st_cps = select(startup_ind, Date, Consumer_Products_and_Services) %>%
  mutate(., Industry= "Consumer_Products_and_Services") %>%
  rename(., c("Consumer_Products_and_Services" = "Amount"))

st_elect = select(startup_ind, Date, Electronics_Instrumentation) %>%
  mutate(., Industry= "Electronics_Instrumentation") %>%
  rename(., c("Electronics_Instrumentation" = "Amount"))

st_fin = select(startup_ind, Date, Financial_Services) %>%
  mutate(., Industry= "Financial_Services") %>%
  rename(., c("Financial_Services" = "Amount"))

st_health = select(startup_ind, Date, Healthcare_Services) %>%
  mutate(., Industry= "Healthcare_Services") %>%
  rename(., c("Healthcare_Services" = "Amount"))

st_ind = select(startup_ind, Date, Industrial_Energy) %>%
  mutate(., Industry= "Industrial_Energy") %>%
  rename(., c("Industrial_Energy" = "Amount"))

st_it = select(startup_ind, Date, IT_Services) %>%
  mutate(., Industry= "IT_Services") %>%
  rename(., c("IT_Services" = "Amount"))

st_me = select(startup_ind, Date, Media_and_Entertainment) %>%
  mutate(., Industry= "Media_and_Entertainment") %>%
  rename(., c("Media_and_Entertainment" = "Amount"))

st_med = select(startup_ind, Date, Medical_Devices_and_Equipment) %>%
  mutate(., Industry= "Medical_Devices_and_Equipment") %>%
  rename(., c("Medical_Devices_and_Equipment" = "Amount"))

st_net = select(startup_ind, Date, Networking_and_Equipment) %>%
  mutate(., Industry= "Networking_and_Equipment") %>%
  rename(., c("Networking_and_Equipment" = "Amount"))

st_other = select(startup_ind, Date, Other) %>%
  mutate(., Industry= "Other") %>%
  rename(., c("Other" = "Amount"))

st_retail = select(startup_ind, Date, Retailing_Distribution) %>%
  mutate(., Industry= "Retailing_Distribution") %>%
  rename(., c("Retailing_Distribution" = "Amount"))

st_semi = select(startup_ind, Date, Semiconductors) %>%
  mutate(., Industry= "Semiconductors") %>%
  rename(., c("Semiconductors" = "Amount"))

st_soft = select(startup_ind, Date, Software) %>%
  mutate(., Industry= "Software") %>%
  rename(., c("Software" = "Amount"))

st_tele = select(startup_ind, Date, Telecommunications) %>%
  mutate(., Industry= "Telecommunications") %>%
  rename(., c("Telecommunications" = "Amount"))

st_industry = rbind(st_bio, st_cp, st_cps,st_elect, st_fin, st_health, st_ind, st_it, st_me, st_med, st_net, st_other, st_retail,st_semi, st_soft,st_tele)

#transform for % and limite to 8 industries because of graph limitations

st_indust_transform = ddply(st_industry, "Date", transform,Percent = Amount / sum(Amount) * 100)

top_8 = group_by(st_indust_transform, Industry) %>%
  summarise(., sum = sum(Amount)) %>%
  arrange(., desc(sum)) %>%
  top_n(., 9) %>%
  select(., Industry)

st_industry_top = as.data.frame(filter(st_industry, Industry %in% c("Software","Biotech","Industrial_Energy","Media_and_Entertainment",
                                                              "Medical_Devices_and_Equipment","IT_Services","Telecommunications","Consumer_Products_and_Services","Financial_Services")))

st_indust_top_transform =  ddply(st_industry_top, "Date", transform,Percent = Amount / sum(Amount) * 100) %>%
  arrange(., Industry, Percent)

st_indust_top_transform$Industry = as.factor(st_indust_top_transform$Industry)

ggplot(st_indust_top_transform, aes(x=Date, y=Percent, fill=Industry)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(st_indust_top_transform$Industry))) 

#--------------------
#Google Vis Data Prep
#--------------------
raise_per_comp = group_by(recent_data, funding_rounds) %>%
  summarise(., Rounds = n(), sum = round(sum(raised_amount)/1000000,2)) %>%
  arrange(.,desc(sum)) 

maps_data = recent_data[ ! duplicated(recent_data[c("funding_rounds")]), ] %>%
  select(., funding_rounds,zip_code,city,state_code,latitude,longitude) 

maps_df = na.omit(left_join(maps_data, raise_per_comp , by = "funding_rounds"))

#---------------------------- 
# Capital raised by state
#----------------------------

maps_state = group_by(maps_df, state_code) %>%
  summarise(., Rounds = n(), sum = sum(sum)) %>%
  arrange(., desc(sum))

Geo_city = gvisGeoChart(maps_state, "state_code", colorvar = "sum", 
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=1000, height=600))
plot(Geo_city)

#--------------------------------------------
#Capital raised by state from Angel, Seed, a
#--------------------------------------------

raise_per_e = group_by(recent_data, funding_rounds) %>%
  filter(., round_code %in% c("a","angel", "seed")) %>%
  summarise(., Rounds = n(), sum = round(sum(raised_amount)/1000000,2)) %>%
  arrange(.,desc(sum))

maps_df_e = na.omit(left_join(maps_data, raise_per_e , by = "funding_rounds"))

maps_state_e = group_by(maps_df_e, state_code) %>%
  summarise(., Rounds = n(), sum = sum(sum)) %>%
  arrange(., desc(sum))

Geo_city_e = gvisGeoChart(maps_state_e, "state_code", colorvar = "sum", 
                        options=list(region="US", 
                                     displayMode="regions", 
                                     resolution="provinces",
                                     width=800, height=600))

plot(Geo_city_e)


#----------
# VC funds
#----------

vc_a = as.data.frame(read.csv('Desktop/Data Science/Crunchbase/venture fund locations.csv',header = TRUE) %>%
  select(., -serialid) %>%
  mutate(., uniqueid = paste(name, city)))

vc_b  = vc_a[ ! duplicated(vc_a[c("uniqueid")]), ]
  
#Number of unique offices per firm
vc_offices = group_by(vc_b, name) %>%
  summarise(., n_offices = n())

vc_local = left_join(vc_b, vc_offices, by = "name")

vc_funds = na.omit(read.csv('Desktop/Data Science/Crunchbase/VC fund size.csv',header = TRUE) %>%
  mutate(., uniqueid = paste(name,funded_year,funded_month,funded_day)) %>%
  filter(., funded_year > 2004 & funded_year <= 2014)) %>%
  filter(., raised_currency_code == "USD") %>%
  select(., -serialid) %>%
  inner_join(., vc_local, by= "name" ) %>%
  mutate(., id = paste(uniqueid.x, uniqueid.y)) %>%
  rename(., c("raised_amount" = "Capital_Invested", "name" = "VC_Firm", "raised_currency_code" = "Currency", "funded_year" = "Year", "state_code" = "State", "uniqueid.y" = "VC_Office")) 

vc_df = vc_funds[ ! duplicated(vc_funds[c("id")]), ] %>%
  mutate(., capital_per_office = round(Capital_Invested/n_offices,2))

View(vc_df)
#-------------------
# VC Funds Annually
#-------------------

vc_funds_annual = group_by(vc_df, Year, State) %>%
  summarise(., count = n(), sum = round(sum(capital_per_office)/1000,2)) %>%
  arrange(.,desc(sum))

colnames(vc_funds_annual) = c("Year", "State", "Offices", "Capital_Invested","Log_Capital")

#---------------
# VC Tree Graph
#---------------

vc_fund_names = group_by(vc_df, State, VC_Office) %>%
  summarise(., sum = sum(capital_per_office/1000)) %>%
  mutate(., log_sum = log(sum)) %>%
  arrange(., desc(sum))
vc_fund_CA = filter(vc_fund_names, State == "CA")
View(vc_fund_CA)
vc_fund_states = group_by(vc_fund_names[16:613, ], State) %>%
  summarise(., count = n(), sum = sum(sum), log_sum = sum(log_sum)) 

vc_fund_states2 = cbind(root = "USA", vc_fund_states) %>%
  rename(., c("State" = "ID_var"))

vc_fund_names2 = rbind(c(NA,"USA",sum(vc_fund_names$count), sum(vc_fund_names$sum),sum(vc_fund_names$log_sum)),vc_fund_names[16:613, ]) %>%
  rename(., c("State" = "root", "VC_Office"="ID_var"))

vc_fund_names3 = rbind(vc_fund_states2, vc_fund_names2)

Tree1 <- gvisTreeMap(vc_fund_names3, idvar = "ID_var",
                     parentvar = "root", 
                     sizevar = "sum", 
                     colorvar = "log_sum",
                     width = 1000,
                     options = list(
                       minColor='white',
                       maxColor='green',
                       headerHeight=20,
                       fontColor='black',
                       showScale=TRUE))
plot(Tree1)

#-------------------
# VC Funds US Graph
#-------------------

vc_funds_geo = group_by(vc_fund_names, State) %>%
  summarise(., count = n(States))

Geo_vc = gvisGeoChart(vc_fund_states, "State", colorvar = "count", 
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=1000, height=600))
plot(Geo_vc)

#-----------------------
# Engineers in US
#-----------------------

class(engineers_data$OCC_GROUP)

engineers_data = read.csv('Desktop/Data Science/engineer_labor_state.csv',header = TRUE) %>%
  filter(., grepl('major', OCC_GROUP)) %>%
  filter(., grepl('Computer|Engineer', OCC_TITLE))

View(engineers_data)
#convert factor to numeric through "seq_along"
engineers_data$A_MEAN=seq_along(levels(engineers_data$A_MEAN))[engineers_data$A_MEAN]

engineers_sal = mutate(engineers_data, sal_mean = A_MEAN*10)

engineers_df = group_by(engineers_sal, STATE) %>%
  summarise(., mean = mean(sal_mean)) %>%
  arrange(., desc(mean))

Geo_state_eng = gvisGeoChart(engineers_df, "STATE", colorvar = "mean", 
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       colors="['white','darkblue']",
                                       width=600, height=925))
plot(Geo_city_eng)

bar_state_eng = gvisBarChart(engineers_df,
                             options=list(legend ="none",
                                          title = "Average Annual Wage",
                                          fontSize = 8,
                                          width = 500, height = 925))

gvis_eng = gvisMerge(Geo_state_eng, bar_state_eng, horizontal = TRUE)
plot(gvis_eng)