#install.packages("data.table")
library(data.table)
library(dplyr)
library(googleVis)
library(ggplot2)
library(maps)
library(mapdata)
library(devtools)
library(zipcode)
library(RColorBrewer)
library(dplyr)
library(choroplethr)
library(choroplethrZip)
library(Hmisc)
library(corrplot)

data(df_pop_state)
setwd("/Users/satishjoshi/DataBootcamp/bootcamp004_project/Project1-ExploreVis/uwonderiwander")

source("projecthelper.R")



suppressPackageStartupMessages(library(googleVis))

fleet_data=read.csv("data/2015Dec_Census.txt",  stringsAsFactors=FALSE)
nrow(fleet_data) #1643373

fleet_data$MCS150_MILEAGE[is.na(fleet_data$MCS150_MILEAGE)] = 0
fleet_data$NBR_POWER_UNIT[is.na(fleet_data$NBR_POWER_UNIT)] = 0
fleet_data$DRIVER_TOTAL[is.na(fleet_data$DRIVER_TOTAL)] = 0

#Summary of mean, median, min, max, sd of miles, units, drivers
#p <- ggplot(fleet_data) 
#p + geom_boxplot(aes(x=PHY_COUNTRY, y=DRIVER_TOTAL, color='red'))
#p + geom_boxplot(aes(x=PHY_COUNTRY, y=NBR_POWER_UNIT, color='green'))
#p + geom_boxplot(aes(x=PHY_COUNTRY, y=MCS150_MILEAGE, color='yello'))

fleet_summary = merge(fleet_summary, by_country)
by_country = fleet_data %>% group_by(PHY_COUNTRY) %>% summarise(country_count = n())
fleet_summary = by_country
by_country = fleet_data %>% group_by(PHY_COUNTRY) %>% summarise(unit_total = sum(NBR_POWER_UNIT))
fleet_summary = merge(fleet_summary, by_country)
by_country = fleet_data %>% group_by(PHY_COUNTRY) %>% summarise(driver_total = sum(DRIVER_TOTAL))
fleet_summary = merge(fleet_summary, by_country)
by_country = fleet_data %>% group_by(PHY_COUNTRY) %>% summarise(mileage_total = sum(MCS150_MILEAGE))
fleet_summary = merge(fleet_summary, by_country)


fleets = clean_data(fleet_data)
nrow(fleets) #1394055 - 1442254
write.csv(fleets, file = "data/USOnlyFleets.csv", row.names = TRUE)

summary(fleets)
summary = c(type = 'DRIVER_TOTAL',summarise(fleets, sum = sum(DRIVER_TOTAL), min = min(DRIVER_TOTAL), max = max(DRIVER_TOTAL), mean = mean(DRIVER_TOTAL), median = median(DRIVER_TOTAL), sd = sd(DRIVER_TOTAL), IQR = IQR(DRIVER_TOTAL)))
summary2 = c('NBR_POWER_UNIT', summarise(fleets, sum(NBR_POWER_UNIT), min(NBR_POWER_UNIT), max(NBR_POWER_UNIT), mean(NBR_POWER_UNIT), median(NBR_POWER_UNIT), sd(NBR_POWER_UNIT), IQR(NBR_POWER_UNIT)))
summary3 = c('MCS150_MILEAGE', summarise(fleets, sum(MCS150_MILEAGE), min(MCS150_MILEAGE), max(MCS150_MILEAGE), mean(MCS150_MILEAGE), median(MCS150_MILEAGE), sd(MCS150_MILEAGE), IQR(MCS150_MILEAGE)))
summary4 = as.data.frame(cbind(summary, summary2, summary3))
str(summary4)
#categorise the unit and driver counts
numbers = c(0,1,2,6,11,51,101,501, 1001, 2001, 5001, 10001, 20001, 50001, 100001)
labels = c('0','1','2-5','6-10','11-50','51-100','101-500', '501-1000', '1001-2000', 
           '2K - 5K', '5K - 10K', '10K - 20K', '20K - 50K', ' < 100000','unknown')
category = data_frame(numbers, labels)
fleets = mutate(fleets, driver_category = cut(fleets$DRIVER_TOTAL, category$numbers,right=FALSE, labels=category$labels[1:14]))
fleets = mutate(fleets, unit_category = cut(fleets$NBR_POWER_UNIT, category$numbers,right=FALSE, labels=category$labels[1:14]))

fleets_outlier = subset(fleets, (NBR_POWER_UNIT > 200000) | (DRIVER_TOTAL > 100000))
fleets = subset(fleets, (NBR_POWER_UNIT < 200000) & (DRIVER_TOTAL < 100000))

fleets_tbl = tbl_df(fleets)
fleets_tbl = select(fleets_tbl, op_type = CARRIER_OPERATION, domicile_state = PHY_STATE, units_total = NBR_POWER_UNIT,
                      driver_total = DRIVER_TOTAL, driver_category, unit_category)
fleets_tbl = mutate(fleets_tbl, domicile_state = tolower(state.name[match(fleets_tbl$domicile_state, state.abb)]))
fleets_tbl = mutate(fleets_tbl, state_population = df_pop_state$value[match(fleets_tbl$domicile_state, df_pop_state$region)])

#create summary by domicile state and op_type
by_state = fleets_tbl %>% group_by(domicile_state, op_type) %>% summarise(fleets_count_phy_state = n())
by_state = merge(by_state,fleets_tbl %>% group_by(domicile_state, op_type) %>% summarise(driver_total_phy_state = sum(driver_total)))
by_state = merge(by_state, fleets_tbl %>% group_by(domicile_state, op_type) %>% summarise(unit_total_phy_state = sum(units_total)))
by_state = mutate(by_state, state_population = df_pop_state$value[match(by_state$domicile_state, df_pop_state$region)])

state_drivers_plot = ggplot(data=by_state, aes(x=reorder(domicile_state, driver_total_phy_state), y=driver_total_phy_state, fill = op_type)) +
   geom_bar(stat="identity") +
   xlab("State") + ylab("Drivers") +
   scale_fill_discrete(name = "Type of Operation", labels = c("Interstate", "Intrastate Hazmat", "Intrastate Non-Hazmat")) +
#   scale_fill_brewer(palette = 'Blues') +
   theme(axis.text.x = element_text(angle = 90)) +
   ggtitle("Number of Drivers by State") +
    scale_y_continuous(labels = comma)
state_drivers_plot

state_units_plot = ggplot(data=by_state, aes(x=reorder(domicile_state, unit_total_phy_state), y=unit_total_phy_state, fill = op_type)) +
  geom_bar(stat="identity") +
  xlab("State") + ylab("Units") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),name = "Type of Operation", labels = c("Interstate", "Intrastate Hazmat", "Intrastate Non-Hazmat")) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Number of Units by State")  +
  scale_y_continuous(labels = comma)
state_units_plot



#Get warning for The following regions were missing and are being set to NA: district of columbia
#at the same time this does not return anything - state.name[state.abb =='DC']


#Trcuker population by state and temporaty renaming for plot
by_state_only = by_state %>% group_by(domicile_state) %>% summarise(driver_total = sum(driver_total_phy_state ))
by_state_only = rename(by_state_only,  region = domicile_state,  value = driver_total)
STATE_DRIVERS = state_choropleth(by_state_only, title = "Trucker Population", legend = "Truckers", num_colors = 9, zoom = NULL)

STATE_DRIVERS

STATE_DRIVERS_TOP5 = state_choropleth(by_state_only, title = "Trucker Population by top 5 states", legend = "Trcukers", num_colors = 9, 
                                 zoom = c("california","texas", "georgia", "florida", "pennsylvania"))
STATE_DRIVERS_TOP5
#Warning message:
#  In self$bind() :
#  The following regions were missing and are being set to NA: district of columbia

#Unit population by state and temporaty renaming for plot
by_state_only = by_state %>% group_by(domicile_state) %>% summarise(unit_total = sum(unit_total_phy_state ))
by_state_only = rename(by_state_only,  region = domicile_state,  value = unit_total)
#by_state_only$ggplot_scale = scale_fill_brewer(palette="Spectral",  na.value='white')
#scale_fill_manual(name="Candidate", values=c("blue", "red"), drop=FALSE)
#by_state_only$value = cut2(by_state_only$value,  cuts=c(0,1000000,50000))

STATE_UNITS = state_choropleth(by_state_only, title = "Vehicle Population", legend = "Vehicles", num_colors = 1, 
                               zoom = NULL)

STATE_UNITS

STATE_UNITS_TOP5 = state_choropleth(by_state_only, title = "Vehicle Population by top 5 states", legend = "Trcukers", num_colors = 1, 
                                 zoom = c("california","texas", "georgia", "new york", "pennsylvania"))

STATE_UNITS_TOP5
#state population 

STATE_POPULATION = state_choropleth(df_pop_state, title = "Total Population", legend = "Population")
STATE_POPULATION

#Choropleth by Zip 

drivers_by_zip = fleets %>% group_by(PHY_ZIP) %>% summarise(driver_total_phy_zip = sum(DRIVER_TOTAL))
drivers_by_zip = rename(drivers_by_zip, region = PHY_ZIP, value = driver_total_phy_zip)

zip_state_map(drivers_by_zip,"pennsylvania", title="Pennsylvania Truck Driver Population") 
zip_state_map(drivers_by_zip,"california", title="California Truck Driver Population",  palette=4) 
zip_state_map(drivers_by_zip,"texas", title="Texas Truck Driver Population",  palette=3) 
zip_state_map(drivers_by_zip,"georgia", title="Georgia Truck Driver Population",  palette=5) 
zip_state_map(drivers_by_zip,"florida", title="Florida Truck Driver Population",  palette=6) 
zip_state_map(drivers_by_zip,"new york", title="New York Truck Driver Population",  palette=2 ) 

# To cluttered....need to do something different
# fleet_driver_plot = ggplot(data=by_state, aes(x=fleets_count_phy_state, y=driver_total_phy_state)) + 
#   geom_point(aes(color = domicile_state)) +
#   xlab("Number of Fleets") + ylab("Number of Drivers") 


fleet_driver_plot = ggplot(data=by_state, aes(x=fleets_count_phy_state, y=driver_total_phy_state)) + 
  geom_point(aes(color = op_type)) +
  xlab("Number of Fleets") + ylab("Number of Drivers") +
  ggtitle("Number of Fleets to Drivers relationship") +
  scale_color_manual(name = "Type of Operation", labels = c("Interstate", "Intrastate Hazmat", "Intrastate Non-Hazmat"), 
                     values = c("dark blue", "dark red", "dark green")) +
  scale_y_continuous(labels = comma) +
  facet_grid(~op_type)
fleet_driver_plot

fleet_unit_plot = ggplot(data=by_state, aes(x=fleets_count_phy_state, y=unit_total_phy_state)) + 
  geom_point(aes(color = op_type)) +
  xlab("Number of Fleets") + ylab("Number of Vehicles") +
  ggtitle("Number of Fleets to Vehicles relationship") +
  scale_color_manual(name = "Type of Operation", labels = c("Interstate", "Intrastate Hazmat", "Intrastate Non-Hazmat"), 
                    values = c("blue", "red", "green")) +
  scale_y_continuous(labels = comma) +
  facet_grid(~op_type)
fleet_unit_plot

#fleets_outlier = subset(fleets, (NBR_POWER_UNIT > 200000) | (DRIVER_TOTAL > 100000))
#fleets_subset = subset(fleets, (NBR_POWER_UNIT < 200000) & (DRIVER_TOTAL < 100000))
p = ggplot(fleets_subset, aes(NBR_POWER_UNIT, DRIVER_TOTAL ))
p + geom_point(aes(color=PHY_STATE))
p


# min.mean.sd.max <- function(x) {
#   r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
#   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
#   r
# }

# p1 = ggplot(aes(y = NBR_POWER_UNIT, x = factor(PHY_STATE)), data = fleets)
# p1 = p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
#   geom_jitter(position=position_jitter(width=.2), size=3) + 
#   ggtitle("Boxplot con media, 95%CI, valore min. e max.") + 
#   xlab("Gruppi") + ylab("Valori")
# p1
# 
# 
# xport_data=read.csv("data/transportation_value.csv",  stringsAsFactors=FALSE)
# str(xport_data)

