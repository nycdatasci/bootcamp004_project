# Loading data from usual data sub directory
# Have got charts sub directory to save the plots


#Not all the libraries may be needed but at some point or other they were used
#need to cleanup loading of libraries only to ones really required
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
library(mice)

data(state)

setwd("/Users/satishjoshi/DataBootcamp/ShinyProject")

crash_data=read.csv("data/good_crash_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
census_data=read.csv("data/good_census_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
nrow(crash_data) #226287 of 226289 original
nrow(census_data) #1643370 of 1643374 original

inspection_data=read.csv("data/good_inspection_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
nrow(inspection_data) # 5739024 instead of 6071503 of 6071612 original

unique(crash_data$Report_State)
unique(crash_data$Report_Date)
unique(crash_data$Fatalities)
unique(crash_data$Injuries)
unique(crash_data$Hazmat_released)

crash_data$Hazmat_released[crash_data$Hazmat_released == ""] = "Unknown"
crash_data$Hazmat_released[crash_data$Hazmat_released == "Y"] = "Yes"
crash_data$Hazmat_released[crash_data$Hazmat_released == "N"] = "No"

unique(crash_data$Trafficway_Desc)
crash_data$Trafficway_Desc[crash_data$Trafficway_Desc == ""] = "Unknown"

unique(crash_data$Tow_Away)
crash_data$Tow_Away[crash_data$Tow_Away == "Y"] = "Yes"
crash_data$Tow_Away[crash_data$Tow_Away == "N"] = "No"

unique(crash_data$Road_surface_Condition_Desc)
crash_data$Road_surface_Condition_Desc[crash_data$Road_surface_Condition_Desc == ""] = "Unknown"

unique(crash_data$Weather_Condition_Desc)
crash_data$Weather_Condition_Desc[crash_data$Weather_Condition_Desc == ""] = "Unknown"

unique(crash_data$Light_Condition_Desc)
crash_data$Light_Condition_Desc[crash_data$Light_Condition_Desc == ""] = "Unknown"

unique(crash_data$Severity_Weight)
unique(crash_data$Time_weight)
unique(crash_data$citation_issue_desc)
crash_data$citation_issue_desc[crash_data$citation_issue_desc == ""] = "Unknown"

length(unique(crash_data$Vehicle_ID_Number))/nrow(crash_data)
by_vin = crash_data %>% group_by(Vehicle_ID_Number) %>% summarise(multiple_vin_count= n())            
by_vin[by_vin$multiple_vin_count > 1,]



inspection_data$UNIT_LICENSE[is.na(inspection_data$UNIT_LICENSE)] = "Unknown"


crash_data$make_code[(nchar(crash_data$Vehicle_ID_Number) > 3)] = substring(crash_data[(nchar(crash_data$Vehicle_ID_Number) > 3), "Vehicle_ID_Number"], 1,3)
by_make = crash_data %>% group_by(make_code) %>% summarise(crash_count_by_make = n()) %>% arrange(desc(crash_count_by_make))

#http://vpic.nhtsa.dot.gov/mid/
#https://github.com/fastfedora/google-docs/blob/master/scripts/ImportJSON/Code.gsw

crash_data = mutate(crash_data, Report_State = state.name[match(crash_data$Report_State, state.abb)])
census_data = mutate(census_data, PHY_STATE = state.name[match(census_data$PHY_STATE, state.abb)])
census_data = mutate(census_data, MAILING_STATE = state.name[match(census_data$MAILING_STATE, state.abb)])

bad_crash_state_value = crash_data[is.na(crash_data$Report_State),]
crash_data = crash_data[!(is.na(crash_data$Report_State)),]

write.csv(bad_crash_state_value, "data/bad_crash_states.csv")

crash_data[is.na(crash_data$Report_Date),]
mean(as.Date(crash_data$Report_Date, "%d-%b-%y"))
min(as.Date(crash_data$Report_Date, "%d-%b-%y"))
max(as.Date(crash_data$Report_Date, "%d-%b-%y"))

crash_data = mutate(crash_data, Report_Date = as.Date(crash_data$Report_Date, "%d-%b-%y"))

crash_data = mutate(crash_data, Report_Year = as.numeric(format(crash_data$Report_Date,'%Y')))
crash_data = mutate(crash_data, Report_Month = as.numeric(format(crash_data$Report_Date,'%m')))


MakeCode_Table = MakeCode_Table[,-1]
MakeCode_Table[nrow(MakeCode_Table)+1] = c("UUU", "Invalid", "Invalid", "Invalid", "Invalid", "Invalid", "Invalid")

crash_data[(nchar(crash_data$Vehicle_ID_Number) <= 3), "Vehicle_ID_Number"] = "UUU"

substring(crash_data[, "Vehicle_ID_Number"], 1,3)

crashes_by_make =  crash_data %>% 
    group_by (make_code = substring(crash_data[, "Vehicle_ID_Number"], 1,3), 
            Report_Year) %>% 
      summarise(Total_Crashes = n())  %>% 
        arrange(make_code)  

crashes_by_make = mutate(crashes_by_make, CommonName = "NotKnown", Make = "NotKnown", ManufacturerName = "NotKnown", 
                      ParentCompanyName = "NotKnown", URL = "NotKnown", VehicleType = "NotKnown")

for (i in 1:nrow(crashes_by_make)) {
  xx = MakeCode_Table[crashes_by_make$make_code[i] == MakeCode_Table$Code, ]
  #print(length(xx[[2]]))
  #print(xx[[2]])
  if (length(xx[[2]]) > 0) crashes_by_make$CommonName[i] = xx[[2]]
  if (length(xx[[3]]) > 0) crashes_by_make$Make[i] = xx[[3]]
  if (length(xx[[4]]) > 0) crashes_by_make$ManufacturerName[i] = xx[[4]]
  if (length(xx[[5]]) > 0) crashes_by_make$ParentCompanyName[i] = xx[[5]]
  if (length(xx[[6]]) > 0) crashes_by_make$URL[i] = xx[[6]]
  if (length(xx[[7]]) > 0) crashes_by_make$VehicleType[i] = xx[[7]]
  #  print(c(xx[-1]))
  #  print(c("Count", i))
  #   There were 36 warnings (use warnings() to see them)
  #   > warnings()
  #   Warning messages:
  #     1: In insp_by_make$CommonName[i] = xx[[2]] :
  #     number of items to replace is not a multiple of replacement length
  #   
}

crashes_by_make$CommonName[crashes_by_make$CommonName == ""] = "Unknown"
crashes_by_make$ParentCompanyName[crashes_by_make$ParentCompanyName == ""] = "Unknown"
crashes_by_make$URL[crashes_by_make$URL == ""] = "Unknown"

unique(crashes_by_make$Make)
unique(crashes_by_make$CommonName)
unique(crashes_by_make$ManufacturerName)
unique(crashes_by_make$ParentCompanyName)
unique(crashes_by_make$URL)
unique(crashes_by_make$VehicleType)

write.csv(crashes_by_make, "data/crashes_by_make.csv")



inspection_data = mutate(inspection_data, Inspection_Date = as.Date(inspection_data$INSP_DATE, "%d-%b-%y"))

#Dont ask why the next two mutates did NOT work on INSP_DATE but on INspection_date
#Error: invalid 'trim' argument

inspection_data = mutate(inspection_data, Inspection_Year = as.numeric(format(inspection_data$Inspection_Date,'%Y')))
inspection_data = mutate(inspection_data, Inspection_Month = as.numeric(format(inspection_data$Inspection_Date,'%m')))

inspection_data[(nchar(inspection_data$VIN) <= 3), "VIN"] = "UUU"

substring(inspection_data[, "VIN"], 1,3)
inspection_data[is.na(inspection_data$INSP_DATE), ]
as.numeric(format(inspection_data$INSP_DATE,'%Y'))

insp_by_make =  inspection_data %>% 
  group_by (make_code = substring(inspection_data[, "VIN"], 1,3), 
            Inspection_Year) %>% 
  summarise(Total_Insps = n())  %>% 
  arrange(desc(Total_Insps))  


insp_by_make = mutate(insp_by_make, CommonName = "NotKnown", Make = "NotKnown", ManufacturerName = "NotKnown", 
                      ParentCompanyName = "NotKnown", URL = "NotKnown", VehicleType = "NotKnown")

nrow(insp_by_make)
for (i in 1:nrow(insp_by_make)) {
  xx = MakeCode_Table[insp_by_make$make_code[i] == MakeCode_Table$Code, ]
  #print(length(xx[[2]]))
  #print(xx[[2]])
  if (length(xx[[2]]) > 0) insp_by_make$CommonName[i] = xx[[2]]
  if (length(xx[[3]]) > 0) insp_by_make$Make[i] = xx[[3]]
  if (length(xx[[4]]) > 0) insp_by_make$ManufacturerName[i] = xx[[4]]
  if (length(xx[[5]]) > 0) insp_by_make$ParentCompanyName[i] = xx[[5]]
  if (length(xx[[6]]) > 0) insp_by_make$URL[i] = xx[[6]]
  if (length(xx[[7]]) > 0) insp_by_make$VehicleType[i] = xx[[7]]
  #  print(c(xx[-1]))
#  print(c("Count", i))
#   There were 36 warnings (use warnings() to see them)
#   > warnings()
#   Warning messages:
#     1: In insp_by_make$CommonName[i] = xx[[2]] :
#     number of items to replace is not a multiple of replacement length
#   
}

insp_by_make$CommonName[insp_by_make$CommonName == ""] = "Unknown"
insp_by_make$ParentCompanyName[insp_by_make$ParentCompanyName == ""] = "Unknown"
insp_by_make$URL[insp_by_make$URL == ""] = "Unknown"

unique(insp_by_make$Make)
unique(insp_by_make$CommonName)
unique(insp_by_make$ManufacturerName)
unique(insp_by_make$ParentCompanyName)
unique(insp_by_make$URL)
unique(insp_by_make$VehicleType)

write.csv(insp_by_make, "data/inspections_by_make.csv")


