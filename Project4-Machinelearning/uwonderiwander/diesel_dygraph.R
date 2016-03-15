library(devtools)
library(zipcode)
library(RColorBrewer)
library(dplyr)
library(choroplethr)
library(choroplethrZip)
library(Hmisc)
library(corrplot)
library(xts)
#library(xtsExtra)
require(timeSeries)
require(xts)
require(PerformanceAnalytics)
require(fTrading)
library(dygraphs)

#require(graphics)
#setwd("/Users/satishjoshi/DataBootcamp/ipython")
setwd("/Users/satishjoshi/DataBootcamp/bootcamp004_project/Project4-Machinelearning/uwonderiwander")
diesel_data=read.csv("weekly_diesel.csv",  stringsAsFactors=FALSE)
diesel_data_hdr=read.csv("weekly_diesel_hdr.csv",  stringsAsFactors=FALSE)
#diesel_data[is.na(diesel_data)] <- 0
summary(diesel_data)

diesel_data[diesel_data==""]  <- NA 
diesel_data[diesel_data=="NR"]  <- NA 
for (Var in names(diesel_data)) {
  missing <- sum(is.na(diesel_data[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

is.na(diesel_data$Date)
# [1] "EMD_EPD2D_PTE_R1X_DPG" "166"                  
# [1] "EMD_EPD2D_PTE_R1Y_DPG" "166"                  
# [1] "EMD_EPD2D_PTE_R1Z_DPG" "166"                  
# [1] "EMD_EPD2D_PTE_SCA_DPG" "67"                   
# [1] "EMD_EPD2D_PTE_R5XCA_DPG" "925"                    

# Date           EMD_EPD2D_PTE_NUS_DPG EMD_EPD2D_PTE_R10_DPG EMD_EPD2D_PTE_R1X_DPG
# Length:1145        Min.   :0.953         Min.   :0.961         Min.   :1.049        
# Class :character   1st Qu.:1.274         1st Qu.:1.254         1st Qu.:1.526        
# Mode  :character   Median :2.092         Median :2.141         Median :2.622        
# Mean   :2.277         Mean   :2.293         Mean   :2.589        
# 3rd Qu.:3.066         3rd Qu.:3.072         3rd Qu.:3.583        
# Max.   :4.764         Max.   :4.822         Max.   :4.889        
# NA's   :166          
# EMD_EPD2D_PTE_R1Y_DPG EMD_EPD2D_PTE_R1Z_DPG EMD_EPD2D_PTE_R20_DPG EMD_EPD2D_PTE_R30_DPG
# Min.   :1.047         Min.   :0.916         Min.   :0.930         Min.   :0.928        
# 1st Qu.:1.532         1st Qu.:1.394         1st Qu.:1.257         1st Qu.:1.216        
# Median :2.618         Median :2.441         Median :2.038         Median :2.035        
# Mean   :2.582         Mean   :2.423         Mean   :2.247         Mean   :2.217        
# 3rd Qu.:3.521         3rd Qu.:3.341         3rd Qu.:3.049         3rd Qu.:2.982        
# Max.   :4.913         Max.   :4.777         Max.   :4.698         Max.   :4.737        
# NA's   :166           NA's   :166                                                      
# EMD_EPD2D_PTE_R40_DPG EMD_EPD2D_PTE_R50_DPG EMD_EPD2D_PTE_SCA_DPG EMD_EPD2D_PTE_R5XCA_DPG
# Min.   :0.982         Min.   :1.032         Min.   :1.097         Min.   :2.043          
# 1st Qu.:1.309         1st Qu.:1.378         1st Qu.:1.494         1st Qu.:3.006          
# Median :2.124         Median :2.273         Median :2.434         Median :3.906          
# Mean   :2.310         Mean   :2.407         Mean   :2.546         Mean   :3.614          
# 3rd Qu.:3.139         3rd Qu.:3.192         3rd Qu.:3.317         3rd Qu.:3.973          
# Max.   :4.718         Max.   :4.909         Max.   :5.027         Max.   :4.383          


# Dec 12, 2011 - EMD_EPD2D_PTE_R5XCA_DPG
#Weekly West Coast (PADD 5) Except California No 2 Diesel Retail Prices  (Dollars per Gallon)
# introduced
# going to remove it

diesel_data$EMD_EPD2D_PTE_R5XCA_DPG <- NULL

is.na(diesel_data$Date)

# 26-May-97 is the 167th row
#deleting before that for consistency sake

diesel_data <- diesel_data[!is.na(diesel_data$EMD_EPD2D_PTE_R1X_DPG), ]
#now that is clean for analysis

diesel_data$Date
diesel_data$Date <- as.Date(as.character(diesel_data$Date),format="%d-%b-%y")
diesel_data <- xts(diesel_data, diesel_data$Date)
class(diesel_data)
str(diesel_data)

#new_diesel_data<-melt(diesel_data,'Date')
#ggplot(new_diesel_data,aes(x=Date,y=value,group=variable,color=variable) ) + geom_line() +scale_x_date()
colnames(diesel_data)
dygraph(diesel_data, main = "Diesel Prices Across Regions") %>% 
  dySeries("EMD_EPD2D_PTE_NUS_DPG", label = "US Average") %>%
  dySeries("EMD_EPD2D_PTE_R10_DPG", label = "East Coast") %>%
  dySeries("EMD_EPD2D_PTE_R1X_DPG", label = "New England") %>%
  dySeries("EMD_EPD2D_PTE_R1Y_DPG", label = "Central Altlantic") %>%
  dySeries("EMD_EPD2D_PTE_R1Z_DPG", label = "Lower Atlantic") %>%
  dySeries("EMD_EPD2D_PTE_R20_DPG", label = "Mid West") %>%
  dySeries("EMD_EPD2D_PTE_R30_DPG", label = "Gulf Coast") %>%
  dySeries("EMD_EPD2D_PTE_R40_DPG", label = "Rocky Mountain") %>%
  dySeries("EMD_EPD2D_PTE_R50_DPG", label = "West Coast") %>%
  dySeries("EMD_EPD2D_PTE_SCA_DPG", label = "California") %>%
  dyShading(from = "2008-2-11", to = "2009-1-5", color = "#CCEBD6") %>%
  dyShading(from = "2014-9-14", to = "2015-2-16", color = "#FFE6E6") %>%
#  http://www.infoplease.com/world/events/2008/jun.html  
  dyEvent("2008-02-01", "Economy Loses Jobs for the First Time in 52 Months", labelLoc = "bottom") %>%
  #  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector() %>%
  dyLegend(show = "follow")

# 
# http://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/economics_and_finance/transportation_services_index/documentation/index.html
# Data Entry 
# 
# http://www.truckline.com/News_and_Information_Reports_Truck_Tonnage.aspx
# 
# The current monthly values are copied (entered by keyboard) from the Monthly Truck Tonnage Report, a PDF that BTS receives monthly from ATA through an ATA membership.  The data can also be pulled from the monthly press releases.

#fleet_data=read.csv("fleet_owner_500.csv",  stringsAsFactors=FALSE)
