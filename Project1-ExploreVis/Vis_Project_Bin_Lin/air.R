#
library(dplyr)
library(ggplot2)

setwd("/Users/binlin/Documents/DataScience/projects/visualization project/")

yearsToImport = c(1980:2015)
airFilePaths = paste('data/aqidaily', yearsToImport, '.csv', sep = '')

airFiles = list()

# Read all the needed csv for each year
for(i in 1:length(airFilePaths)) {
  airFiles[[i]] = read.csv(airFilePaths[i], na.strings='.')
}
  
# Combine the daily data into one data frame
airData = Reduce(function(...) merge(..., all=TRUE), airFiles)

# Cleanup the data format
airData$Date = as.Date(airData$Date, '%m/%d/%Y')
airData$NO2 = as.numeric(airData$NO2)

# Quick check the missing value (to make sure data is good)
na_count_by_rows = rowSums(is.na(airData))
airData[which(na_count_by_rows > 0), ]

conditions = c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Unhealthy', 'Very Unhealthy', 'Hazardous')
qualityColors = c('Good'='green','Moderate'='yellow', 'Unhealthy for Sensitive Groups'='orange', 'Unhealthy'='red', 'Very Unhealthy'='purple', 'Hazardous'='maroon')

getCondition = function(aqi) {
  condition = NULL
  if ( aqi >= 0 & aqi <= 50) {
    condition = conditions[1]
  } else if ( aqi > 50 & aqi <= 100) {
    condition = conditions[2]
  } else if ( aqi > 100 & aqi <= 150) {
    condition = conditions[3]
  } else if ( aqi > 150 & aqi <= 200) {
    condition = conditions[4]
  } else if ( aqi > 200 & aqi <= 300) {
    condition = conditions[5]
  } else if ( aqi > 300 & aqi <= 500) {
    condition = conditions[6]
  } else {
    condition = "?"
  }
  
  return (condition)
}


# Add month, year, condition as new columns
airData2 = mutate(airData, Condition = factor(sapply(Overall.AQI.Value, getCondition), levels = conditions), Month = as.integer(format(Date, "%m")), Year = as.integer(format(Date, "%Y")))

byYear = group_by(airData2, Year)

averageAqiByYear = summarise(byYear, Air.Quality.Index = mean(Overall.AQI.Value))

averageAqiByYear = mutate(averageAqiByYear, Condition = factor(sapply(Air.Quality.Index, getCondition), levels = conditions))


# diagram 1: all the dots for every date from beginning year to end year
ggplot(airData2, aes(Date, Overall.AQI.Value)) + 
  geom_point(aes(color=Condition)) + 
  geom_smooth() + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  scale_color_manual(values=qualityColors) 


# diagram 2: average per year
ggplot(averageAqiByYear, aes(Year, Air.Quality.Index)) + 
  geom_point(aes(color=Condition)) +
  geom_smooth() + 
  scale_color_manual(values=qualityColors)


# diagram 3: trend of each pollutant cross these years
# bar, percentage of major pollutant 
ggplot(filter(airData2, (Year %% 1 ==0)), aes(Year, fill = Main.Pollutant)) + geom_bar(position="fill")

# The index of each polluant accorss the years
ggplot(airData2, aes(x=Date)) +      
  geom_smooth(aes(y = CO, colour="CO"))  +      
  geom_smooth(aes(y = SO2, colour="SO2")) +
  geom_smooth(aes(y = NO2, colour="NO2")) +      
  geom_smooth(aes(y = Ozone, colour="Ozone")) + 
  geom_smooth(aes(y = PM25, colour="PM25")) +      
  geom_smooth(aes(y = PM10, colour="PM10")) +
  ylab("Air Quality Index") +
  scale_color_manual("Pollutant", values = c("CO"="blue", "SO2"="red", "NO2"="orange", "Ozone"="purple", "PM25"="yellow", "PM10"="green"))

ggplot(airData2, aes(x=Date)) +      
  geom_point(aes(y = CO, colour="CO"))  +      
  geom_point(aes(y = SO2, colour="SO2")) +
  geom_point(aes(y = NO2, colour="NO2")) +      
  geom_point(aes(y = Ozone, colour="Ozone")) + 
  geom_point(aes(y = PM25, colour="PM25")) +      
  geom_point(aes(y = PM10, colour="PM10")) +
  ylab("Air Quality Index") +
  scale_color_manual("Pollutant", values = c("CO"="blue", "SO2"="red", "NO2"="orange", "Ozone"="purple", "PM25"="yellow", "PM10"="green"))

# diagram 4: average per month
byMonth = group_by(airData2, Month)
averageAqiByMonth = summarise(byMonth, Air.Quality.Index = mean(Overall.AQI.Value))
averageAqiByMonth = mutate(averageAqiByMonth, Condition = factor(sapply(Air.Quality.Index, getCondition), levels = conditions))

ggplot(averageAqiByMonth, aes(Month, Air.Quality.Index)) + scale_x_discrete() +
  geom_point(aes(color=Condition)) +
  geom_smooth() + 
  scale_color_manual(values=qualityColors)


# diagram 5: September 11, 2001
airDataSept11 = read.csv('data/nyc_aqidaily2001.csv')
airDataSept11$Date = as.Date(airDataSept11$Date, '%m/%d/%Y')
airDataSept11_2 = mutate(airDataSept11, Condition = factor(sapply(Overall.AQI.Value, getCondition), levels = conditions))

# points show the AQI within the two months after Sept 11, 2001
ggplot(filter(airDataSept11_2, Date >= "2001-09-01" & Date <= "2001-12-01"), aes(Date, Overall.AQI.Value)) + 
  geom_point(aes(color=Condition), size=4) + 
  scale_x_date(date_breaks = "30 day") + 
  geom_line()+
  scale_color_manual(values=qualityColors) 

# diagram 6: Queens county: 2011,2012,2013 (Not used in presentation)
airQueensFilePaths = paste('data/aqi_queens/aqidaily', c(2011:2013), '.csv', sep = '')
airFilesQueens = list()

# Read all the needed csv for each year
for(i in 1:length(airQueensFilePaths)) {
  airFilesQueens[[i]] = read.csv(airQueensFilePaths[i], na.strings='.')
}

# Combine the daily data into one data frame
airDataQueens = Reduce(function(...) merge(..., all=TRUE), airFilesQueens)

airDataQueens$Date = as.Date(airDataQueens$Date, '%m/%d/%Y')
airDataQueens2 = mutate(airDataQueens, Condition = factor(sapply(Overall.AQI.Value, getCondition), levels = conditions))

ggplot(airDataQueens2, aes(Date, Overall.AQI.Value)) + 
  geom_point(aes(color=Condition)) + 
  geom_smooth() + 
  scale_color_manual(values=qualityColors) 



