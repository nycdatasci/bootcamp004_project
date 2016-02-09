ibrary(data.table)
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

setwd("/Users/satishjoshi/DataBootcamp/ShinyProject")

#https://ai.fmcsa.dot.gov/SafetyProgram/spViolation.aspx?rpt=RDDV

census_data=read.csv("data/good_census_records.csv",  header=T, sep = ",",  stringsAsFactors=FALSE)
nrow(census_data) #1643370

inspection_data=read.csv("data/good_inspection_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
nrow(inspection_data) #5739024

violation_data=read.csv("data/good_violation_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
nrow(violation_data) #7910050

crash_data=read.csv("data/good_crash_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
nrow(crash_data) #226289

#then rename columns

insp_dot_covered = length(unique(inspection_data$DOT_NUMBER))
q = length(unique(inspection_data$DOT_NUMBER) %in% (unique(census_data$DOT_NUMBER)))
ifelse (insp_dot_covered == q, 
  paste("Good news - No bad DOT Numbers in Inspection Data", insp_dot_covered),
  paste("BAD news - BAD DOT Numbers in Inspection Data", insp_dot_covered, q)
)

viol_dot_covered = length(unique(violation_data$DOT_Number))
q2 = length(unique(violation_data$DOT_Number) %in% (unique(census_data$DOT_NUMBER)))
ifelse (viol_dot_covered == q2, 
        paste("Good news - No bad DOT Numbers in Violation Data", viol_dot_covered),
        paste("BAD news - BAD DOT Numbers in Violation Data", viol_dot_covered, q2)
)

paste("Ratio of Violations to Inspection per DOT Number", viol_dot_covered / insp_dot_covered)

crash_dot_covered = length(unique(crash_data$DOT_Number))
q3 = length(unique(crash_data$DOT_Number) %in% (unique(census_data$DOT_NUMBER)))
ifelse (crash_dot_covered == q, 
        paste("Good news - No bad DOT Numbers in crash Data", crash_dot_covered),
        paste("BAD news - BAD DOT Numbers in Inspection Data", crash_dot_covered, q3)
)

length(unique(violation_data$DOT_Number))
length(unique(violation_data$DOT_Number) %in% (unique(census_data$DOT_NUMBER)))

nrow(inspection_data[!(unique(inspection_data$DOT_NUMBER) %in% (unique(census_data$DOT_NUMBER)))])
     inspection_data[(unique(inspection_data$DOT_NUMBER) %in% (unique(census_data$DOT_NUMBER))),]
     
