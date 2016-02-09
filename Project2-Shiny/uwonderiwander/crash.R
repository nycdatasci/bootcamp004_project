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

# Loaing the data to create the state population map which will be compared against driver and unit state maps

data(df_pop_state)

#set this to the place where data is downloaded fromFMCSA web site

setwd("/Users/satishjoshi/DataBootcamp/ShinyProject")

#source("projecthelper.R")

suppressPackageStartupMessages(library(googleVis))

#Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
#                      more columns than column names
                    
#modified the file header to read extra columns needed due to bad data
#not sure whether this is going to make things worse butworth trying out
crash_data=read.csv2("data/test.txt",  header=T, sep = ",", quote = "", stringsAsFactors=FALSE)
nrow(crash_data) #1643373

#collect unique values for the extra columns
n4 = unique(crash_data$n4) 
n3 = unique(crash_data$n3)
n2 = unique(crash_data$n2)
n1 = unique(crash_data$n1)

#remove empty value from vector so that it can be used for looking at records in crash data
n4 = n4[(n4 != "")]
n3 = n3[(n3 != "")]
n2 = n2[(n2 != "")]
n1 = n1[(n1 != "")]

nrow(crash_data)
nrow(crash_data[crash_data$n4 == "",])
nrow(crash_data[crash_data$n3 == "",])
nrow(crash_data[crash_data$n2 == "",])
nrow(crash_data[crash_data$n1 == "",])

# > nrow(crash_data)
# [1] 227382
# > nrow(crash_data[crash_data$n4 == "",])
# [1] 225506
# > nrow(crash_data[crash_data$n3 == "",])
# [1] 223146
# > nrow(crash_data[crash_data$n2 == "",])
# [1] 106008
# > nrow(crash_data[crash_data$n1 == "",])
# [1] 29157
# 
# Wow we go from 227382 to 29157 if the header structure of data file is adhered to 
# else we risk mangled data

# Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
#                       duplicate 'row.names' are not allowed
violation_data=read.csv("data/2015Dec_Violation.txt",  header=T, row.names = NULL, sep = ",", quote = "", stringsAsFactors=FALSE)

inspection_data=read.csv("data/2015Dec_Inspection.txt",  header=T, stringsAsFactors=FALSE)

census_data=read.csv("data/2015Dec_Census.txt",  header=T, stringsAsFactors=FALSE)

summary(inspection_data)

# UNIQUE_ID         REPORT_NUMBER      REPORT_STATE         DOT_NUMBER     
# Min.   :        0   Length:5739344     Length:5739344     Min.   :      4  
# 1st Qu.:449795878   Class :character   Class :character   1st Qu.: 358548  
# Median :467619980   Mode  :character   Mode  :character   Median : 975449  
# Mean   :466547492                                         Mean   :1130227  
# 3rd Qu.:482082599                                         3rd Qu.:1891928  
# Max.   :495909036                                         Max.   :2833879  
# NA's   :211      
# INSP_DATE         INSP_LEVEL_ID   COUNTY_CODE_STATE   TIME_WEIGHT    DRIVER_OOS_TOTAL  
# Length:5739344     Min.   :1.000   Length:5739344     Min.   :1.000   Min.   : 0.00000  
# Class :character   1st Qu.:1.000   Class :character   1st Qu.:1.000   1st Qu.: 0.00000  
# Mode  :character   Median :2.000   Mode  :character   Median :2.000   Median : 0.00000  
# Mean   :2.088                      Mean   :1.798   Mean   : 0.05312  
# 3rd Qu.:3.000                      3rd Qu.:3.000   3rd Qu.: 0.00000  
# Max.   :6.000                      Max.   :3.000   Max.   :15.00000  
# NA's   :211                        NA's   :211     NA's   :211       
# VEHICLE_OOS_TOTAL TOTAL_HAZMAT_SENT   OOS_TOTAL       HAZMAT_OOS_TOTAL  
# Min.   : 0.0000   Min.   :0.00000   Min.   : 0.0000   Min.   : 0.00000  
# 1st Qu.: 0.0000   1st Qu.:0.00000   1st Qu.: 0.0000   1st Qu.: 0.00000  
# Median : 0.0000   Median :0.00000   Median : 0.0000   Median : 0.00000  
# Mean   : 0.2007   Mean   :0.06926   Mean   : 0.2538   Mean   : 0.00283  
# 3rd Qu.: 0.0000   3rd Qu.:0.00000   3rd Qu.: 0.0000   3rd Qu.: 0.00000  
# Max.   :30.0000   Max.   :9.00000   Max.   :30.0000   Max.   :14.00000  
# NA's   :211       NA's   :211       NA's   :211       NA's   :211       
# HAZMAT_PLACARD_REQ UNIT_TYPE_DESC      UNIT_MAKE         UNIT_LICENSE      
# Length:5739344     Length:5739344     Length:5739344     Length:5739344    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# 
# UNIT_LICENSE_STATE     VIN            UNIT_DECAL_NUMBER  UNIT_TYPE_DESC2   
# Length:5739344     Length:5739344     Length:5739344     Length:5739344    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# 
# UNIT_MAKE2        UNIT_LICENSE2      UNIT_LICENSE_STATE2     VIN2          
# Length:5739344     Length:5739344     Length:5739344      Length:5739344    
# Class :character   Class :character   Class :character    Class :character  
# Mode  :character   Mode  :character   Mode  :character    Mode  :character  
# 
# 
# 
# 
# UNIT_DECAL_NUMBER2 UNSAFE_INSP        FATIGUED_INSP      DR_FITNESS_INSP   
# Length:5739344     Length:5739344     Length:5739344     Length:5739344    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# 
# SUBT_ALCOHOL_INSP  VH_MAINT_INSP        HM_INSP           BASIC_VIOL       
# Length:5739344     Length:5739344     Length:5739344     Length:5739344    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# 
# UNSAFE_VIOL      FATIGUED_VIOL    DR_FITNESS_VIOL   SUBT_ALCOHOL_VIOL VH_MAINT_VIOL    
# Min.   : 0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   : 0.0000  
# 1st Qu.: 0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.: 0.0000  
# Median : 0.0000   Median :0.0000   Median :0.00000   Median :0.00000   Median : 0.0000  
# Mean   : 0.0839   Mean   :0.1328   Mean   :0.06966   Mean   :0.00118   Mean   : 0.9126  
# 3rd Qu.: 0.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.: 1.0000  
# Max.   :11.0000   Max.   :8.0000   Max.   :7.00000   Max.   :3.00000   Max.   :32.0000  
# NA's   :214       NA's   :211      NA's   :211       NA's   :212       NA's   :212      
# HM_VIOL      
# Min.   : 0.00   
# 1st Qu.: 0.00   
# Median : 0.00   
# Mean   : 0.01   
# 3rd Qu.: 0.00   
# Max.   :12.00   
# NA's   :154054 

str(inspection_data)

# 'data.frame':	5739344 obs. of  39 variables:
#   $ UNIQUE_ID          : int  434658276 434658303 434658312 434658321 434658330 434658339 434658348 434658357 434658366 434658411 ...
# $ REPORT_NUMBER      : chr  "0005621887" "0005621907" "0005621908" "0005621909" ...
# $ REPORT_STATE       : chr  "NC" "NC" "NC" "NC" ...
# $ DOT_NUMBER         : int  472357 511412 1112390 264184 690096 260762 427360 892176 1425099 2042197 ...
# $ INSP_DATE          : chr  "28-DEC-13" "28-DEC-13" "28-DEC-13" "28-DEC-13" ...
# $ INSP_LEVEL_ID      : int  2 3 1 2 3 3 3 2 3 2 ...
# $ COUNTY_CODE_STATE  : chr  "NC" "NC" "NC" "NC" ...
# $ TIME_WEIGHT        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ DRIVER_OOS_TOTAL   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ VEHICLE_OOS_TOTAL  : int  0 0 1 0 0 0 0 0 0 0 ...
# $ TOTAL_HAZMAT_SENT  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ OOS_TOTAL          : int  0 0 1 0 0 0 0 0 0 0 ...
# $ HAZMAT_OOS_TOTAL   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ HAZMAT_PLACARD_REQ : chr  "" "" "" "" ...
# $ UNIT_TYPE_DESC     : chr  "TRUCK TRACTOR" "TRUCK TRACTOR" "TRUCK TRACTOR" "TRUCK TRACTOR" ...
# $ UNIT_MAKE          : chr  "FRHT" "INTL" "WSTR" "FRHT" ...
# $ UNIT_LICENSE       : chr  "MC4649" "M129HZ" "LM2303" "2132581" ...
# $ UNIT_LICENSE_STATE : chr  "NC" "TN" "NC" "IN" ...
# $ VIN                : chr  "1FUYDZYB9XP975544" "3HSDJSJR1CN582944" "2WKEDDXH3VK948509" "1FUKJGLDR1DSBV311" ...
# $ UNIT_DECAL_NUMBER  : chr  "" "" "" "" ...
# $ UNIT_TYPE_DESC2    : chr  "SEMI-TRAILER" "" "SEMI-TRAILER" "SEMI-TRAILER" ...
# $ UNIT_MAKE2         : chr  "GREA" "" "TRAI" "GDAN" ...
# $ UNIT_LICENSE2      : chr  "AY67850" "" "CA87099" "2946JN" ...
# $ UNIT_LICENSE_STATE2: chr  "NC" "" "NC" "OK" ...
# $ VIN2               : chr  "1GRDM06277H705944" "" "1T9FC38B2V1066067" "1GRAA0620EB700032" ...
# $ UNIT_DECAL_NUMBER2 : chr  "" "" "" "" ...
# $ UNSAFE_INSP        : chr  "Y" "Y" "Y" "Y" ...
# $ FATIGUED_INSP      : chr  "Y" "Y" "Y" "Y" ...
# $ DR_FITNESS_INSP    : chr  "Y" "Y" "Y" "Y" ...
# $ SUBT_ALCOHOL_INSP  : chr  "Y" "Y" "Y" "Y" ...
# $ VH_MAINT_INSP      : chr  "Y" "" "Y" "Y" ...
# $ HM_INSP            : chr  "" "" "" "" ...
# $ BASIC_VIOL         : chr  "0" "0" "2" "0" ...
# $ UNSAFE_VIOL        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ FATIGUED_VIOL      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ DR_FITNESS_VIOL    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ SUBT_ALCOHOL_VIOL  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ VH_MAINT_VIOL      : int  0 0 2 0 0 0 0 1 0 2 ...
# $ HM_VIOL            : int  0 0 0 0 0 0 0 0 0 0 ...


max(inspection_data$INSP_DATE)
#[1] "31-OCT-15"

max(as.Date(inspection_data$INSP_DATE, format = "%d-%b-%y"))
min(as.Date(inspection_data$INSP_DATE, format = "%d-%b-%y"))

min(inspection_data$INSP_DATE[inspection_data$INSP_DATE != ""])
#[1] "01-APR-14"

inspection_data[is.na(inspection_data$INSP_DATE)]

unique(inspection_data$DRIVER_OOS_TOTAL)

unique(inspection_data$VEHICLE_OOS_TOTAL)

unique(inspection_data$HAZMAT_OOS_TOTAL)

unique(inspection_data$TOTAL_HAZMAT_SENT)

unique(inspection_data$UNIT_TYPE_DESC)

unique(inspection_data$UNIT_LICENSE_STATE)

unique(inspection_data$UNIT_MAKE)

unique(inspection_data$BASIC_VIOL)

unique(inspection_data$VH_MAINT_VIOL)

unique(inspection_data$REPORT_STATE)




FriendlyMakeNames = c("Ford", "Freightliner", "General Motors", "International", "Kenworth", 
                      "KenworthDT", "Mack", "Peterbilt", "Vanhan", "Volvo", "WesternStar")

MakeMapping = data.frame(UniqueMakes, FriendlyMakeNames)

UniqueUnitStates = levels(inspection_data$UNIT_LICENSE_STATE)

UniqueReportStates = levels(inspection_data$REPORT_STATE)
UniqueCountyCodeStates = levels(inspection_data$COUNTY_CODE_STATE)
write.csv(UniqueMakes, "MakeList.csv")

write.csv(UniqueUnitStates, "InspStateList.csv")
write.csv(UniqueReportStates, "ReportStateList.csv")
write.csv(UniqueCountyCodeStates, "CountyCodeStateList.csv")

#any columns with mising values 
names(inspection_data[apply(inspection_data, 2, function(x) any(is.na(x)))])
#[1] "DOT_NUMBER"          "INSP_LEVEL_ID"       "TIME_WEIGHT"         "DRIVER_OOS_TOTAL"    "VEHICLE_OOS_TOTAL"   "TOTAL_HAZMAT_SENT"  
#[7] "OOS_TOTAL"           "HAZMAT_OOS_TOTAL"    "UNIT_LICENSE"        "UNIT_LICENSE_STATE"  "VIN"                 "UNIT_DECAL_NUMBER"  
#[13] "UNIT_MAKE2"          "UNIT_LICENSE2"       "UNIT_LICENSE_STATE2" "VIN2"                "UNIT_DECAL_NUMBER2"  "UNSAFE_VIOL"        
#[19] "FATIGUED_VIOL"       "DR_FITNESS_VIOL"     "SUBT_ALCOHOL_VIOL"   "VH_MAINT_VIOL"       "HM_VIOL" 


MissingValueRows = which(apply(inspection_data, 1, function(x) any(is.na(x))))
write.csv(MissingValueRows, "InspectionsMissingValueRows.csv")
#report state looks like same as inspection state based upon the unique list for the two columns

MissingValueSummary = sapply(inspection_data, function(x) sum(is.na(x)))
write.csv(MissingValueSummary, "InspectionMissingValueSummary.csv")

MissingValueRowDetails = apply(inspection_data, 1, function(x) any(is.na(x)))
write.csv(MissingValueRowDetails, "InspectionMissingValueDetails.csv")

# Question - sum(MissingValueSummary) = 159750, length(MissingValueRows) = 156566, how to account for diff of 3184
# And MissingValueRowDetails shows 488856 rows

library(mice)
library(VIM)
md.pattern(inspection_data)

# Error in data.matrix(x) : invalid multibyte string at '<bf>?51<30>183'
# In addition: Warning messages:
#   1: In data.matrix(x) : NAs introduced by coercion
# 2: In data.matrix(x) : NAs introduced by coercion
# 3: In data.matrix(x) : NAs introduced by coercion
# 4: In data.matrix(x) : NAs introduced by coercion
# 5: In data.matrix(x) : NAs introduced by coercion
# 6: In data.matrix(x) : NAs introduced by coercion
# 7: In data.matrix(x) : NAs introduced by coercion
aggr(inspection_data)

md.pattern(violation_data)

# > md.pattern(violation_data)
# X.INSP_DATE. row.names X.OOS_INDICATOR. X.OOS_WEIGHT. X.SEVERITY_WEIGHT. X.TIME_WEIGHT. X.TOT_SEVERITY_WGHT. X.GROUP_DESC. X.DOT_NUMBER. X.VIOL_UNIT. X.UNIQUE_ID. X.SECTION_DESC.
# 257770            1         1                1             1                  1              1                    1             0             1            1            0               0
# 1427209            1         1                1             1                  1              1                    1             1             1            0            0               0
# 33            1         1                1             1                  1              1                    1             0             0            0            0               1
# 5            1         1                1             1                  1              1                    1             0             1            0            0               1
# 872159            1         1                1             1                  1              1                    1             0             0            1            0               0
# 2474667            1         1                1             1                  1              1                    1             1             0            0            0               0
# 1876            1         1                1             1                  1              1                    1             0             0            0            0               1
# 419586            1         1                1             1                  1              1                    1             0             1            0            0               0
# 1905713            1         1                1             1                  1              1                    1             0             0            0            0               0
# 231876            1         0                0             0                  0              0                    0             0             0            0            0               0
# 114948            0         0                0             0                  0              0                    0             0             0            0            1               0
# 200447            0         1                0             0                  0              0                    0             0             0            0            0               0
# 3761            0         0                0             0                  0              0                    0             0             0            0            0               0
# 319156    350585           551032        551032             551032         551032               551032       4008174       5805480      6780121      7795102         7908136
# X.VIOL_VALUE. X.VIOL_CODE. X.BASIC_DESC.         
# 257770             0            0             0        6
# 1427209             0            0             0        6
# 33             1            0             0        6
# 5             0            0             0        6
# 872159             0            0             0        7
# 2474667             0            0             0        7
# 1876             0            0             0        7
# 419586             0            0             0        7
# 1905713             0            0             0        8
# 231876             0            0             0       14
# 114948             0            0             0       14
# 200447             0            0             0       14
# 3761             0            0             0       15
# 7910017      7910050       7910050 59452031
# Warning messages:
#   1: In data.matrix(x) : NAs introduced by coercion
# 2: In data.matrix(x) : NAs introduced by coercion
# 3: In data.matrix(x) : NAs introduced by coercion
# 4: In data.matrix(x) : NAs introduced by coercion
# 5: In data.matrix(x) : NAs introduced by coercion
# 6: In data.matrix(x) : NAs introduced by coercion
# 7: In data.matrix(x) : NAs introduced by coercion
# 8: In data.matrix(x) : NAs introduced by coercion
# 9: In data.matrix(x) : NAs introduced by coercion
# 10: In data.matrix(x) : NAs introduced by coercion
# >
aggr(violation_data)

md.pattern(crash_data)
aggr(crash_data)


md.pattern(census_data)
aggr(census_data)

