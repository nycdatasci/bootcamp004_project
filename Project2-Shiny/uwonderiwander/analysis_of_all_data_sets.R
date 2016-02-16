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

#iconv -c -f us-ascii -t UTF-8 2015Dec_crash.txt > 2015Dec_crashUTF8.txt
#two cleanups were done using vi - :%s/, / - /g which cleaned bulk of rows
#then :%s/Sand,Mud,Dirt,Oil or Gravel/Sand - Mud - Dirt - Oil or Gravel/g

crash_data=read.csv("data/2015Dec_crashUTF8_cleaned.txt",  header=T, sep = ",", quote = "", stringsAsFactors=FALSE)
nrow(crash_data) #226288


#none of this is needed from below now that data cleaning is done in advance
#collect unique values for the extra columns
# n4 = unique(crash_data$n4) 
# n3 = unique(crash_data$n3)
# n2 = unique(crash_data$n2)
# n1 = unique(crash_data$n1)
# 
# #remove empty value from vector so that it can be used for looking at records in crash data
# n4 = n4[(n4 != "")]
# n3 = n3[(n3 != "")]
# n2 = n2[(n2 != "")]
# n1 = n1[(n1 != "")]
# 
# nrow(crash_data)
# nrow(crash_data[crash_data$n4 == "",])
# nrow(crash_data[crash_data$n3 == "",])
# nrow(crash_data[crash_data$n2 == "",])
# nrow(crash_data[crash_data$n1 == "",])
# 
# # > nrow(crash_data)
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

#:s/, / - /g to fix data problem in this file as welll
#:1,$s/,Tire vs. Load/ - Tire vs. Load/g
#was resulting in more substitutions hence data has to be looked at further
# problem is - WEIGHT CARRIED ON 1ST AXLE EXCEEDS TIRE LOAD LIMIT OF 12,350 LBS (6,175 LBS X 2)

violation_data=read.csv("data/2015Dec_Violation_cleaned.txt",  header=T, row.names = NULL, sep = ",", quote = "", stringsAsFactors=FALSE)

#7359019 rows are read as 7360385 rows so prob is not fixed

inspection_data=read.csv("data/2015Dec_InspectionUTF8.txt",  header=T,sep = ",", quote = "", stringsAsFactors=FALSE)

#iconv -c -f us-ascii -t UTF-8 2015Dec_Census.txt > 2015Dec_CensusUTF8.txt 

#only census data is quoted, nothing else

census_data=read.csv("data/2015Dec_CensusUTF8.txt",  header=T, sep = ",", stringsAsFactors=FALSE)

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

#output written to separate file

# Error in data.matrix(x) : invalid multibyte string at '<bf>?51<30>183'
# In addition: Warning messages:
#   1: In data.matrix(x) : NAs introduced by coercion
# 2: In data.matrix(x) : NAs introduced by coercion
# 3: In data.matrix(x) : NAs introduced by coercion
# 4: In data.matrix(x) : NAs introduced by coercion
# 5: In data.matrix(x) : NAs introduced by coercion
# 6: In data.matrix(x) : NAs introduced by coercion
# 7: In data.matrix(x) : NAs introduced by coercion

#satishs-MBP:data satishjoshi$ iconv -f us-ascii -t UTF-8 2015Dec_Inspection.txt > 2015Dec_InspectionUTF8.txt 

#iconv: 2015Dec_Inspection.txt:897423:148: cannot convert
#iconv -c -f us-ascii -t UTF-8 2015Dec_Inspection.txt > 2015Dec_InspectionUTF8.txt

p = md.pairs(inspection_data)
p$rm[,1:10]
p$rm[,11:20]
p$rm[,21:30]
p$rm[,31:39]

aggr(inspection_data)
#did not produce anything so have to save files manually :( pdf("data/InspAggr.pdf",width=16,height=11)
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

p = md.pairs(violation_data)
p$rm[,1:10]
p$rm[,11:15]


aggr(violation_data)
str(violation_data)
# 'data.frame':	7910050 obs. of  15 variables:
#   $ row.names           : chr  "434658312" "434658312" "434658357" "434658411" ...
# $ X.UNIQUE_ID.        : chr  "28-DEC-13" "28-DEC-13" "28-DEC-13" "28-DEC-13" ...
# $ X.INSP_DATE.        : chr  "1112390" "1112390" "892176" "2042197" ...
# $ X.DOT_NUMBER.       : chr  "39347E" "39395A" "39345DLPC" "39325F" ...
# $ X.VIOL_CODE.        : chr  "Vehicle Maintenance" "Vehicle Maintenance" "Vehicle Maintenance" "Vehicle Maintenance" ...
# $ X.BASIC_DESC.       : chr  "N" "N" "N" "N" ...
# $ X.OOS_INDICATOR.    : int  0 0 0 0 0 0 NA 0 0 0 ...
# $ X.OOS_WEIGHT.       : int  4 2 4 6 6 2 NA 2 7 5 ...
# $ X.SEVERITY_WEIGHT.  : int  1 1 1 1 1 1 NA 1 1 1 ...
# $ X.TIME_WEIGHT.      : int  4 2 4 6 6 2 NA 2 7 5 ...
# $ X.TOT_SEVERITY_WGHT.: int  4 2 4 6 6 2 NA 2 7 5 ...
# $ X.VIOL_VALUE.       : chr  "Clamp or Roto type brake out-of-adjustment" "No/discharged/unsecured fire extinguisher" "Brake Connections with Leaks - Connection to Power Unit" "Stop lamp violations" ...
# $ X.SECTION_DESC.     : chr  "Brakes Out of Adjustment" "Emergency Equipment" "Brakes" "Lighting" ...
# $ X.GROUP_DESC.       : chr  "1" "1" " All Others" "1" ...
# $ X.VIOL_UNIT.        : chr  "" "" "1" "" ...

md.pattern(crash_data)

p = md.pairs(crash_data)
p$rm[,1:10]
p$rm[,11:15]



#Error in data.matrix(x) : invalid multibyte string at '<bf>'
#In addition: There were 15 warnings (use warnings() to see them)
# > warnings()
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
# 11: In data.matrix(x) : NAs introduced by coercion
# 12: In data.matrix(x) : NAs introduced by coercion
# 13: In data.matrix(x) : NAs introduced by coercion
# 14: In data.matrix(x) : NAs introduced by coercion
# 15: In data.matrix(x) : NAs introduced by coercion

aggr(crash_data)

str(crash_data)

# 'data.frame':	227382 obs. of  25 variables:
#   $ X.REPORT_NUMBER.              : chr  "AK0000001476" "AK0000014153" "AK0000014417" "AK0000014691" ...
# $ X.REPORT_SEQ_NO.              : chr  "1" "1" "1" "1" ...
# $ X.DOT_NUMBER.                 : chr  "315946" "1570490" "1623383" "190356" ...
# $ X.REPORT_DATE.                : chr  "14-JAN-14" "28-JAN-14" "15-MAR-14" "28-APR-14" ...
# $ X.REPORT_STATE.               : chr  "AK" "AK" "AK" "AK" ...
# $ X.FATALITIES.                 : int  0 0 0 0 0 0 0 0 0 0 ...
# $ X.INJURIES.                   : int  0 0 1 0 1 1 1 1 0 0 ...
# $ X.TOW_AWAY.                   : chr  "Y" "Y" "Y" "Y" ...
# $ X.HAZMAT_RELEASED.            : chr  "" "" "" "" ...
# $ X.TRAFFICWAY_DESC.            : chr  "Two-Way Trafficway" "Two-Way Trafficway" "Two-Way Trafficway" "Two-Way Trafficway" ...
# $ X.ACCESS_CONTROL_DESC.        : chr  " Divided" " Not Divided" " Not Divided" " Not Divided" ...
# $ X.ROAD_SURFACE_CONDITION_DESC.: chr  " Unprotected Median" "Full Control" "No Control" "No Control" ...
# $ X.WEATHER_CONDITION_DESC.     : chr  "Partial Access Control" "Dry" "Snow" "Dry" ...
# $ X.LIGHT_CONDITION_DESC.       : chr  "Wet" "No Adverse Conditions" "Snow" "No Adverse Conditions" ...
# $ X.VEHICLE_ID_NUMBER.          : chr  "Snow" "Dark - Lighted" "Dark - Not Lighted" "Daylight" ...
# $ X.VEHICLE_LICENSE_NUMBER.     : chr  "Daylight" "1FUJCRAV25PV11467" "1HTSCAAM5YH269831" "1XKDP4TX6DR344080" ...
# $ X.VEHICLE_LICENSE_STATE.      : chr  "1FUJA6CK26PW82860" "FAV751" "FHN579" "GKU320" ...
# $ X.SEVERITY_WEIGHT.            : chr  "GKC253" "AK" "AK" "AK" ...
# $ X.TIME_WEIGHT.                : chr  "AK" "1" "2" "1" ...
# $ X.CITATION_ISSUED_DESC.       : chr  "1" "1" "1" "1" ...
# $ X.SEQ_NUM.                    : chr  "1" "YES" "" "YES" ...
# $ n1                            : chr  "YES" "1" "1" "1" ...
# $ n2                            : chr  "1" "" "" "" ...
# $ n3                            : chr  "" "" "" "" ...
# $ n4                            : chr  "" "" "" "" ...

md.pattern(census_data)
# Error in data.matrix(x) : 
#   invalid multibyte string at '<cb>LKH<41>TIB@BELLSOUTH.NET'
# In addition: There were 17 warnings (use warnings() to see them)
# > warnings()
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
# 11: In data.matrix(x) : NAs introduced by coercion
# 12: In data.matrix(x) : NAs introduced by coercion
# 13: In data.matrix(x) : NAs introduced by coercion
# 14: In data.matrix(x) : NAs introduced by coercion
# 15: In data.matrix(x) : NAs introduced by coercion
# 16: In data.matrix(x) : NAs introduced by coercion
# 17: In data.matrix(x) : NAs introduced by coercion
aggr(census_data)
str(census_data)

# 'data.frame':	1643373 obs. of  26 variables:
#   $ DOT_NUMBER         : int  10000 1000000 1000002 1000004 1000005 1000007 1000008 1000009 1000010 1000012 ...
# $ LEGAL_NAME         : chr  "POWELL DISTRIBUTING CO INC" "JAMES EARL KILLINGSWORTH JR" "NEW JERSEY BOOM & ERECTORS INC" "NEAL RAY" ...
# $ DBA_NAME           : chr  "" "" "" "RAY TRUCKING" ...
# $ CARRIER_OPERATION  : chr  "A" "A" "A" "C" ...
# $ HM_FLAG            : chr  "N" "N" "N" "N" ...
# $ PC_FLAG            : chr  "N" "N" "N" "N" ...
# $ PHY_STREET         : chr  "9125 N BURRAGE AVE" "1789 LEE RD #335" "120 SANS DRIVE" "218 PEACHTREE ST N" ...
# $ PHY_CITY           : chr  "PORTLAND" "SMITHS" "HENRYVILLE" "WARWICK" ...
# $ PHY_STATE          : chr  "OR" "AL" "PA" "GA" ...
# $ PHY_ZIP            : chr  "97217-6961" "36877" "18332" "31796" ...
# $ PHY_COUNTRY        : chr  "US" "US" "US" "US" ...
# $ MAILING_STREET     : chr  "P O BOX 17160" "1789 LEE RD #335" "120 SANS DRIVE" "218 PEACHTREE ST N" ...
# $ MAILING_CITY       : chr  "PORTLAND" "SMITHS" "HENRYVILLE" "WARWICK" ...
# $ MAILING_STATE      : chr  "OR" "AL" "PA" "GA" ...
# $ MAILING_ZIP        : chr  "97217-0160" "36867" "18332" "31796" ...
# $ MAILING_COUNTRY    : chr  "US" "US" "US" "US" ...
# $ TELEPHONE          : chr  "(503) 289-5558" "(706) 315-3289" "(570) 620-1546" "(229) 535-4140" ...
# $ FAX                : chr  "(503) 735-0100" "" "(570) 620-1517" "" ...
# $ EMAIL_ADDRESS      : chr  "" "" "NJBOOM@ENTER.NET" "" ...
# $ MCS150_DATE        : chr  "27-JUN-14" "03-APR-08" "23-DEC-13" "12-MAR-14" ...
# $ MCS150_MILEAGE     : num  20000 50000 194261 532000 0 ...
# $ MCS150_MILEAGE_YEAR: int  2009 2004 2013 2006 NA 2012 2005 2005 2014 2008 ...
# $ ADD_DATE           : chr  "01-JUN-74" "23-JAN-02" "22-JAN-02" "22-JAN-02" ...
# $ OIC_STATE          : chr  "OR" "AL" "PA" "GA" ...
# $ NBR_POWER_UNIT     : int  1 5 1 2 1 1 4 1 5 9 ...
# $ DRIVER_TOTAL       : int  1 1 1 1 1 1 3 1 3 5 ...
# 

summary(census_data)

p = md.pairs(census_data)
p$rm[,1:10]
p$rm[,11:20]
p$rm[,21:26]


unique(inspection_data$HM_VIOL)
inspection_data$HM_VIOL[is.na(inspection_data$HM_VIOL)] = 0

bad_records = inspection_data[is.na(inspection_data$DOT_NUMBER),]

write.csv(bad_records, "data/bad_inspection_records.csv")
inspection_data = inspection_data[!is.na(inspection_data$DOT_NUMBER),]

inspection_data$UNIT_LICENSE[is.na(inspection_data$UNIT_LICENSE)] = "Unknown"
inspection_data$UNIT_DECAL_NUMBER[is.na(inspection_data$UNIT_DECAL_NUMBER)] = "Unknown"
inspection_data$UNIT_LICENSE2[is.na(inspection_data$UNIT_LICENSE2)] = "Unknown"
inspection_data$UNIT_DECAL_NUMBER2[is.na(inspection_data$UNIT_DECAL_NUMBER2)] = "Unknown"

bad_records2 = inspection_data[(is.na(inspection_data$UNIT_LICENSE_STATE)) | (is.na(inspection_data$VIN)) ,]
write.csv(bad_records, "data/bad_inspection_records2.csv")
inspection_data = inspection_data[!((is.na(inspection_data$UNIT_LICENSE_STATE)) | (is.na(inspection_data$VIN))) ,]

inspection_data$UNIT_LICENSE2[is.na(inspection_data$UNIT_LICENSE2)] = ""
inspection_data$UNIT_MAKE2[is.na(inspection_data$UNIT_MAKE2)] = ""
inspection_data$UNIT_LICENSE_STATE2[is.na(inspection_data$UNIT_LICENSE_STATE2)] = ""
inspection_data$VIN2[is.na(inspection_data$VIN2)] = ""
inspection_data$UNSAFE_VIOL[is.na(inspection_data$UNSAFE_VIOL)] = 0
inspection_data$SUBT_ALCOHOL_VIOL[is.na(inspection_data$SUBT_ALCOHOL_VIOL)] = 0
inspection_data$VH_MAINT_VIOL[is.na(inspection_data$VH_MAINT_VIOL)] = 0

p = md.pairs(inspection_data)
p$rm[,1:10] 
p$rm[,11:20]
p$rm[,21:30]
p$rm[,31:39]

write.csv(inspection_data, "data/good_inspection_records.csv")



p = md.pairs(census_data)
p$rm[,1:10]
p$rm[,11:20]
p$rm[,21:26]

bad_census_records = census_data[(is.na(census_data$DBA_NAME)) | (is.na(census_data$EMAIL_ADDRESS)) |
                                  (is.na(census_data$MCS150_MILEAGE)) | (is.na(census_data$MCS150_MILEAGE_YEAR)) |
                                  (is.na(census_data$NBR_POWER_UNIT)) | (is.na(census_data$DRIVER_TOTAL)) |
                                  (is.na(census_data$MAILING_STATE)) | (is.na(census_data$PHY_STATE)) |
                                  (is.na(census_data$LEGAL_NAME)) ,]
bad_census_records
write.csv(bad_census_records, "data/corrected_census_records.csv")

census_data$PHY_STATE[is.na(census_data$PHY_STATE)] = census_data$MAILING_STATE[is.na(census_data$PHY_STATE)] #3
census_data$LEGAL_NAME[is.na(census_data$LEGAL_NAME)] = census_data$DBA_NAME[is.na(census_data$LEGAL_NAME)] #5
census_data$MAILING_STATE[is.na(census_data$MAILING_STATE)] = census_data$PHY_STATE[is.na(census_data$MAILING_STATE)] #3
census_data$DBA_NAME[is.na(census_data$DBA_NAME)] = census_data$LEGAL_NAME[is.na(census_data$DBA_NAME)] #57
census_data$EMAIL_ADDRESS[is.na(census_data$EMAIL_ADDRESS)] = "" #77
census_data$MCS150_MILEAGE[is.na(census_data$MCS150_MILEAGE)] = 0 #521285
census_data$MCS150_MILEAGE_YEAR[is.na(census_data$MCS150_MILEAGE_YEAR)] = "1969" #738642

census_data$NBR_POWER_UNIT[is.na(census_data$NBR_POWER_UNIT)] = 0 #119758
census_data$DRIVER_TOTAL[is.na(census_data$DRIVER_TOTAL)] = 0 #40896


census_data = census_data[!((is.na(census_data$MAILING_STATE)) | (is.na(census_data$PHY_STATE))) ,]

write.csv(census_data, "data/good_census_records.csv")

p = md.pairs(violation_data)
p$rm[,1:10]
p$rm[,11:15]

violation_data$X.OOS_INDICATOR.[is.na(violation_data$X.OOS_INDICATOR.)] = 0 #551032
violation_data$X.OOS_WEIGHT.[is.na(violation_data$X.OOS_WEIGHT.)] = 0 #551032
violation_data$X.SEVERITY_WEIGHT.[is.na(violation_data$X.SEVERITY_WEIGHT.)] = 0 #551032
violation_data$X.TIME_WEIGHT.[is.na(violation_data$X.TIME_WEIGHT.)] = 0 #551032
violation_data$X.TOT_SEVERITY_WGHT.[is.na(violation_data$X.TOT_SEVERITY_WGHT.)] = 0 #551032


colnames(violation_data) = c("Unique_ID",
                             #unique identification number for each inspection
                             "Insp_Date",           
                             #- the date of the inspection 
                             "DOT_Number",        
                             #- Unique number assigned to a company by the DOT
                             "Viol_Code",          
                             #- Code of the violation
                             "BASIC_Desc",          
                             #- Name of the BASIC   
                             "OOS_Indicator",       
                             #- 'Y' mean the violation is identified as Out-Of-Service violation
                             "OOS_Weight",         
                             #- The weight that is assigned to a violation if its identified as an Out-Of-Service violation 
                             "Severity_Weight",    
                             #- The severity weight that is assigned to a violation
                             "Time_Weight",         
                             #- The time weight that is assigned to a violation
                             "Total_Severity_Wght",
                             #- Total severity weight of a violation 
                             "Viol_Value",
                             "Section_Desc",       
                             #- The description of a violation
                             "Group_Desc",         
                             #- The description of the violation group
                             "Viol_Unit"          
                             #- The unit a violation is cited against: vehicle main unit (1), vehicle secondary unit (2), Driver (D), Co-driver (C)
                             )
write.csv(violation_data, "data/good_violation_records.csv")

unique(violation_data$ExtraCol)
unique(violation_data$Viol_Unit)
min(violation_data$Unique_ID)     
nchar(max(violation_data$Unique_ID))
violation_data[is.numeric(violation_data$Unique_ID),]
violation_data[!is.numeric(violation_data$Unique_ID),]

nrow(violation_data[nchar(violation_data$Unique_ID) > 9,])

#1367 - matches with the extra rows read

violation_data[nchar(violation_data$Unique_ID) > 9,]

#all of them have Tire vs. Load moving into next row and pushing out things

min(violation_data$Insp_date)     
max(violation_data$Insp_date)     
violation_data[is.numeric(violation_data$Insp_date),]
violation_data[!is.numeric(violation_data$Insp_date),]

violation_data[violation_data$Unique_ID == 435073644,]

unique(violation_data$Viol_Unit)     
unique(violation_data$OOS_Indicator)     
unique(violation_data$Time_Weight)     


#Have to ignore anything after Group_Desc because of the way the data came in until import is improved

#https://ai.fmcsa.dot.gov/SafetyProgram/spViolation.aspx?rpt=RDDV

nrow(inspection_data[!(unique(inspection_data$DOT_NUMBER) %in% (unique(census_data$DOT_NUMBER))),]
inspection_data[(unique(inspection_data$DOT_NUMBER) %in% (unique(census_data$DOT_NUMBER))),]




colnames(crash_data) = c("Report_number", # - Unique state report number for the incident
                             "Report_seq_no",  #             - Sequence number for each vehicle involved in a crash
                             "DOT_Number", #                  - Unique number assigned to a company by the DOT
                             "Report_Date", #                 - The date a incident occurred
                             "Report_State", #                - State abbreviation
                             "Fatalities", #                  - Total number of fatalities reported in the crash
                             "Injuries", #                    - Total number of injuries reported in the crash 
                             "Tow_Away", #                    - 'Y' indicates that a vehicle involved in the crash was towed from the scene. 
                             "Hazmat_released", #             - 'Y' indicates that hazardous materials were released at the time of a crash
                             "Trafficway_Desc", #             - Description of the trafficway
                             "Access_Control_Desc", #         - Description of the access control
                             "Road_surface_Condition_Desc", # - Description of the road surface condition
                             "Weather_Condition_Desc", #      - Description of the weather condition
                             "Light_Condition_Desc", #        - Description of the light condition
                             "Vehicle_ID_Number", #           - Vehicle Identification number (VIN)
                             "Vehicle_License_number", #      - Vehicle license number
                             "Vehicle_license_state", #       - vehicle license state
                             "Severity_Weight", #             - The severity weight that is assigned to the incident
                             "Time_weight", #                 - the time weight that is assigned to the incident
                             "citation_issue_desc",  #        - Description of the citation issue
                             "seq_num" #                     - Sequence number
)

p = md.pairs(crash_data)
p$rm[,1:10]
p$rm[,11:21]

crash_data$Vehicle_License_number[is.na(crash_data$Vehicle_License_number)] = "NotRecorded" #16
crash_data = crash_data[!((is.na(crash_data$Vehicle_ID_Number)) | (is.na(crash_data$Vehicle_License_number))) ,] #8
unique(crash_data$Time_weight)

nrow(crash_data[crash_data$Time_weight == 1,])
nrow(crash_data[crash_data$Time_weight == 2,])
nrow(crash_data[crash_data$Time_weight == 3,])

#going to assign time_weight of 1 for NA rows as well

crash_data$Time_weight[is.na(crash_data$Time_weight)] = 1 #911

unique(crash_data$Severity_Weight)

nrow(crash_data[crash_data$Severity_Weight == 1,])
nrow(crash_data[crash_data$Severity_Weight == 2,])
nrow(crash_data[crash_data$Severity_Weight == 3,])

crash_data$Severity_Weight[is.na(crash_data$Severity_Weight)] = 1 #911

write.csv(crash_data, "data/good_crash_records.csv")

crash_data$Vehicle_ID_Number[1:3]
crash_data[(nchar(crash_data$Vehicle_ID_Number) == 17), "Vehicle_ID_Number"]
crash_data[(nchar(crash_data$Vehicle_ID_Number) != 17), "Vehicle_ID_Number"]

make_codes_found = unique(substring(crash_data[(nchar(crash_data$Vehicle_ID_Number) > 3), "Vehicle_ID_Number"], 1,3))
bad_make_codes_found = unique(crash_data[(nchar(crash_data$Vehicle_ID_Number) <= 3), "Vehicle_ID_Number"])

MakeDetails_tbl = data.frame( "Code" = character(), "CommonName" = character(), "Make" = character(),
                              "ManufacturerName" = character(), "ParentCompanyName" = character(), "URL" = character(),
                              "VehicleType" = character(), stringsAsFactors=FALSE)

for (i in 1:length(make_codes_found)) {
  result = GetMakeDetails(make_codes_found[i])
  MakeDetails_tbl[nrow(MakeDetails_tbl) + 1, ] = c(make_codes_found[i], result[1], result[2], result[3], result[4], result[5], result[6])
  Sys.sleep(5)
  print(c("Going for", i))
  print(result)
}

write.csv(MakeDetails_tbl, "data/make_data_records.csv")

insp_make_codes_found = unique(substring(inspection_data[(nchar(inspection_data$VIN) > 3), "VIN"], 1,3))
insp_bad_make_codes_found = unique(inspection_data[(nchar(inspection_data$VIN) <= 3), "VIN"])

insp_make_codes_found[insp_make_codes_found %in% make_codes_found]
insp_make_codes_found = insp_make_codes_found[!(insp_make_codes_found %in% make_codes_found)]

MakeDetails_tbl2 = data.frame( "Code" = character(), "CommonName" = character(), "Make" = character(),
                              "ManufacturerName" = character(), "ParentCompanyName" = character(), "URL" = character(),
                              "VehicleType" = character(), stringsAsFactors=FALSE)

for (i in 1:length(insp_make_codes_found)) {
  result = GetMakeDetails(insp_make_codes_found[i])
  MakeDetails_tbl2[nrow(MakeDetails_tbl2) + 1, ] = c(insp_make_codes_found[i], result[1], result[2], result[3], result[4], result[5], result[6])
  Sys.sleep(5)
  print(c("Going for", i))
  print(result)
}

write.csv(MakeDetails_tbl2, "data/make_data2_records.csv")


write.csv(MakeCode_Table, "data/all_make_data_records.csv")

MakeCode_Table=read.csv("data/all_make_data_records.csv",  header=T, sep = ",", stringsAsFactors=FALSE)
