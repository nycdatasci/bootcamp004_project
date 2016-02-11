library(dplyr)
library(leaflet)
library(maps)
library(ggplot2)
library(reshape2)
library(zipcode)
library(choroplethr)
library(RColorBrewer)
library(devtools)
library(choroplethrZip)

####################################
##### Reading/working directory
#edufull = read.csv('/Users/boulenge/Desktop/Projects/Project2 - Shiny/Shiny-TomB/edufull.csv')
#setwd('/Users/boulenge/Desktop/Projects/Project2 - Shiny/Shiny-TomB')


####################################
##### variable information
var.info = c('CITY', 'STABBR', 'ZIP', 'LONGITUDE', 'LATITUDE',
             "Institution Name" = 'INSTNM',
             "Net tuition revenue per full-time equivalent student" = 'TUITFTE',
             "Instructional expenditures divided by the number of FTE students" = 'INEXPFTE',
             "Average faculty salary per month" = 'AVGFACSAL',
             "Average incomes for families" = 'faminc',
             "Average annual cost of attendance for academic year institutions" = 'COSTT4_A',
             "Average annual cost of attendance for prgram year institutions" = 'COSTT4_P',
             "Tuition for in-state students" = 'TUITIONFEE_IN',
             "Tuition for out-of-state students" = 'TUITIONFEE_OUT',
             "Tuititon for program-year institutions" = 'TUITIONFEE_PROG',
             "Net total price for the public" = 'NPT4_PUB',
             "Net total price for the private" = 'NPT4_PRIV',
             "Highest award level offered at the school" = 'HIGHDEG',
             "Type of degree that the school primarily awards" = 'PREDDEG',
             "Predominant degree (0=N/A, 1=certificate..4=Graduate)" = 'sch_deg',
             "Institution’s governance structure (public, private nonprofit, or private for-profit)" = 'CONTROL',
             "Religious Affiliation" = 'RELAFFIL', 
             "Historically Black Colleges and Universities" = 'HBCU',
             "Predominantly Black Institutions" = 'PBI',
             "Alaska Native-/Native Hawaiian-serving Institutions" = 'ANNHI',
             "Tribal Colleges and Universities" = 'TRIBAL',
             "Asian American-/Native American-Pacific Islander-serving Institutions" = 'AANAPII',
             "Hispanic-serving Institutions" = 'HSI',
             "Native American Non-Tribal Institutions" = 'NANTI',
             "Admissions over all campuses" = 'ADM_RATE_ALL',
             "Admissions for each campuses for institutions with branches" = 'ADM_RATE',
             "75th percentile for Math SAT" = 'SATMT75',
             "75th percentile for Math ACT" = 'ACTMT75',
             "Number of students enrolled in Fall" = 'UGDS',
             "Ratio of female students" = 'female',
             "Ratio of married students" =  'married',
             "Percentage of White students" = 'pct_white', 
             "Percentage of Black students" = 'pct_black', 
             "Percentage of Asian students" = 'pct_asian',
             "Percentage of Hispanic students" = 'pct_hispanic', 
             "Percentage of US born students" = 'pct_born_us', 
             "Poverty rate" = 'poverty_rate',
             "Median loan debt" = 'DEBT_MDN',
             "Median loan debt (completed students)" = 'GRAD_DEBT_MDN', 
             "Median loan debt (withdrawn students)" = "WDRAW_DEBT_MDN",
             "3-year cohort default rate" = 'CDR3')


# State Averages Map variables
varM <- c("Average Family Income" = "faminc", 
          "Average Tuition" = "TUITFTE",
          "3 year Default Rate" = "CDR3", 
          "Median Loan debt" = "DEBT_MDN",
          "Predominant degree type" = "PREDDEG")
varM2 = c('Avg.Income', 'Avg.Tuition', 
          'Avg.Default.Rate', 'Median.Loan.Debt', 'Avg.Degree.Type')
# Overview map variables
varInt <- c("Institution name" = "INSTNM", 
            "Proportion of Women" = "female",
            "Admission Rate" = "ADM_RATE", 
            "US born students" = "pct_born_us")


####################################
##### States names, associated names
addname = c('American Samoa', 'District of Columbia', 'Micronesia', 'Guam', 'Marshall Islands',
            'Northern Mariana Islands', 'Porto Rico', 'Palau', 'Philippine Islands')
st.name = c(state.name, addname)

# add associated states
select_state = function(x) {
  if(x %in% state.abb) state.name[grep(as.character(x), state.abb)]
  else if (x == 'AS') {
    'American Samoa'
  }
  else if (x == 'DC') {
    'District of Columbia'
  }
  else if (x == 'FM') {
    'Micronesia'
  }
  else if (x == 'GU') {
    'Guam'
  }
  else if (x == 'MH') {
    'Marshall Islands'
  }
  else if (x == 'PR') {
    'Porto Rico'
  }
  else if (x == 'PW') {
    'Palau'
  }
  else if (x == 'VI') {
    'Virgin Islands'
  }
  else if (x == 'MP') {
    'Northern Mariana Islands'
  }
}

############################################
######## 200 sample ########################
# edu200 = data.frame()
# for (abb in unique(edufull$STABBR)) {
#   edu200 = rbind(edu200, 
#                  edufull[sample(which(edufull$STABBR == as.character(abb)), 
#                                 min(round(200/length(unique(edufull$STABBR))), 
#                                     length(which(edufull$STABBR == as.character(abb))))),])
# }

# edu200 = edufull[sample(1:nrow(edufull), 200), ]
# 
# edu200 = edu200[, var.name]
# 
# edu200 = edu200[!is.null(edu200$LONGITUDE) & !is.null(edu200$LATITUDE),]
# edu200 = edu200[!is.na(edu200$LONGITUDE) & !is.na(edu200$LATITUDE),]
# edu200$faminc = as.numeric(as.character(edu200$faminc))
# edu200$CDR3 = as.numeric(as.character(edu200$CDR3))
# edu200$TUITFTE = as.numeric(as.character(edu200$TUITFTE))
# edu200$AVGFACSAL = as.numeric(as.character(edu200$AVGFACSAL))
# edu200$married = as.numeric(as.character(edu200$married))
# edu200$pct_white = as.numeric(as.character(edu200$pct_white))
# edu200$DEBT_MDN = as.numeric(as.character(edu200$DEBT_MDN))
# edu200$PREDDEG = as.numeric(as.character(edu200$PREDDEG))
# edu200$INEXPFTE = as.numeric(as.character(edu200$INEXPFTE))

edu200 = read.csv('edu200.csv')

#########################
#Averaging over States
edu.map200 = dplyr::tbl_df(edu200) %>% dplyr::rename(., abb = STABBR) %>%
  dplyr::group_by(., abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE))
edu.map200 = as.data.frame(edu.map200)
edu.map200$abb = as.character(edu.map200$abb)
edu.map200 = edu.map200[order(edu.map200$abb),]

edu.map200$region = tolower(unlist(sapply(edu.map200$abb, select_state)))

states.map200 = map_data("state")[, c('long', 'lat', 'group', 'region')] %>% 
  left_join(., edu.map200[, c('region', 'faminc', 'TUITFTE', 
                              'CDR3', 'DEBT_MDN', 'PREDDEG')], by = "region")

#########################
#Averaging over Zip codes
edu.zip200 = dplyr::tbl_df(edu200) %>% 
  dplyr::rename(., abb = STABBR, zip = ZIP) %>%
  dplyr::group_by(., zip, abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE)) %>%
  na.omit(.)

edu.zip200$zip = as.character(edu.zip200$zip)

############################################
######## Widgets variables #################
# edu200 base for State Averages variables
varSt = as.character(unique(states.map200$region))
# edu200 base for Overview variables
varCty = as.character(unique(edu200$CITY))
varSt2 = as.character(unique(edu200$STABBR))
# edu200 base for Explorer variables
numVar = c(1:11, 24:39)
groupVar = c(12:23)
temp = edu200[, -c(1:6)]
varT = colnames(temp[,numVar])
varG = colnames(temp[,groupVar])
# states names
ec.st = c("maine", "new hampshire", "massachusetts", "connecticut", 
          "new york", "new jersey", "delaware", "maryland", 
          "pennsylvania", "district of columbia", "vermont")
ec.st.abb = c("ME", "NH", "MA", "CT", "NY", "NJ", "DE", "MD", "PA", "DC", "VT")
#ec.st = ec.st[ec.st %in% states.map$region]
############################################





############################################
######## 500 sample ########################
# edu500 = data.frame()
# for (abb in unique(edufull$STABBR)) {
#   edu500 = rbind(edu500, 
#                  edufull[sample(which(edufull$STABBR == as.character(abb)), 
#                                 min(round(500/length(unique(edufull$STABBR))), 
#                                     length(which(edufull$STABBR == as.character(abb))))),])
# }

# edu500 = edufull[sample(1:nrow(edufull), 500), ]
# 
# edu500 = edu500[, var.name]
# 
# edu500 = edu500[!is.null(edu500$LONGITUDE) & !is.null(edu500$LATITUDE),]
# edu500 = edu500[!is.na(edu500$LONGITUDE) & !is.na(edu500$LATITUDE),]
# edu500$faminc = as.numeric(as.character(edu500$faminc))
# edu500$CDR3 = as.numeric(as.character(edu500$CDR3))
# edu500$TUITFTE = as.numeric(as.character(edu500$TUITFTE))
# edu500$AVGFACSAL = as.numeric(as.character(edu500$AVGFACSAL))
# edu500$married = as.numeric(as.character(edu500$married))
# edu500$pct_white = as.numeric(as.character(edu500$pct_white))
# edu500$DEBT_MDN = as.numeric(as.character(edu500$DEBT_MDN))
# edu500$PREDDEG = as.numeric(as.character(edu500$PREDDEG))
# edu500$INEXPFTE = as.numeric(as.character(edu500$INEXPFTE))

edu500 = read.csv('edu500.csv')

#########################
#Averaging over States
edu.map500 = dplyr::tbl_df(edu500) %>% dplyr::rename(., abb = STABBR) %>%
  dplyr::group_by(., abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE))
edu.map500 = as.data.frame(edu.map500)
edu.map500$abb = as.character(edu.map500$abb)
edu.map500 = edu.map500[order(edu.map500$abb),]

edu.map500$region = tolower(unlist(sapply(edu.map500$abb, select_state)))

states.map500 = map_data("state")[, c('long', 'lat', 'group', 'region')] %>% 
  left_join(., edu.map500[, c('region', 'faminc', 'TUITFTE', 
                              'CDR3', 'DEBT_MDN', 'PREDDEG')], by = "region")

#########################
#Averaging over Zip codes
edu.zip500 = dplyr::tbl_df(edu500) %>% 
  dplyr::rename(., abb = STABBR, zip = ZIP) %>%
  dplyr::group_by(., zip, abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE)) %>%
  na.omit(.)

edu.zip500$zip = as.character(edu.zip500$zip)





############################################
######## 1000 sample ########################
# edu1000 = data.frame()
# for (abb in unique(edufull$STABBR)) {
#   edu1000 = rbind(edu1000, 
#                  edufull[sample(which(edufull$STABBR == as.character(abb)), 
#                                 min(round(1000/length(unique(edufull$STABBR))), 
#                                     length(which(edufull$STABBR == as.character(abb))))),])
# }

# edu1000 = edufull[sample(1:nrow(edufull), 1000), ]
# 
# edu1000 = edu1000[, var.name]
# 
# edu1000 = edu1000[!is.null(edu1000$LONGITUDE) & !is.null(edu1000$LATITUDE),]
# edu1000 = edu1000[!is.na(edu1000$LONGITUDE) & !is.na(edu1000$LATITUDE),]
# edu1000$faminc = as.numeric(as.character(edu1000$faminc))
# edu1000$CDR3 = as.numeric(as.character(edu1000$CDR3))
# edu1000$TUITFTE = as.numeric(as.character(edu1000$TUITFTE))
# edu1000$AVGFACSAL = as.numeric(as.character(edu1000$AVGFACSAL))
# edu1000$married = as.numeric(as.character(edu1000$married))
# edu1000$pct_white = as.numeric(as.character(edu1000$pct_white))
# edu1000$DEBT_MDN = as.numeric(as.character(edu1000$DEBT_MDN))
# edu1000$PREDDEG = as.numeric(as.character(edu1000$PREDDEG))
# edu1000$INEXPFTE = as.numeric(as.character(edu1000$INEXPFTE))

edu1000 = read.csv('edu1000.csv')

#########################
#Averaging over States
edu.map1000 = dplyr::tbl_df(edu1000) %>% dplyr::rename(., abb = STABBR) %>%
  dplyr::group_by(., abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE))
edu.map1000 = as.data.frame(edu.map1000)
edu.map1000$abb = as.character(edu.map1000$abb)
edu.map1000 = edu.map1000[order(edu.map1000$abb),]

edu.map1000$region = tolower(unlist(sapply(edu.map1000$abb, select_state)))

states.map1000 = map_data("state")[, c('long', 'lat', 'group', 'region')] %>% 
  left_join(., edu.map1000[, c('region', 'faminc', 'TUITFTE', 
                               'CDR3', 'DEBT_MDN', 'PREDDEG')], by = "region")

#########################
#Averaging over Zip codes
edu.zip1000 = dplyr::tbl_df(edu1000) %>% 
  dplyr::rename(., abb = STABBR, zip = ZIP) %>%
  dplyr::group_by(., zip, abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE)) %>%
  na.omit(.)

edu.zip1000$zip = as.character(edu.zip1000$zip)





############################################
######## Full sample #######################
# edufull_red = data.frame()
# for (abb in unique(edufull$STABBR)) {
#   edufull_red = rbind(edufull_red, 
#                  edufull[sample(which(edufull$STABBR == as.character(abb)), 
#                                 min(round(nrow(edufull)/length(unique(edufull$STABBR))), 
#                                     length(which(edufull$STABBR == as.character(abb))))),])
# }

# edufull_red = edufull
# 
# edufull_red = edufull_red[, var.name]
# 
# edufull_red = edufull_red[!is.null(edufull_red$LONGITUDE) & !is.null(edufull_red$LATITUDE),]
# edufull_red = edufull_red[!is.na(edufull_red$LONGITUDE) & !is.na(edufull_red$LATITUDE),]
# edufull_red$faminc = as.numeric(as.character(edufull_red$faminc))
# edufull_red$CDR3 = as.numeric(as.character(edufull_red$CDR3))
# edufull_red$TUITFTE = as.numeric(as.character(edufull_red$TUITFTE))
# edufull_red$AVGFACSAL = as.numeric(as.character(edufull_red$AVGFACSAL))
# edufull_red$married = as.numeric(as.character(edufull_red$married))
# edufull_red$pct_white = as.numeric(as.character(edufull_red$pct_white))
# edufull_red$DEBT_MDN = as.numeric(as.character(edufull_red$DEBT_MDN))
# edufull_red$PREDDEG = as.numeric(as.character(edufull_red$PREDDEG))
# edufull_red$INEXPFTE = as.numeric(as.character(edufull_red$INEXPFTE))

edufull_red = read.csv('edufull_red.csv')

#########################
#Averaging over States
edu.mapfull_red = dplyr::tbl_df(edufull_red) %>% dplyr::rename(., abb = STABBR) %>%
  dplyr::group_by(., abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE))
edu.mapfull_red = as.data.frame(edu.mapfull_red)
edu.mapfull_red$abb = as.character(edu.mapfull_red$abb)
edu.mapfull_red = edu.mapfull_red[order(edu.mapfull_red$abb),]

edu.mapfull_red$region = tolower(unlist(sapply(edu.mapfull_red$abb, select_state)))

states.mapfull_red = map_data("state")[, c('long', 'lat', 'group', 'region')] %>% 
  left_join(., edu.mapfull_red[, c('region', 'faminc', 'TUITFTE', 
                                   'CDR3', 'DEBT_MDN', 'PREDDEG')], by = "region")

#########################
#Averaging over Zip codes
edu.zipfull_red = dplyr::tbl_df(edufull_red) %>% 
  dplyr::rename(., abb = STABBR, zip = ZIP) %>%
  dplyr::group_by(., zip, abb) %>%
  dplyr::summarise(., faminc = mean(faminc, na.rm = TRUE),
                   TUITFTE = mean(TUITFTE, na.rm = TRUE),
                   CDR3 = mean(CDR3, na.rm = TRUE),
                   DEBT_MDN = mean(DEBT_MDN, na.rm = TRUE),
                   PREDDEG = mean(PREDDEG, na.rm = TRUE),
                   long = mean(LONGITUDE, na.rm = TRUE),
                   lat = mean(LATITUDE, na.rm = TRUE)) %>%
  na.omit(.)

edu.zipfull_red$zip = as.character(edu.zipfull_red$zip)

############################################
######## writing files #####################
# write.csv(edu200, file = 'edu200.csv')
# write.csv(edu500, file = 'edu500.csv')
# write.csv(edu1000, file = 'edu1000.csv')
# write.csv(edufull_red, file = 'edufull_red.csv')
