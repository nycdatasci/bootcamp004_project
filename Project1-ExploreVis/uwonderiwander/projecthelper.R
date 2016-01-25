zip_state_map = function(dataframe, state, palettevalue = 1, legend = "Truckers", title = "Truck Driver Population") { 

  p = ZipChoropleth$new(dataframe)
  p$title = title
  p$ggplot_scale = scale_fill_brewer(legend, palette=palettevalue, drop=FALSE)
  p$set_zoom_zip(state_zoom=state, county_zoom=NULL, msa_zoom=NULL, zip_zoom=NULL)
  p$render()
}

clean_data = function(fleets) {
# str(fleets)
# 
# UniquePhyscialCountry = levels(fleets$PHY_COUNTRY)
# UniquePhyscialStates = levels(fleets$PHY_STATE)
# UniqueMailingStates = levels(fleets$MAILING_STATE)
# UniqueMailingCountry = levels(fleets$MAILING_COUNTRY)
# 
# 
# 
# MissingValueSummary = sapply(fleets, function(x) sum(is.na(x)))
# 
# #fleets with DBA_NAME NA
# fleets[which(is.na(fleets[,"DBA_NAME"])), "PHY_COUNTRY" ]
# fleets[which(is.na(fleets[,"DBA_NAME"])), match("PHY_STATE", names(fleets)):match("PHY_COUNTRY", names(fleets))]
# fleets[which(is.na(fleets[,"DBA_NAME"])), c("LEGAL_NAME","PHY_STATE","PHY_COUNTRY")]
# 
# fleets[ !fleets$PHY_COUNTRY == "US" ,]

#fleet_table = data.table(fleets)
#fleet_table[, sum(DOT_NUMBER), by = PHY_COUNTRY]

# l = split(fleets$DOT_NUMBER, fleets$PHY_STATE)
# ms = table(sort(fleets$MAILING_STATE))
# ps = table(sort(fleets$PHY_STATE))
# missing_ps = setdiff(statecodes$ABBR, names(ps))
# missing_ms = setdiff(statecodes$ABBR, names(ms))
# 
# meta_missing_ps =statecodes$ABBR[!statecodes$ABBR %in% names(ps)]
# meta_missing_ms = statecodes$ABBR[!statecodes$ABBR %in% names(ms)]
# 
# missing_state_ms = fleets[fleets$MAILING_STATE %in% missing_ms,]
# missing_state_ps = fleets[fleets$PHY_STATE %in% missing_ps,]
# 
# na_state_ms = fleets[is.na(fleets$MAILING_STATE),]
# na_state_ps = fleets[is.na(fleets$PHY_STATE),]

#decided to restrict to just US instead of North Amreican countries for the first cut
#valid_country_codes = c("US", "CA", "MX")

#Decided to eliminate every country except for the North American Countries after initial analysis of the data
not_considered_fleets = fleets[!fleets$PHY_COUNTRY == 'US',] 
nrow(not_considered_fleets) #36837

not_considered_fleets2 =  fleets[!fleets$MAILING_COUNTRY == 'US',]
nrow(not_considered_fleets2) #26394

fleets =  fleets[fleets$PHY_COUNTRY == 'US',] 
nrow(fleets) #1606536

fleets =  fleets[fleets$MAILING_COUNTRY == 'US',]
nrow(fleets) #1606379

fleets$PHY_ZIP = clean.zipcodes(fleets$PHY_ZIP)

not_considered_fleets3 = fleets[is.na(fleets$PHY_ZIP),]
nrow(not_considered_fleets3) # 26
fleets =  fleets[!is.na(fleets$PHY_ZIP),]
nrow(fleets) #1606353


not_considered_fleets4 = fleets[is.na(fleets$MAILING_ZIP),]
nrow(not_considered_fleets4) # 0
fleets =  fleets[!is.na(fleets$MAILING_ZIP),]
nrow(fleets) #1606353

not_considered_fleets5 = fleets[!(fleets$PHY_STATE %in% state.abb),]
nrow(not_considered_fleets5) # 3347
fleets =  fleets[fleets$PHY_STATE %in% state.abb,]
nrow(fleets) #1603006

not_considered_fleets6 = fleets[!(fleets$MAILING_STATE %in% state.abb),]
nrow(not_considered_fleets6) # 3347
fleets =  fleets[fleets$MAILING_STATE %in% state.abb,]
nrow(fleets) #1442255

not_considered_fleets7 = fleets[is.na(fleets$NBR_POWER_UNIT),]
nrow(not_considered_fleets7) # 36220
fleets =  fleets[!is.na(fleets$NBR_POWER_UNIT),]
nrow(fleets) #1406035

not_considered_fleets8 = fleets[is.na(fleets$DRIVER_TOTAL),]
nrow(not_considered_fleets8) # 11979
fleets =  fleets[!is.na(fleets$DRIVER_TOTAL),]
nrow(fleets) #1394056

not_considered_fleets9 = fleets[!(fleets$OIC_STATE %in% state.abb),]
nrow(not_considered_fleets9) # 1
fleets =  fleets[fleets$OIC_STATE %in% state.abb,]
nrow(fleets) #1394055


unique(fleets$CARRIER_OPERATION)
unique(fleets$HM_FLAG)
unique(fleets$PC_FLAG)

fleets[is.na(fleets$CARRIER_OPERATION),]
fleets[is.na(fleets$HM_FLAG),]
fleets[is.na(fleets$PC_FLAG),]

return(fleets)
}

