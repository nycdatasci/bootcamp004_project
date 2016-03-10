library(magrittr)
consumption = read.csv(file.choose(), header = TRUE)
consumption2 = group_by(consumption, Commodity_Description, Market_Year)
consumption2 = spread(consumption, Attribute_Description, Value, fill=0)
consumption2 = select(consumption2, -10, -12)
consumption2 = select(consumption2, -Attribute_ID, -Calendar_Year, -Country_Code)

Exp = select(consumption2, -5, -7, -8)
Imp = select(consumption2, -5, -6, -8)
Prod = select(consumption2, -5, -6, -7)
cons = select(consumption2, -6, -7, -8)


Exp %<>% filter(Exports != 0)
Imp %<>% filter(Imports != 0)
Prod %<>% filter(Production != 0)
cons$Domestic_Consumption = cons$`Domestic Consumption`
cons = cons[,-5]
Cons = cons[which(cons$Domestic_Consumption != 0),]


a1 = left_join(Exp, Imp, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year"))
a2 = left_join(Prod, Cons, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year"))
a1 %<>% select(-Unit_Description.y)
a2 %<>% select(-Unit_Description.y)
Df = left_join(a1, a2, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year"))

Df %<>% select(-Unit_Description.x.y)
Df = select(Df, c(-3, -4, -6, -8, -10))

colnames(Df) = c("Commodity_Description", "Market_Year", "Exports", "Imports", "Production", "Consumption")
Df[is.na(Df)] <- 0

############ Fixing Df for Palm Oil, Palm Kernel, and Coconut Oil ############

Coco = filter(consumption, Commodity_Description == "Oil, Coconut")
CocoP = select(Coco, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Coconut")%>%
  filter(Attribute_Description == "Production")%>%
  arrange(Market_Year)

CocoC = select(Coco, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Coconut")%>%
  filter(Attribute_Description == "Domestic Consumption")%>%
  arrange(Market_Year)

Palm_K = filter(consumption, Commodity_Description == "Oil, Palm Kernel")
Palm_KP = select(Palm_K, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Palm Kernel")%>%
  filter(Attribute_Description == "Production")
Palm_KP = filter(Palm_KP, Market_Year >1987)%>%
  arrange(Market_Year)

Palm_KC = select(Palm_K, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Palm Kernel")%>%
  filter(Attribute_Description == "Domestic Consumption")
Palm_KC = filter(Palm_KC, Market_Year >1987)%>%
  arrange(Market_Year)

Palm = filter(consumption, Commodity_Description == "Oil, Palm")
PalmP = select(Palm, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Palm")%>%
  filter(Attribute_Description == "Production")
PalmP = filter(PalmP, Market_Year >1987)%>%
  arrange(Market_Year)

PalmC = select(Palm, Market_Year, Commodity_Description, Attribute_Description, Value)%>%
  filter(Commodity_Description == "Oil, Palm")%>%
  filter(Attribute_Description == "Domestic Consumption")
PalmC = filter(PalmC, Market_Year >1987)%>%
  arrange(Market_Year)


######### Subsetting Df for Palm Oil, Coconut Oil, and Palm Kernel Oil ###########
Coconut = filter(Df, Commodity_Description == "Oil, Coconut")
Palm_Oil = filter(Df, Commodity_Description == "Oil, Palm")
Palm_Kernel = filter(Df, Commodity_Description == "Oil, Palm Kernel")

Palm_Oil$Production = PalmP$Value
Palm_Oil$Consumption = PalmC$Value
Palm_Kernel$Production = Palm_KP$Value
Palm_Kernel$Consumption = Palm_KC$Value
Coconut$Production = CocoP$Value
Coconut$Consumption = CocoC$Value

DF = filter(Df, Commodity_Description != "Oil, Coconut")%>%
  filter(Commodity_Description != "Oil, Palm")%>%
  filter(Commodity_Description != "Oil, Palm Kernel")

dataframe = rbind(Coconut, Palm_Oil, Palm_Kernel, DF)

###################
dataframe = mutate(dataframe, "Trade_Balance" = Exports - Imports, 
                   "Production/Exports" = Production/Exports,
                   "Imports/Consumption" = 100*Imports/Consumption)

df = dataframe
namesdf = c(colnames(df))
names(df)[8] = "Production_Export_Ratio"
names(df)[9] = "Import_Consumption_Ratio"
saveRDS(df, "data_clean.rds")


