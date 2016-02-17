consumption = read.csv(file.choose(), header = TRUE)

consumption2 = group_by(consumption, Commodity_Description, Market_Year)
consumption2 = spread(consumption, Attribute_Description, Value, fill=0)
consumption2 = select(consumption2, -10, -12)
consumption2 = select(consumption2, -Attribute_ID, -Calendar_Year, -Country_Code)

Exp = select(consumption2, -5, -7, -8)
Imp = select(consumption2, -5, -6, -8)
Prod = select(consumption2, -5, -6, -7)
cons = select(consumption2, -6, -7, -8)

library(magrittr)
Exp %<>% filter(Exports != 0)
Imp %<>% filter(Imports != 0)
Prod %<>% filter(Production != 0)
cons$Domestic_Consumption = cons$`Domestic Consumption`
cons = cons[,-5]
Cons = cons[which(cons$Domestic_Consumption != 0),]
# Cons = dplyr::select(cons, Domestic_Consumption)
View(Cons)

library(mg)
a1 = left_join(Exp, Imp, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year", 
                                "Month" = "Month"))

a2 = left_join(Prod, Cons, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year", 
                                  "Month" = "Month"))
a1 %<>% select(-Unit_Description.y)
a2 %<>% select(-Unit_Description.y)
Df = left_join(a1, a2, by = c("Commodity_Description" = "Commodity_Description", "Market_Year" = "Market_Year", 
                              "Month" = "Month"))

Df %<>% select(-Unit_Description.x.y)
colnames(Df) = c("Commodity_Description", "Market_Year", "Month", "Unit", "Exports", "Imports", "Production", "Consumption")



Df[is.na(Df)] <- 0
Df = mutate(Df, "Production/Consumption" = 100*Production/Consumption) %>%
  mutate("Exports/Production" = 100*Exports/Production) %>%
  mutate("Imports/Consumption" = 100*Imports/Consumption)%>%
  mutate("Trade_Balance" = Exports - Imports)
df = select(Df, -3)
df[is.na(df)] <- 0




save(df, "data_clean.rds")

