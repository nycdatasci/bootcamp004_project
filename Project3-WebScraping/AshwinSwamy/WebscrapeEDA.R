#reading CSV files from Python Webscraping
install.packages("dplyr")
install.packages("tidyr")
install.packages("dygraphs")
install.packages("xts")
install.packages("googleVis")
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(googleVis)
library(corrplot)

Pork = read.csv("CPI_Pork2", header=TRUE)
Beef = read.csv("CPI_beef2", header=TRUE)
Coco = read.csv("CPI_Coco2", header=TRUE)
Palm = read.csv("CPI_Palm2", header=TRUE)
colnames(UNdf)


Pork = select(Pork, -1)
Beef = select(Beef, -1)
Coco = select(Coco, -1)
Palm = select(Palm, -1)


colnames(Palm) <- c("Price", "Change", "Month", "Year")
colnames(Beef) <- c("Change", "Price", "Month", "Year")
colnames(Pork) <- c("Change", "Price", "Month", "Year")
colnames(Coco) <- c("Price", "Change", "Month", "Year")

#Need to convert dataframe into proper date format...

#For Pork
Pork2 = c(paste("-", Pork$Year, sep= ""))
Pork$Year = Pork2
Pork3 = c(paste("01-", Pork$Month, sep= ""))
Pork$Month = Pork3
Pork = unite(Pork, Year, Month, Year, sep = "", remove = TRUE)
Pork$Year = as.Date(Pork$Year, format = "%d-%b-%Y")
class(Pork$Year)

#For Beef
Beef2 = c(paste("-", Beef$Year, sep= ""))
Beef$Year = Beef2
Beef3 = c(paste("01-", Beef$Month, sep= ""))
Beef$Month = Beef3
Beef = unite(Beef, Year, Month, Year, sep = "", remove = TRUE)
Beef$Year = as.Date(Beef$Year, format = "%d-%b-%Y")
class(Beef$Year)

#For Palm
Palm2 = c(paste("-", Palm$Year, sep= ""))
Palm$Year = Palm2
Palm3 = c(paste("01-", Palm$Month, sep= ""))
Palm$Month = Palm3
Palm = unite(Palm, Year, Month, Year, sep = "", remove = TRUE)
Palm$Year = as.Date(Palm$Year, format = "%d-%b-%Y")
class(Palm$Year)

#For Coconut
Coco2 = c(paste("-", Coco$Year, sep= ""))
Coco$Year = Coco2
Coco3 = c(paste("01-", Coco$Month, sep= ""))
Coco$Month = Coco3
Coco = unite(Coco, Year, Month, Year, sep = "", remove = TRUE)
Coco$Year = as.Date(Coco$Year, format = "%d-%b-%Y")
class(Coco$Year)

Coco = Coco[c(3,1,2)]
Beef = Beef[c(3,2,1)]
Palm = Palm[c(3,1,2)]
Pork = Pork[c(3,2,1)]

#Creating Time Series Objects
Cocoxts = xts(Coco[,-1],order.by=as.POSIXct(Coco$Year))
Beefxts = xts(Beef[,-1],order.by=as.POSIXct(Beef$Year))
Palmxts = xts(Palm[,-1],order.by=as.POSIXct(Palm$Year))
Porkxts = xts(Pork[,-1],order.by=as.POSIXct(Pork$Year))

#Dygraphs
Cocody= dygraph(Cocoxts, main = "Coconut Oil Monthly Price (USD/Metric Ton)") %>%
  dyRangeSelector()

Beefdy= dygraph(Beefxts, main = "Beef Monthly Price (US cents/pound)") %>%
  dyRangeSelector()

Palmdy= dygraph(Palmxts, main = "Palm Oil Monthly Price (USD/Metric Ton)") %>%
  dyRangeSelector()

Porkdy= dygraph(Porkxts, main = "Swine Monthly Price (US cents/pound)") %>%
  dyRangeSelector()

#Plotting on Same Graph
Meatxts <- cbind(Porkxts, Beefxts)
colnames(Meatxts) <- c("Pork_Price", "Pork_Change", "Beef_Price", "Beef_Change")
Meatdy <- dygraph(Meatxts, main = "Beef and Swine Monthly Prices (US cents/pound)")%>%
  dyRangeSelector()


Oilxts <- cbind(Palmxts, Cocoxts)
colnames(Oilxts) <- c("Palm_Price", "Palm_Change", "Coconut_Price", "Coconut_Change")
Oildy <- dygraph(Oilxts, main = "Palm Oil and Olive Oil Monthly Prices (US cents/pound)")%>%
  dyRangeSelector()


#Calling Dataframe from UN
UNdf = read.csv("data_all.csv", header=TRUE)

#Lots of cleaning to do...
UN = select(UNdf, Year, Trade.Flow, Reporter, Partner, Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Netweight..kg., Trade.Value..US..)

#Definitions for "Reporter" and "Partner" depend on whether it's an import or export, according to worldbank website:
#http://wits.worldbank.org/glossary.html

UN.Import = filter(UN, Trade.Flow == "Import")%>%
  rename(Origin=Partner, Destination=Reporter)%>%
  rename(Netweight=Netweight..kg., Import.Value = Trade.Value..US..)
UN.Export = filter(UN, Trade.Flow == "Export" | Trade.Flow == "Re-Export")%>%
  rename(Destination=Partner, Origin=Reporter)%>%
  rename(Netweight=Netweight..kg., Export.Value = Trade.Value..US..)

UN.Import = UN.Import[,c(1,2,4,3,5:10)]
View(UN.Import)

UN.Import$Qty.Unit="MT"
UN.Export$Qty.Unit="MT"
UN.Import = mutate(UN.Import, Netweight = Netweight*.001)
UN.Export = mutate(UN.Export, Netweight = Netweight*.001)

#Create 10 Separate DataFrames for Pork, Beef, Coconut Oil, Palm Oil
#Pull out aggregate rows for Pork, Beef, Coconut Oil, Palm Oil
#KEY: Import.Coco.Partner = Coco imports with country partners
#KEY: Import.Coco.Total = Aggregate imports with country partners

#Import Partners By Country - for Google Viz
Import.Coco.Partner = filter(UN.Import, Commodity == "Coconut (copra) oil", Year > 1985, Origin != "World")
Import.Palm.Partner = filter(UN.Import, Commodity == "Palm oil", Year > 1985, Origin != "World")

Import.Pork.Partner = filter(UN.Import, Commodity == "Pig meat fresh, chilled or frozen", Year > 1985, Origin != "World")
Import.Beef.Partner = filter(UN.Import, Commodity == "Bovine meat, fresh, chilled or frozen", Year > 1985, Origin != "World")

#Export Partners By Country - for Google Viz
Export.Coco.Partner = filter(UN.Export, Commodity == "Coconut (copra) oil", Year > 1985, Destination != "World")
Export.Palm.Partner = filter(UN.Export, Commodity == "Palm oil", Year > 1985, Destination != "World")
  
Export.Pork.Partner = filter(UN.Export, Commodity == "Pig meat fresh, chilled or frozen", Year > 1985, Destination != "World")
Export.Beef.Partner = filter(UN.Export, Commodity == "Bovine meat, fresh, chilled or frozen", Year > 1985, Destination != "World")

########### Aggregate Values - for Corr Plot and Time Series Plots ##########
####### Warning...added "Destination != "World" arguments above after writing below arguments...will edit
Export.Coco.Total = filter(Export.Coco.Partner, Destination == "World")
Import.Coco.Total = filter(Import.Coco.Partner, Origin == "World")

Export.Palm.Total = filter(Export.Palm.Partner, Destination == "World")
Import.Palm.Total = filter(Import.Palm.Partner, Origin == "World")

#Aggregate Values - for Corr Plot and Time Series Plots

Export.Pork.Total = filter(Export.Pork.Partner, Destination == "World")
Import.Pork.Total = filter(Import.Pork.Partner, Origin == "World")
  
Export.Beef.Total = filter(Export.Beef.Partner, Destination == "World")
Import.Beef.Total = filter(Import.Beef.Partner, Origin == "World")
  
############################ GOOGLE VIZ ############################
#Step 1 - join price data frames back in!
#Step 2 - make the Gvizes Motion Charts
View(Import.Coco.Partner)
View(Coco)

#Need to undo time series changes to df...woops
PorkViz = read.csv("CPI_Pork2", header=TRUE)
BeefViz = read.csv("CPI_beef2", header=TRUE)
CocoViz = read.csv("CPI_Coco2", header=TRUE)
PalmViz = read.csv("CPI_Palm2", header=TRUE)

PorkViz = select(PorkViz, -1)
BeefViz = select(BeefViz, -1)
CocoViz = select(CocoViz, -1)
PalmViz = select(PalmViz, -1)

colnames(PalmViz) <- c("Price", "Change", "Month", "Year")
colnames(BeefViz) <- c("Change", "Price", "Month", "Year")
colnames(PorkViz) <- c("Change", "Price", "Month", "Year")
colnames(CocoViz) <- c("Price", "Change", "Month", "Year")
View(PalmViz)

PalmViz = filter(PalmViz, Month=="Jan")%>%
  select(-Month)
BeefViz = filter(BeefViz, Month=="Jan")%>%
  select(-Month)
PorkViz = filter(PorkViz, Month=="Jan")%>%
  select(-Month)
CocoViz = filter(CocoViz, Month=="Jan")%>%
  select(-Month)


PalmViz = select(PalmViz, -Change)
BeefViz = select(BeefViz, -Change)
PorkViz = select(PorkViz, -Change)
CocoViz = select(CocoViz, -Change)


#Joining with datasets
Import.Coco.Partner2 = inner_join(Import.Coco.Partner, CocoViz)%>%
  filter(Origin!="USA")%>%
  select(Netweight, Import.Value, Price, Origin, Year)

Import.Palm.Partner2 = inner_join(Import.Palm.Partner, PalmViz)%>%
  filter(Origin!="USA")%>%
  select(Netweight, Import.Value, Price, Origin, Year)

Import.Beef.Partner2 = inner_join(Import.Beef.Partner, BeefViz)%>%
  filter(Origin!="USA")%>%
  select(Netweight, Import.Value, Price, Origin, Year)

Import.Pork.Partner2 = inner_join(Import.Pork.Partner, PorkViz)%>%
  filter(Origin!="USA")%>%
  select(Netweight, Import.Value, Price, Origin, Year)

Export.Coco.Partner2 = inner_join(Export.Coco.Partner, CocoViz)%>%
  filter(Destination!="USA", Trade.Flow != "Re-Export")%>%
  select(Netweight, Export.Value, Price, Destination, Year)

Export.Palm.Partner2 = inner_join(Export.Palm.Partner, PalmViz)%>%
  filter(Destination!="USA", Trade.Flow != "Re-Export")%>%
  select(Netweight, Export.Value, Price, Destination, Year)
View(Export.Palm.Partner2)

Export.Beef.Partner2 = inner_join(Export.Beef.Partner, BeefViz)%>%
  filter(Destination!="USA", Trade.Flow != "Re-Export")%>%
  select(Netweight, Export.Value, Price, Destination, Year)
  
Export.Pork.Partner2 = inner_join(Export.Pork.Partner, PorkViz)%>%
  filter(Destination!="USA", Trade.Flow != "Re-Export")%>%
  select(Netweight, Export.Value, Price, Destination, Year)
  


View(Import.Coco.Partner)
View(Export.Coco.Partner)
#Create GoogleViz Motion Charts

Import.Coco.Gviz <- gvisMotionChart(
  data = Import.Coco.Partner2, 
  idvar = "Origin", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Import.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Import.Coco.Gviz)


Import.Palm.Gviz <- gvisMotionChart(
  data = Import.Palm.Partner2, 
  idvar = "Origin", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Import.Value",
  colorvar = "Price",
  sizevar = "Netweight",
  options = list(width = 850, showChartButtons=TRUE))
plot(Import.Palm.Gviz)
View(Import.Palm.Partner2)
  
Import.Beef.Gviz <- gvisMotionChart(
  data = Import.Beef.Partner2, 
  idvar = "Origin", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Import.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Import.Beef.Gviz)
  
Import.Pork.Gviz <- gvisMotionChart(
  data = Import.Pork.Partner2, 
  idvar = "Origin", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Import.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Import.Pork.Gviz)
View(Import.Pork.Partner2)
  

##########################################

Export.Palm.Gviz <- gvisMotionChart(
  data = Export.Palm.Partner2, 
  idvar = "Destination", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Export.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Export.Palm.Gviz)

Export.Beef.Gviz <- gvisMotionChart(
  data = Export.Beef.Partner2, 
  idvar = "Destination", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Export.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Export.Beef.Gviz)


#This one works fine
Export.Pork.Gviz <- gvisMotionChart(
  data = Export.Pork.Partner2, 
  idvar = "Destination", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Export.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Export.Pork.Gviz)

Export.Coco.Gviz <- gvisMotionChart(
  data = Export.Coco.Partner2, 
  idvar = "Destination", 
  timevar = "Year",
  xvar = "Netweight", 
  yvar = "Export.Value",
  colorvar = "Price",
  sizevar = "Price",
  options = list(width = 850, showChartButtons=TRUE))
plot(Export.Coco.Gviz)
 


########################### Time Series ##########################
#Step 1 - join with price data frames
#Export.Coco.Total = filter(Export.Coco.Partner, Destination == "World")
Export.Coco.Total = select(Export.Coco.Total, Year, Netweight, Export.Value)
Export.Coco.Total$Year = as.Date(as.character(Export.Coco.Total$Year), "%Y")
View(Import.Coco.Total)

#Import.Coco.Total = filter(Import.Coco.Partner, Origin == "World")
Import.Coco.Total = select(Import.Coco.Total, Year, Netweight, Import.Value)
Import.Coco.Total$Year = as.Date(as.character(Import.Coco.Total$Year), "%Y")

#Export.Palm.Total = filter(Export.Palm.Partner, Destination == "World")
Export.Palm.Total = select(Export.Palm.Total, Year, Netweight, Export.Value)
Export.Palm.Total$Year = as.Date(as.character(Export.Palm.Total$Year), "%Y")

#Import.Palm.Total = filter(Import.Palm.Partner, Origin == "World")
Import.Palm.Total = select(Import.Palm.Total, Year, Netweight, Import.Value)
Import.Palm.Total$Year = as.Date(as.character(Import.Palm.Total$Year), "%Y")

#Aggregate Values - for Corr Plot and Time Series Plots

#Export.Pork.Total = filter(Export.Pork.Partner, Destination == "World")
Export.Pork.Total = select(Export.Pork.Total, Year, Netweight, Export.Value)
Export.Pork.Total$Year = as.Date(as.character(Export.Pork.Total$Year), "%Y")

#Import.Pork.Total = filter(Import.Pork.Partner, Origin == "World")
Import.Pork.Total = select(Import.Pork.Total, Year, Netweight, Import.Value)
Import.Pork.Total$Year = as.Date(as.character(Import.Pork.Total$Year), "%Y")

#Export.Beef.Total = filter(Export.Beef.Partner, Destination == "World")
Export.Beef.Total = select(Export.Beef.Total, Year, Netweight, Export.Value)
Export.Beef.Total$Year = as.Date(as.character(Export.Beef.Total$Year), "%Y")
View(Export.Beef.Total)
#Import.Beef.Total = filter(Import.Beef.Partner, Origin == "World")
Import.Beef.Total = select(Import.Beef.Total, Year, Netweight, Import.Value)
Import.Beef.Total$Year = as.Date(as.character(Import.Beef.Total$Year), "%Y")

#Make the graphs
BeefExport = xts(Export.Beef.Total[,-1],order.by=as.POSIXct(Export.Beef.Total$Year))
BeefImport = xts(Import.Beef.Total[,-1],order.by=as.POSIXct(Import.Beef.Total$Year))
View(BeefExport)
BeefTradexts <- cbind(BeefExport, BeefImport)
BeefTrade = BeefTradexts[complete.cases(BeefTradexts),]
# View(BeefTradexts)
# BeefTrade= dygraph(BeefTradexts, main = "Weight (MT) and Value (USD) for Beef Trade") %>%
#   dyRangeSelector()

PalmExport = xts(Export.Palm.Total[,-1],order.by=as.POSIXct(Export.Palm.Total$Year))
PalmImport = xts(Import.Palm.Total[,-1],order.by=as.POSIXct(Import.Palm.Total$Year))
PalmTradexts <- cbind(PalmExport, PalmImport)
PalmTrade= dygraph(PalmTradexts, main = "Weight (MT) and Value (USD) for Palm Oil Trade") %>%
  dyRangeSelector()
PalmTrade

# CocoExport = xts(Export.Coco.Total[,-1],order.by=as.POSIXct(Export.Coco.Total$Year))
# CocoImport = xts(Import.Coco.Total[,-1],order.by=as.POSIXct(Import.Coco.Total$Year))
# CocoTradexts <- cbind(CocoExport, CocoImport)
# CocoTrade = CocoTradexts[complete.cases(CocoTradexts),]
# View(CocoTrade)
# CocoTradeUSD = CocoTrade[,c(2,4)]
# CocoTradeMT = CocoTrade[,c(1,3)]
# CocoUSDPlot = dygraph(CocoTradeUSD, main = "Coconut Oil Trade in USD") %>%
#   dyRangeSelector()
# CocoMTPlot = dygraph(CocoTradeMT, main = "Coconut Oil Trade in USD") %>%
#   dyRangeSelector()

# PorkExport = xts(Export.Pork.Total[,-1],order.by=as.POSIXct(Export.Pork.Total$Year))
# PorkImport = xts(Import.Pork.Total[,-1],order.by=as.POSIXct(Import.Pork.Total$Year))
# PorkTradexts <- cbind(PorkExport, PorkImport)
# PorkTrade= dygraph(PorkTradexts, main = "Weight (MT) and Value (USD) for Pork (Swine) Trade") %>%
#   dyRangeSelector()
# PorkTrade
# 
# PorkEx = dygraph(PorkExport, main = "Pork Exports")%>%
#   dyRangeSelector()
# View(PalmTradexts)
# PalmTradexts = PalmTradexts[complete.cases(PalmTradexts),]
# View(PalmTradexts)
# PalmTradexts = PalmTradexts[,c(2,4)]
# PalmTrade = dygraph(PalmTradexts, main = "Palm Trade in USD")%>%
#   dyRangeSelector()


#Aggregate Coco Plots


#Aggregate Palm Plots
PalmTradexts = PalmTradexts[complete.cases(PalmTradexts),]
colnames(PalmTradexts) = c("Export(MT)", "Export(USD)", "Import(MT)", "Import(USD)") 
View(PalmTradexts)
PalmTradeUSD = PalmTradexts[,c(2,4)]
PalmTradeMT = PalmTradexts[,c(1,3)]
PalmUSDPlot = dygraph(PalmTradeUSD, main = "Palm Trade in USD") %>%
  dyRangeSelector()
PalmMTPlot = dygraph(PalmTradeMT, main = "Palm Trade in MT") %>%
  dyRangeSelector()
PalmTrade = PalmTradexts 

#Aggregate Pork Plots
PorkExport = xts(Export.Pork.Total[,-1],order.by=as.POSIXct(Export.Pork.Total$Year))
PorkImport = xts(Import.Pork.Total[,-1],order.by=as.POSIXct(Import.Pork.Total$Year))
PorkTradexts <- cbind(PorkExport, PorkImport)
PorkTrade = PorkTradexts[complete.cases(PorkTradexts),]
colnames(PorkTrade) = c("Export(MT)", "Export(USD)", "Import(MT)", "Import(USD)") 
PorkTradeUSD = PorkTrade[,c(2,4)]
PorkTradeMT = PorkTrade[,c(1,3)]
PorkUSDPlot = dygraph(PorkTradeUSD, main = "Pork(Swine) Trade in USD")
PorkMTPlot= dygraph(PorkTradeMT, main = "Pork(Swine) Trade in MT") %>%
  dyRangeSelector()

#Aggregate Beef Plots
BeefTrade = BeefTradexts[complete.cases(BeefTradexts),]
colnames(BeefTrade) = c("Export(MT)", "Export(USD)", "Import(MT)", "Import(USD)") 
BeefTradeUSD = BeefTrade[,c(2,4)]
BeefTradeMT = BeefTrade[,c(1,3)]
BeefUSDPlot = dygraph(BeefTradeUSD, main = "Beef Trade in USD") %>%
  dyRangeSelector()
BeefMTPlot = dygraph(BeefTradeMT, main = "Beef Trade in MT") %>%
  dyRangeSelector()

#Aggregate Time Series Plots
BeefUSDPlot
BeefMTPlot

PalmMTPlot
PalmUSDPlot

PorkUSDPlot
PorkMTPlot

#Aggregate Tables
BT = as.data.frame(BeefTrade)

#PorkTrade
PT = as.data.frame(PorkTrade)
#PalmTrade
PalmT = as.data.frame(PalmTrade)

Palm4 = filter(Palm, Month == "Jan")%>%
  select(Price, Year)
Beef4 = filter(Beef, Month == "Jan")%>%
  select(Price, Year)
Pork4 = filter(Pork, Month == "Jan")%>%
  select(Price, Year)


Palm4 = filter(Palm4, Year>1988, Year<2015)
Year = c(Palm4$Year)
Price = c(Palm4$Price)
Export_MT = c(PalmT$`Export(MT)`)
Export_USD = c(PalmT$`Export(USD)`)
Import_MT = c(PalmT$`Import(MT)`) 
Import_USD = c(PalmT$`Import(USD)`) 
Palm_Cor = cbind.data.frame(Year, Price, Export_MT, Export_USD, Import_MT, Import_USD)


Beef4 = filter(Beef4, Year>1985, Year<2015)
Year = c(Beef4$Year)
Price = c(Beef4$Price)
Export_MT = c(BT$`Export(MT)`)
Export_USD = c(BT$`Export(USD)`)
Import_MT = c(BT$`Import(MT)`) 
Import_USD = c(BT$`Import(USD)`) 
Beef_Cor = cbind.data.frame(Year, Price, Export_MT, Export_USD, Import_MT, Import_USD)

Pork4 = filter(Pork4, Year>1985, Year<2015)
Year = c(Pork4$Year)
Price = c(Pork4$Price)
Export_MT = c(PT$`Export(MT)`)
Export_USD = c(PT$`Export(USD)`)
Import_MT = c(PT$`Import(MT)`) 
Import_USD = c(PT$`Import(USD)`) 
Pork_Cor = cbind.data.frame(Year, Price, Export_MT, Export_USD, Import_MT, Import_USD)

beefcon = read.csv("Beef.csv", header = FALSE)
porkcon = read.csv("Swine.csv", header = FALSE)
palmcon = read.csv("Palm_Oil.csv", header = FALSE)
palmcon = filter(palmcon, V2>1988)

#Cor Plot DFs
Pork_Cor = mutate(Pork_Cor, Consumption = porkcon$V4)
Beef_Cor = mutate(Beef_Cor, Consumption = beefcon$V4)
Palm_Cor = mutate(Palm_Cor, Consumption = palmcon$V4)

Pork_Cor1 = select(Pork_Cor, -Year)
Beef_Cor1 = select(Beef_Cor, -Year)
Palm_Cor1 = select(Palm_Cor, -Year)
View(Palm_Cor1)
View(Pork_Cor)
Pork_Corplot = corrplot.mixed(cor(Pork_Cor1), lower="number")
Beef_Corplot = corrplot.mixed(cor(Beef_Cor1), lower="number")
Palm_Corplot = corrplot.mixed(cor(Palm_Cor1), lower="number")
#Create Cor
