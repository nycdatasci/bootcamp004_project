library(shiny)
library(shinydashboard)
library(googleVis)
require(datasets)
library(plotly)
library(dplyr)
library(sp)
library(rgdal)
library(dplyr)
library(deldir)
library(leaflet)
library(rgeos)
library(htmltools)
library(maps)
library(geosphere)
require(RCurl)
library(DT)
library(reshape2)
library(stringr)

setwd('/Users/satishjoshi/DataBootcamp/bootcamp004_project/Project3-WebScraping/uwonderiwander')

#price table
price_table <- read.csv("price_table.csv", stringsAsFactors = F)
price_table$X <- NULL
price_table$Index <- NULL
price_table$Updated <- NULL

#gvisColumnChart
logistics <- read.csv("logistics.csv", stringsAsFactors = F)
logistics$X <- NULL
logistics$Link <- NULL
logistics_tbl = logistics[,c("Rank2015","CompanyName","NetRevenue","GrossRevenue","Employees")]

#gvisBubbleChart
forhire <- read.csv("for_hire.csv", stringsAsFactors = F)
forhire$X <- NULL
forhire_tbl = forhire[,c("Rank2015","CompanyName","Tractors", "Trailers", "NetIncome","Revenue","Employees")]

#gvisComboChart
private_fleet <- read.csv("private_fleet.csv", stringsAsFactors = F)
private_fleet[["TruckAssets"]] <- NULL
private_fleet[["TrailerAssets"]] <- NULL
private_fleet[["Rank2015Reverse"]] <- NULL

private_fleet$X <- NULL
private_fleet$Link <- NULL
private_fleet$TruckAssets = (private_fleet$Tractors2015+private_fleet$StraightTrucks2015+private_fleet$StraightTrucks2015+private_fleet$PickupCargoVan2015)/1000
private_fleet$TrailerAssets = (private_fleet$Trailers2015)/1000
private_fleet$Rank2015Reverse = 100 - (private_fleet$Rank2015)
pvt_flt_tbl = private_fleet[,c("Rank2015","CompanyName","TruckAssets", "TrailerAssets", "AnnualSales_Billion")]

#gvisMerge
global_freight <- read.csv("global_freight.csv", stringsAsFactors = F)
global_freight$X <- NULL
global_freight$Link <- NULL
global_freight = global_freight[complete.cases(global_freight),]

global_freight_by_country <- group_by(global_freight, Headquarters) %>% summarise(FreightRevenue=sum(FreightRevenue))
global_freight_by_country = global_freight_by_country[complete.cases(global_freight_by_country),]

## MOTION CHART DATA

COM_Prices <- read.csv("com_price_table_clean.csv", stringsAsFactors = F)
gsub( " ", "", COM_Prices)
COM_Prices$X <- NULL
COM_Prices$FullDate<- as.Date(COM_Prices$FullDate, "%m/%d/%Y")
#summary(COM_Prices)
unique_dates = unique(COM_Prices$FullDate)
COM_Prices[unique(COM_Prices$FullDate),]

COM_Prices_tbl = COM_Prices[,c("FullDate","Comdata","T.Chek.Self.Serve", "T.Chek.Wholesale", "Week.Ending")]

DOE_Prices <- read.csv("doe_price_table_clean.csv", stringsAsFactors = F)
gsub( " ", "", DOE_Prices)
DOE_Prices$X <- NULL
DOE_Prices$FullDate<- as.Date(DOE_Prices$FullDate, "%m/%d/%Y")
#summary(DOE_Prices)
unique_dates = unique(DOE_Prices$FullDate)
DOE_Prices[unique(DOE_Prices$FullDate),]
#length(unique(DOE_Prices))
#nrow(DOE_Prices)
#length(unique_dates)

Comdata = COM_Prices[,c("Comdata", "FullDate")]
Comdata[,"Name"]  <- c("Comdata")
names(Comdata)[1] <- "Price"
TCheckSelfServe = COM_Prices[,c( "T.Chek.Self.Serve", "FullDate")]
TCheckSelfServe[,"Name"]  <- c("TCheckSelfServe")
names(TCheckSelfServe)[1] <- "Price"
TCheckWholesale = COM_Prices[,c( "T.Chek.Wholesale", "FullDate")]
TCheckWholesale[,"Name"]  <- c("TCheckWholesale")
names(TCheckWholesale)[1] <- "Price"
unique(TCheckWholesale$FullDate)

DOE_Prices$Name
DOEPrices <- NULL
#unique(DOEPrices$Name)

DOENames = names(DOE_Prices)
str(DOENames)
for(i in 5:(length(DOENames)-1)) {
#  print (DOENames[i])
  col_name = gsub("[.]", "_",DOENames[i])
#  print (col_name)
  
  DOEPriceAdd = unique(DOE_Prices[,c(DOENames[i], "FullDate")])
  DOEPriceAdd[,"Name"]  <- c(gsub("[.]", "_",DOENames[i]))
  names(DOEPriceAdd)[1] <- "Price"
#  print (nrow(DOEPriceAdd))
  DOEPrices <- rbind(DOEPrices, DOEPriceAdd)
#  unique(DOEPrices$Name)

}


unique(DOEPrices$Name)


COMPrices <- NULL
unique(COMPrices$Name)
COMPrices <- rbind(Comdata, TCheckSelfServe)
unique(COMPrices$Name)
COMPrices <- rbind(COMPrices, TCheckWholesale)
unique(COMPrices$Name)


#Weeknumber calculations
COM_Prices$Weeknum <- as.numeric( format(COM_Prices$FullDate+3, "%U"))
DOE_Prices$Weeknum <- as.numeric( format(DOE_Prices$FullDate+3, "%U"))

DOEPrices_Weeknum = DOE_Prices[!duplicated(DOE_Prices[,c('Weeknum','Year')]),]
COMPrices_Weeknum = COM_Prices[!duplicated(COM_Prices[,c('Weeknum','Year')]),]

nrow(DOEPrices_Weeknum)
nrow(COMPrices_Weeknum)

#results2 <- DOEPrices_Weeknum[(DOEPrices_Weeknum$Weeknum == COMPrices_Weeknum$Weeknum) & 
#                             (DOEPrices_Weeknum$Year == COMPrices_Weeknum$Year), ]
#COMPrices_Weeknum = COMPrices_Weeknum[(COMPrices_Weeknum$Weeknum %in% {DOEPrices_Weeknum$Weeknum}) &
#                  (COMPrices_Weeknum$Year %in% {DOEPrices_Weeknum$Year}),   ]
#COM_Unique = distinct(COMPrices_Weeknum[c("Weeknum","Year")])
#DOE_Unique = distinct(DOEPrices_Weeknum[c("Weeknum","Year")])
# DOE_Weeks = c()
# for (i in 1:nrow(DOEPrices_Weeknum)) {
#   DOE_Weeks[i] = paste(toString(DOEPrices_Weeknum$Weeknum[i]),toString(DOEPrices_Weeknum$Year[i]), sep = ",")
#   print (paste(toString(DOEPrices_Weeknum$Weeknum[i]),toString(DOEPrices_Weeknum$Year[i]), sep = ","))
# }
# COM_Weeks = c()
# for (i in 1:nrow(COMPrices_Weeknum)) {
#   COM_Weeks[i] = paste(toString(COMPrices_Weeknum$Weeknum[i]),toString(COMPrices_Weeknum$Year[i]), sep = ",")
#   print (paste(toString(COMPrices_Weeknum$Weeknum[i]),toString(COMPrices_Weeknum$Year[i]), sep = ","))
# }
# length(DOE_Weeks)
# length(COM_Weeks)
# length(unique(DOE_Weeks))
# length(unique(COM_Weeks))
# DOEPrices_Weeknum[(paste(toString(DOEPrices_Weeknum$Weeknum[i]),toString(DOEPrices_Weeknum$Year[i]), sep = ",")) ==
#    (DOE_Weeks[DOE_Weeks %in% COM_Weeks])]
# !(DOE_Weeks  %in% COM_Weeks)
# subset(DOE_Weeks, !(DOE_Weeks %in% COM_Weeks))
# subset(DOE_Weeks, (DOE_Weeks %in% COM_Weeks))


### PLOTTING ###

ui <- dashboardPage(
    dashboardHeader(title = "Diesel Prices CRAP"),
    dashboardSidebar(
        sidebarUserPanel(name = "Satish Joshi"),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Bubble Chart", tabName = "bubble", icon = icon("circle")),
            menuItem("Global Freight", tabName = "global", icon = icon("globe")),
            menuItem("Private Fleet", tabName = "private", icon = icon("bus")),
            menuItem("For Hire Fleet", tabName = "forhire", icon = icon("truck")),
            menuItem("Logistics Companies", tabName = "logistics", icon = icon("hand-o-up")),
            menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
            menuItem("Data", tabName = "data", icon = icon("table")) 
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "home", 
                    fluidRow(
                      #                     column(4,
                      #                            includeText("include.txt"),
                      #                            br(),
                      #                            pre(includeText("include.txt"))
                      #                     ),
                      #                     column(4,
                      #                            includeHTML("include.html")
                      #                     ),
                      column(12,
                             includeMarkdown("include.Rmd")
                      )
                    )
                    
            ),
            tabItem(tabName = "bubble",
                    h2("Dept of Energy Diesel Prices"),
#                     fluidRow(
#                       box(width = 8, status = "info", solidHeader = TRUE,
#                           title = "Choose Data Source",
#                           selectInput("BubbleInput", "",
#                                       choices = c('Dept of Energy - TTNews'='DOEPrices',
#                                                   'ComCheck - TTNews' = 'COMPrices'
#                                       ))),
#                       valueBoxOutput("count")
#                     ),
                    fluidRow(
                        box(htmlOutput("bubble"), width=3)),
                    sliderInput("range", "Range:",
                                min = 0, max = 4, value = c(0,4)),
                    h2("COM Check Diesel Prices"),
                    fluidRow(
                       box(htmlOutput("bubble2"), width=3)),
                     sliderInput("range", "Range:",
                                 min = 0, max = 4, value = c(0,4))
            ),
            tabItem(tabName = "global",
                    h2("Global Freight"),
                    fluidRow(
                      box(htmlOutput("global"), width=12))
            ),
            tabItem(tabName = "private",
                    h2("Private Fleets"),
                    fluidRow(
                      box(htmlOutput("private"), width=10)),
                    fluidRow(
                      box(width = 10, status = "info", solidHeader = F,
                          title = "Private Fleet", DT::dataTableOutput("pvt_flt_tbl")))
            ),
            tabItem(tabName = "forhire",
                    h2("For Hire Fleets"),
                    fluidRow(
                    box(htmlOutput("forhire"), width=10)),
                    fluidRow(
                      box(width = 10, status = "info", solidHeader = F,
                          title = "For Hire Fleet", DT::dataTableOutput("for_hire_tbl")))
            ),
            tabItem(tabName = "logistics",
                    h2("Logistics Companies"),
                    fluidRow(
                    box(htmlOutput("logistics"), width=10)),
                    fluidRow(
                      box(width = 10, status = "info", solidHeader = F,
                          title = "Logistics Companies", DT::dataTableOutput("logistics_tbl")))
            ),
            tabItem(tabName = "calendar",
                    h2("Diesel Prices"),
                    fluidRow(
                        box(htmlOutput("com_calendar"), width = 12, margin = 2)),
                    fluidRow(
                            box(width = 12, status = "info", solidHeader = F,
                               title = "ComCheck Prices", DT::dataTableOutput("com_prices_tbl"))
                    )
            ),
            tabItem(tabName = "data",
                    fluidRow(
                      box(width = 12, status = "info", solidHeader = F,
                          title = "Main Price Table", DT::dataTableOutput("main_price_tbl")),
                      box(width = 12, status = "info", solidHeader = F,
                          title = "DOE Diesel Data", DT::dataTableOutput("doe_raw_price_tbl")),
                      box(width = 12, status = "info", solidHeader = F,
                          title = "Comcheck Diesel Data", DT::dataTableOutput("com_raw_price_tbl")))
            )
        
        )
    
), skin = "black")


server <- function(input, output) {
    
    output$bubble <- renderGvis({
        gvisMotionChart(DOEPrices, idvar="Name", timevar="FullDate", sizevar = "Price", 
                        options=list(state='{"colorOption":"_UNIQUE_COLOR"};'))
#      gvisMotionChart(DOE_Prices, idvar = "Name", timevar = "FullDate", xvar = "GDP per Capita",
#                      yvar = "Population Growth", colorvar = "Region", sizevar = "Total Population",
#                      options = list(width= 850, showChartButtons=TRUE))
    })
    output$bubble2 <- renderGvis({
         gvisMotionChart(COMPrices, idvar="Name", timevar="FullDate", sizevar = "Price", 
                         options=list(state='{"colorOption":"_UNIQUE_COLOR"};'))
       
    })
    output$private <- renderGvis({
       gvisComboChart(private_fleet, xvar="CompanyName",
                             yvar=c(  "Rank2015Reverse", "TruckAssets", "TrailerAssets"),
                             options=list(seriesType="bars",
                                          series='{1: {type:"line"},
                                          2: {type:"line"}}',
                                          width=800, height=600))

     })
     output$logistics <- renderGvis({
       gvisColumnChart(logistics, xvar="CompanyName", yvar=c("NetRevenue", "Employees"),
                       options=list(width=800, height=600))
       
     })     
     output$forhire <- renderGvis({
       gvisBubbleChart(forhire, idvar="CompanyName", 
                       xvar="NetIncome", yvar="Employees",
                       colorvar="Tractors", sizevar="Revenue",
                       
                       options=list(title='For Hire Fleets', 
                                    width=800, height=600)
                        # hAxis='{minValue:75, maxValue:125}')
                       )
     
     }) 
     
     output$global <- renderGvis({
       G <- gvisGeoChart(global_freight_by_country, "Headquarters", "FreightRevenue", 
                        options=list(width=300, height=600))
       
       T <- gvisTable(global_freight, 
                        options=list(page='enable',width=800, height=600))

       GT <- gvisMerge(G,T, horizontal=TRUE) 
       
     })
    output$pvt_flt_tbl <- DT::renderDataTable(DT::datatable({
      pvt_flt_tbl #private_fleet
    }, options = list(searching=T, scrollX=TRUE)))

    output$for_hire_tbl <- DT::renderDataTable(DT::datatable({
      forhire_tbl
    }, options = list(searching=T, scrollX=TRUE)))

    output$logistics_tbl <- DT::renderDataTable(DT::datatable({
      logistics_tbl
    }, options = list(searching=T, scrollX=TRUE)))
        
    output$com_prices_tbl <- DT::renderDataTable(DT::datatable({
      COM_Prices_tbl
    }, options = list(searching=T, scrollX=TRUE)))
    output$main_price_tbl <- DT::renderDataTable(DT::datatable({
      price_table
    }, options = list(searching=T, scrollX=TRUE)))
    output$doe_raw_price_tbl <- DT::renderDataTable(DT::datatable({
      DOE_Prices
    }, options = list(searching=T, scrollX=TRUE)))
    output$com_raw_price_tbl <- DT::renderDataTable(DT::datatable({
      COM_Prices
    }, options = list(searching=T, scrollX=TRUE)))
    
    output$com_calendar <- renderGvis({
        gvisCalendar(COM_Prices, 
                     datevar="FullDate", 
                     numvar="Comdata",
                     options=list(
                       title="Weekly Comcheck Diesel Prices",
                       height=800, width = 800,
                       calendar="{yearLabel: { fontName: 'Times-Roman',
                       fontSize: 14, color: '#1A8763', bold: true},
                       cellSize: 5,
                       cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                       focusedCellColor: {stroke:'red'}}")
        )
    })

}

shinyApp(ui, server)
