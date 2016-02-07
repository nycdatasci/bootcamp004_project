library(shiny)
library(shinydashboard)
library(googleVis)
require(datasets)

states = data.frame(state.name, state.x77)

states = states[,(names(states) %in% c("state.name","Population", "Area"))]

#crash_data = mutate(crash_data, Report_State = state.name[match(crash_data$Report_State, state.abb)])
bad_crash_state_data = crash_data[is.na(crash_data$Report_State), ]
crash_data = crash_data[!(is.na(crash_data$Report_State)), ]
  
by_state_crashes = crash_data %>% group_by(Report_State) %>% summarise(Total_Crashes = n())  %>% arrange(Report_State)           
by_state_fatals = crash_data %>% group_by(Report_State) %>% summarise(Total_Fatals = sum(Fatalities))  %>% arrange(Report_State)           
by_state_injures = crash_data %>% group_by(Report_State) %>% summarise(Total_Injuries = sum(Injuries)) %>% arrange(Report_State)           

byyear_state_crashes = crash_data %>% group_by(Report_State, Report_Year) %>% summarise(Total_Crashes = n())  %>% arrange(Report_State)           
byyear_state_fatals = crash_data %>% group_by(Report_State, Report_Year) %>% summarise(Total_Fatals = sum(Fatalities))  %>% arrange(Report_State)           
byyear_state_injures = crash_data %>% group_by(Report_State, Report_Year) %>% summarise(Total_Injuries = sum(Injuries)) %>% arrange(Report_State)           
# 
# by_state_crashes = mutate(by_state_crashes, Report_State = state.name[match(by_state_crashes$Report_State, state.abb)])
# by_state_fatals = mutate(by_state_fatals, Report_State = state.name[match(by_state_fatals$Report_State, state.abb)])
# by_state_injures = mutate(by_state_injures, Report_State = state.name[match(by_state_injures$Report_State, state.abb)])
# 
# bad_crashes = by_state_crashes[is.na(by_state_crashes$Report_State),]
# by_state_crashes = by_state_crashes[!(is.na(by_state_crashes$Report_State)),]
# bad_fatals = by_state_fatals[is.na(by_state_fatals$Report_State),]
# by_state_fatals = by_state_fatals[!(is.na(by_state_fatals$Report_State)),]
# bad_injures = by_state_injures[is.na(by_state_injures$Report_State),]
# by_state_injures = by_state_injures[!(is.na(by_state_injures$Report_State)),]
# 
# ungroup(byyear_state_crashes)
# byyear_state_crashes = mutate(byyear_state_crashes, Report_StateFull = state.name[match(byyear_state_crashes$Report_State, state.abb)])
# byyear_state_fatals = mutate(byyear_state_fatals, Report_State = state.name[match(byyear_state_fatals$Report_State, state.abb)])
# byyear_state_injures = mutate(byyear_state_injures, Report_State = state.name[match(byyear_state_injures$Report_State, state.abb)])
# 
# bad_crashes2 = byyear_state_crashes[is.na(byyear_state_crashes$Report_State),]
# byyear_state_crashes = byyear_state_crashes[!(is.na(byyear_state_crashes$Report_State)),]
# bad_fatals2 = byyear_state_fatals[is.na(byyear_state_fatals$Report_State),]
# byyear_state_fatals = byyear_state_fatals[!(is.na(byyear_state_fatals$Report_State)),]
# bad_injures2 = byyear_state_injures[is.na(byyear_state_injures$Report_State),]
# byyear_state_injures = byyear_state_injures[!(is.na(byyear_state_injures$Report_State)),]


states = mutate(states, TotalCrashes = by_state_crashes$Total_Crashes[match(by_state_crashes$Report_State, states$state.name)])
states = mutate(states, TotalFatalities = by_state_fatals$Total_Fatals[match(by_state_fatals$Report_State, states$state.name)])
states = mutate(states, TotalInjuries = by_state_injures$Total_Injuries[match(by_state_injures$Report_State, states$state.name)])

by_2015 = byyear_state_crashes %>% filter(Report_Year==2015) 
states = mutate(states, Y2015Crashes = by_2015$Total_Crashes[match(by_2015$Report_State, states$state.name)])
by_2014 = byyear_state_crashes %>% filter(Report_Year==2014) 
states = mutate(states, Y2014Crashes = by_2014$Total_Crashes[match(by_2014$Report_State, states$state.name)])

by_2013 = byyear_state_crashes %>% filter(Report_Year==2013) 
missing_states= c(state.name[!(state.name %in% by_2013$Report_State)])
missing_state_list = data_frame(missing_states, c("2013"), 0)
l = list(by_2013, missing_state_list)
by_2013 = rbindlist(l)
states = mutate(states, Y2013Crashes = by_2013$Total_Crashes[match(by_2013$Report_State, states$state.name)])

by_2015 = byyear_state_fatals %>% filter(Report_Year==2015) 
states = mutate(states, Y2015Fatalities = by_2015$Total_Fatals[match(by_2015$Report_State, states$state.name)])
by_2014 = byyear_state_fatals %>% filter(Report_Year==2014) 
states = mutate(states, Y2014Fatalities = by_2014$Total_Fatals[match(by_2014$Report_State, states$state.name)])
by_2013 = byyear_state_fatals %>% filter(Report_Year==2013) 
missing_states= c(state.name[!(state.name %in% by_2013$Report_State)])
missing_state_list = data_frame(missing_states, c("2013"), 0)
l = list(by_2013, missing_state_list)
by_2013 = rbindlist(l)
states = mutate(states, Y2013Fatalities = by_2013$Total_Fatals[match(by_2013$Report_State, states$state.name)])

by_2015 = byyear_state_injures %>% filter(Report_Year==2015) 
states = mutate(states, Y2015Injuries = by_2015$Total_Injuries[match(by_2015$Report_State, states$state.name)])
by_2014 = byyear_state_injures %>% filter(Report_Year==2014) 
states = mutate(states, Y2014Injuries = by_2014$Total_Injuries[match(by_2014$Report_State, states$state.name)])
by_2013 = byyear_state_injures %>% filter(Report_Year==2013) 
missing_states= c(state.name[!(state.name %in% by_2013$Report_State)])
missing_state_list = data_frame(missing_states, c("2013"), 0)
l = list(by_2013, missing_state_list)
by_2013 = rbindlist(l)
states = mutate(states, Y2013Injuries = by_2013$Total_Injuries[match(by_2013$Report_State, states$state.name)])

#states = mutate(states, PercentCrashes = (states$Total_Crashes/states$Population) * 100)

write.csv(states, "data/crash_summary.csv")

crashes_by_makename = crashes_by_make  %>% 
  group_by (Make) %>% 
  summarise(Crashes = sum(Total_Crashes))  %>% 
  arrange(Crashes) 
(order(crashes_by_makename$Crashes, decreasing = T))

crashes_by_makename_subset = crashes_by_makename[crashes_by_makename$Crashes > 100, ]

inspections_by_makename = insp_by_make  %>% 
  group_by (Make) %>% 
  summarise(Inspections = sum(Total_Insps))  %>% 
  arrange(Inspections) 
(order(inspections_by_makename$Inspections, decreasing = T))

inspections_by_makename_subset = inspections_by_makename[inspections_by_makename$Inspections > 500, ]


choice <- names(states)[-1]

ui <- dashboardPage(
    dashboardHeader(title = "Trucking Crash and Inspections Dashboard"),
    dashboardSidebar(
        sidebarUserPanel("Satish Joshi",
                         image = "https://pbs.twimg.com/profile_images/633795666754576386/HS1cKWjb.jpg"),
        sidebarSearchForm(textId = "searchText",
                          buttonId = "searchButton",
                          label = "Search...",
                          icon = shiny::icon("search")),
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Chart", tabName = "chart", icon = icon("chart")),
            menuItem("Data", tabName = "data", icon = icon("database"))
            )
        ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(
                        box(title = "Select 1", status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            selectizeInput("selected",
                                           "Select1 Item to Display",
                                           selected = "TotalCrashes",
                                           choice)),
                        box(title = "Income Chart", status = "success", solidHeader = TRUE,
                            collapsible = FALSE,
                            htmlOutput("geoChart")),
                        
                        box(title = "Select 2", status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            selectizeInput("selected2",
                                           "Select3 Item to Display",
                                           selected = "TotalFatalities",
                                           choice)),
                        box(title = "Murder Chart", status = "success", solidHeader = TRUE,
                            collapsible = FALSE,
                            htmlOutput("geoChart2")),

                        box(title = "Select 3", status = "info", solidHeader = TRUE,
                            collapsible = TRUE,
                            selectizeInput("selected3",
                                           "Select3 Item to Display",
                                           selected = "TotalInjuries",
                                           choice)),
                        box(title = "Popluation", status = "success", solidHeader = TRUE,
                            collapsible = FALSE,
                            htmlOutput("geoChart3"))
                        )
                    ),
            tabItem(tabName = "chart",
                fluidRow(
                      box(title = "Select 4", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        selectizeInput("selected4",
                                     "Select4 Item to Display",
                                     selected = "TopMakesForCrashes",
                                     choice)),
                  
                        box(title = "Total Crashes over last 3 years", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        htmlOutput("crashChart")
                        ),
                          
                        box(title = "Total Inspections over last 3 years", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        htmlOutput("inspectionChart")
                        )
                          
                )
            ),
            tabItem(tabName = "data",
                    fluidRow(
                        box(htmlOutput("crashtable"), width = 10),
                        box(htmlOutput("crashbymaketable"), width = 10),
                        box(htmlOutput("inspectionbymaketable"), width = 10)
                    )
            )
            
        )
    )
)

server <- function(input, output) {

    
    output$geoChart <- renderGvis({
        gvisGeoChart(states, "state.name", input$selected, 
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=360, height=240,
                                  backgroundColor="lightbrown",
                                  colorAxis="{colors:['pink', 'red']}"
                                  ))
    })
    output$geoChart2 <- renderGvis({
      gvisGeoChart(states, "state.name", input$selected2,
                   options=list(region="US", 
                                displayMode="regions", 
                                resolution="provinces",
                                width=360, height=240,
                                  backgroundColor="lightgreen",
                                  colorAxis="{colors:['lightblue', 'darkblue']}"
                              ))
    })    
    output$geoChart3 <- renderGvis({
      gvisGeoChart(states, "state.name", input$selected3,
                   options=list(region="US", 
                                displayMode="regions", 
                                resolution="provinces",
                                  backgroundColor="lightblue",
                                colorAxis="{colors:['lightgreen', 'darkgreen']}",
                                width=360, height=240))
    })
    output$crashtable <- renderGvis({
        gvisTable(states,
                  options=list(page='enable'))
    })
    output$crashbymaketable <- renderGvis({
      gvisTable(crashes_by_makename,
                options=list(page='enable'))
    })
    output$inspectionbymaketable <- renderGvis({
      gvisTable(inspections_by_makename,
                options=list(page='enable'))
    })
    
    output$crashChart <- renderGvis({
      
      gvisColumnChart(head(crashes_by_makename_subset[order(crashes_by_makename_subset$Crashes, decreasing = T),], n=10),

        
        #crashes_by_makename_subset, 
        xvar="Make", yvar= "Crashes",
                      options=list(title="Crashes by Make",
                                   titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                            fontSize:16}", 
                                   vAxis="{title:'Crash Count'}",
                                   hAxis="{title:'Make'}",
                                   legend="bottom",
                                   color = "green",
                                   backgroundColor="#D3D3D3", 
                                   width=360, height=240 )
                     )

    })
    output$inspectionChart <- renderGvis({
      
      gvisColumnChart(head(inspections_by_makename_subset[order(inspections_by_makename_subset$Inspections, decreasing = T),], n=10),
                      #gvisColumnChart(inspections_by_makename_subset, 
                      xvar="Make", yvar= "Inspections", 
                      options=list(title="Inspections by Make",
                                   titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                            fontSize:16}", 
                                   vAxis="{title:'Inspection Count'}",
                                   hAxis="{title:'Make'}",
                                   legend="bottom",
                                   color = "blue",
                                   backgroundColor="#D3D3D3", 
                                   width=360, height=240 )
                     )
      
    })    
}

shinyApp(ui, server)
