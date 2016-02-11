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
crashes_by_makeyear = crashes_by_make  %>% 
  group_by (Make, Report_Year) %>% 
  summarise(Crashes = sum(Total_Crashes))  %>% 
  arrange(Crashes) 
inspections_by_makeyear = insp_by_make  %>% 
  group_by (Make, Inspection_Year) %>% 
  summarise(Inspections = sum(Total_Insps))  %>% 
  arrange(Inspections)

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

# choice <- names(states)[-1]
# 
# ui <- dashboardPage(
#     dashboardHeader(title = "Crashes & Inspections"),
#     dashboardSidebar(
# #        sidebarUserPanel("Satish Joshi"),
# #        sidebarSearchForm(textId = "searchText",
# ##                          buttonId = "searchButton",
# #                          label = "Search...",
# #                          icon = shiny::icon("search")),
#         sidebarMenu(
#           menuItem("Home", tabName = "home", icon = icon("home")),
#           menuItem("Summary", tabName = "summary", icon = icon("list")),
#           menuItem("Maps", tabName = "map", icon = icon("map-marker")),
#             menuItem("Charts", tabName = "chart", icon = icon("bar-chart")),
#             menuItem("Data Tables", tabName = "data", icon = icon("database"))
#             ),
#         sliderInput("crashrows",
#                     "Makes for crashes:",
#                     min = 1,  max = 65000, value = c(10000,65000)),
#         sliderInput("insprows",
#                     "Makes for inspections:",
#                     min = 1,  max = 1900000, value = c(240000,1900000))
#         ),
#     
#     dashboardBody(
#         tabItems(
#           tabItem(tabName = "home",
#                   fluidRow(
# #                     column(4,
# #                            includeText("include.txt"),
# #                            br(),
# #                            pre(includeText("include.txt"))
# #                     ),
# #                     column(4,
# #                            includeHTML("include.html")
# #                     ),
#                     column(12,
#                            includeMarkdown("include.Rmd")
#                     )
#                   )
# 
#           ),
#           tabItem(tabName = "summary",
#           fluidRow(
#                    infoBoxOutput("TotalInspections"),
#                    infoBoxOutput("progressBox")
#             ),
#           fluidRow(
#                    infoBoxOutput("approvalBox"),
#                    infoBoxOutput("approvalBox2")
#             ),
#           fluidRow(
#                     infoBoxOutput("approvalBox3"),
#                     infoBoxOutput("approvalBox4")
#           ),
#           fluidRow(
#                     infoBoxOutput("approvalBox5"),
#                     infoBoxOutput("approvalBox6")
#           ),
#           fluidRow(
#                     infoBoxOutput("approvalBox7"),
#                     infoBoxOutput("approvalBox8")
#           ),
#           fluidRow(
#                     infoBoxOutput("approvalBox9"),
#                     infoBoxOutput("approvalBox10")
#           ),
#           fluidRow(
#                     infoBoxOutput("approvalBox11"),
#                     infoBoxOutput("approvalBox12")
#           )        
#           ),
#             tabItem(tabName = "map",
#                     fluidRow(
#                       box( status = "info", solidHeader = TRUE,
#                             collapsible = TRUE,
#                             selectizeInput("selected",
#                                            "Select Parameter to Display",
#                                            selected = "TotalCrashes",
#                                            choice)),
#                       box( status = "success", solidHeader = TRUE,
#                           collapsible = FALSE,
#                           htmlOutput("geoChart")),
#                       
#                       
#                       box( status = "info", solidHeader = TRUE,
#                             collapsible = TRUE,
#                             selectizeInput("selected2",
#                                            "Select Paramter to Display",
#                                            selected = "TotalFatalities",
#                                            choice)),
#                       box( status = "success", solidHeader = TRUE,
#                           collapsible = FALSE,
#                           htmlOutput("geoChart2")),
#                       
#                       box( status = "info", solidHeader = TRUE,
#                             collapsible = TRUE,
#                             selectizeInput("selected3",
#                                            "Select Parameter to Display",
#                                            selected = "TotalInjuries",
#                                            choice)),
#                       box( status = "success", solidHeader = TRUE,
#                           collapsible = FALSE,
#                           htmlOutput("geoChart3"))
#                       #title="Chart3",
#                       
#                     )
#             ),
#             tabItem(tabName = "chart",
#                         box(width = 12, 
#                           title = "Total Crashes & Inspections over last 3 years", status = "primary", solidHeader = TRUE,
#                         collapsible = TRUE,
#                         htmlOutput("crashChart"),
#                         br(),
#                         br(),
#                         htmlOutput("inspectionChart")
#                         )
#              ),
#             tabItem(tabName = "data",
#                     fluidRow(
#                         box(htmlOutput("crashtable"), width = 10),
#                         box(htmlOutput("crashbymaketable"), width = 10),
#                         box(htmlOutput("inspectionbymaketable"), width = 10)
#                     )
#             )
#             
#         ) # End of tabitems
#     ) # End of dashboard body
# ) # End of dashpage
# 
# 
#  
# server <- function(input, output) {
#   # TotalInspections = nrow(inspection_data)
#   # TotalCrashes = nrow(crash_data)
#   # 
#   # TotalFatalities = sum(crash_data$Fatalities)
#   # TotalInjuries = sum(crash_data$Injuries)
#   
#   output$TotalInspections <- renderInfoBox({
#     infoBox(
#       "Total Inspections", TotalInspections, icon = icon("list"),
#       color = "purple", fill = TRUE
#     )
#   })
#     output$progressBox <- renderInfoBox({
#       infoBox(
#        "Total Crashes", TotalCrashes, icon = icon("list"),
#        color = "purple", fill = TRUE
#       )
#     })
#    output$approvalBox <- renderInfoBox({
#      infoBox(
#         "Fatalities", TotalFatalities, icon = icon("thumbs-down", lib = "glyphicon"),
#         color = "red", fill = TRUE
#      )
#     })
#    output$approvalBox2 <- renderInfoBox({
#      infoBox(
#        "Injuries", TotalInjuries, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "red", fill = TRUE
#      )
#    })
#    # PercentTowaway = (nrow(crash_data[crash_data$Tow_Away == "Yes",])/TotalCrashes) * 100
#    # PercentHazmat = (nrow(crash_data[crash_data$Hazmat_released == "Yes",])/TotalCrashes) * 100
#    
#    output$approvalBox3 <- renderInfoBox({
#      infoBox(
#        "Tow Away", paste0(PercentTowaway,"%"), icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "red", fill = TRUE
#      )
#    })
#    output$approvalBox4 <- renderInfoBox({
#      infoBox(
#        "Hazmat Involved", paste0(PercentHazmat,"%"), icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "red", fill = TRUE
#      )
#    })
#    # TotalHazOOSViol = sum(inspection_data$HAZMAT_OOS_TOTAL)
#    # TotalOOSViol = sum(inspection_data$OOS_TOTAL)
#    # TotalVehOOSViol = sum(inspection_data$VEHICLE_OOS_TOTAL)
#    # TotalDriverOOSViol = sum(inspection_data$DRIVER_OOS_TOTAL)
#    
#    output$approvalBox5 <- renderInfoBox({
#      infoBox(
#        "Total OOS Violation", TotalOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "blue", fill = TRUE
#      )
#    })
#    output$approvalBox6 <- renderInfoBox({
#      infoBox(
#        "Hazmat OOS Violation", TotalHazOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "blue", fill = TRUE
#      )
#    })
#    output$approvalBox7 <- renderInfoBox({
#      infoBox(
#        "Vehicle OOS Violation", TotalVehOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "blue", fill = TRUE
#      )
#    })
#    output$approvalBox8 <- renderInfoBox({
#      infoBox(
#        "Driver OOS Violation", TotalDriverOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "blue", fill = TRUE
#      )
#    })
#    
#    # TotalUnsafeDrivingInsp = nrow(inspection_data[inspection_data$UNSAFE_INSP == "Y",])
#    # TotalAlcoholDrivingInsp = nrow(inspection_data[inspection_data$SUBT_ALCOHOL_INSP == "Y",])
#    # TotalFatugueDrivingInsp = nrow(inspection_data[inspection_data$FATIGUED_INSP == "Y",])
#    # TotalMaintenanxeInsp = nrow(inspection_data[inspection_data$VH_MAINT_INSP == "Y",])
#    output$approvalBox9 <- renderInfoBox({
#      infoBox(
#        "Vehicle Maintenance Insp", TotalMaintenanxeInsp, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "yellow", fill = TRUE
#      )
#    })
#    output$approvalBox10 <- renderInfoBox({
#      infoBox(
#        "Fatigued Driving Insp", TotalFatugueDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "yellow", fill = TRUE
#      )
#    })
#    output$approvalBox11 <- renderInfoBox({
#      infoBox(
#        "Alcohol Substance Insp", TotalAlcoholDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "yellow", fill = TRUE
#      )
#    })
#    output$approvalBox12 <- renderInfoBox({
#      infoBox(
#        "Unsafe Driving Insp", TotalUnsafeDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
#        color = "yellow", fill = TRUE
#      )
#    })  
#     output$geoChart <- renderGvis({
#         gvisGeoChart(states, "state.name", input$selected, 
#                      options=list(region="US", 
#                                   title="Chart1",
#                                   displayMode="regions", 
#                                   resolution="provinces",
#                                   width=360, height=225,
#                                   backgroundColor="lightbrown",
#                                   colorAxis="{colors:['pink', 'red']}"
#                                   ))
#     })
#     output$geoChart2 <- renderGvis({
#       gvisGeoChart(states, "state.name", input$selected2,
#                    options=list(region="US", 
#                                 title="Chart2",
#                                 displayMode="regions", 
#                                 resolution="provinces",
#                                 width=360, height=225,
#                                 backgroundColor="lightbrown",
#                                 colorAxis="{colors:['lightblue', 'darkblue']}"
#                               ))
#     })    
#     output$geoChart3 <- renderGvis({
#       gvisGeoChart(states, "state.name", input$selected3,
#                    options=list(region="US", 
#                                 title="Chart3",
#                                 displayMode="regions", 
#                                 resolution="provinces",
#                                 backgroundColor="lightbrown",
#                                 width=360, height=225,
#                                 colorAxis="{colors:['lightgreen', 'darkgreen']}"
#                                 ))
#     })
#     output$crashtable <- renderGvis({
#         gvisTable(states[order(states$Population, decreasing = T),],
#                   options=list(page='enable', width=800, height=300, color='blue'))
#     })
#     output$crashbymaketable <- renderGvis({
#       gvisTable(crashes_by_makeyear[order(crashes_by_makeyear$Crashes, decreasing = T),],
#                 options=list(page='enable',width=500, height=300))
#     })
#     output$inspectionbymaketable <- renderGvis({
#       gvisTable(inspections_by_makeyear[order(inspections_by_makeyear$Inspections, decreasing = T),],
#                 options=list(page='enable',width=500, height=300))
#     })
#     options=list(width=200, height=300)
#     output$crashChart <- renderGvis({
#       
#       gvisColumnChart(
#         crashes_by_makename[crashes_by_makename$Crashes >= input$crashrows[1] & 
#                               crashes_by_makename$Crashes <= input$crashrows[2],],
#         
#         #head(crashes_by_makename[order(crashes_by_makename$Crashes, decreasing = T),], n=input$crashrows),
# 
#         
#         #crashes_by_makename_subset, 
#         xvar="Make", yvar= "Crashes",
#                       options=list(title="Crashes by Make",
#                                    titleTextStyle="{color:'red', 
#                                            fontName:'Courier', 
#                                             fontSize:16}", 
#                                    vAxis="{title:'Crash Count'}",
#                                    hAxis="{title:'Make'}",
#                                    color = "green",
#                                    legend = "{position: 'none'}",
#                                    hAxis.textPosition = "in",
#                                    hAxis.slantedTextAngle = 90,
#                                    backgroundColor="#D3D3D3", 
#                                    width=600, height=300 )
#                      )
# 
#     })
#     output$inspectionChart <- renderGvis({
#       
#       gvisColumnChart(
#         inspections_by_makename[inspections_by_makename$Inspections >= input$insprows[1] & 
#                                   inspections_by_makename$Inspections <= input$insprows[2],],
#     
#         #head(inspections_by_makename[order(inspections_by_makename$Inspections, decreasing = T),], n=input$insprows),
#                       #gvisColumnChart(inspections_by_makename_subset, 
#                       xvar="Make", yvar= "Inspections", 
#                       options=list(title="Inspections by Make",
#                                    titleTextStyle="{color:'red', 
#                                            fontName:'Courier', 
#                                             fontSize:16}", 
#                                    vAxis="{title:'Inspection Count'}",
#                                    hAxis="{title:'Make'}",
#                                    color = "blue",
#                                    legend = "{position: 'none'}",
#                                    backgroundColor="#D3D3D3", 
#                                    width=600, height=300 )
#                      )
#       
#     })    
# }
# 
# shinyApp(ui, server)
