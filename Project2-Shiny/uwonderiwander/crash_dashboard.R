library(shiny)
library(shinydashboard)
library(googleVis)
require(datasets)

states <- data.frame(state.name, state.x77)

by_state_crashes = crash_data %>% group_by(Report_State) %>% summarise(Total_Crashes = n())  %>% arrange(Report_State)           
by_state_fatals = crash_data %>% group_by(Report_State) %>% summarise(Total_Fatals = sum(Fatalities))  %>% arrange(Report_State)           
by_state_injures = crash_data %>% group_by(Report_State) %>% summarise(Total_Injuries = sum(Injuries)) %>% arrange(Report_State)           

by_state_fatalities = crash_data %>% group_by(Report_State, Fatalities) %>% summarise(crash_count_by_state = sum(Fatalities))  %>% arrange(Report_State)           
by_state_injuries = crash_data %>% group_by(Report_State, Injuries) %>% summarise(crash_count_by_state = sum(Injuries))  %>% arrange(Report_State)           

by_state_crashes = mutate(by_state_crashes, Report_State = state.name[match(by_state_crashes$Report_State, state.abb)])
by_state_fatals = mutate(by_state_fatals, Report_State = state.name[match(by_state_fatals$Report_State, state.abb)])
by_state_injures = mutate(by_state_injures, Report_State = state.name[match(by_state_injures$Report_State, state.abb)])

bad_crashes = by_state_crashes[is.na(by_state_crashes$Report_State),]
by_state_crashes = by_state_crashes[!(is.na(by_state_crashes$Report_State)),]
bad_fatals = by_state_fatals[is.na(by_state_fatals$Report_State),]
by_state_fatals = by_state_fatals[!(is.na(by_state_fatals$Report_State)),]
bad_injures = by_state_injures[is.na(by_state_injures$Report_State),]
by_state_injures = by_state_injures[!(is.na(by_state_injures$Report_State)),]

by_state_fatals$Report_State
states = mutate(states, TotalCrashes = by_state_crashes$Total_Crashes[match(by_state_crashes$Report_State, states$state.name)])
states = mutate(states, TotalFatalities = by_state_fatals$Total_Fatals[match(by_state_fatals$Report_State, states$state.name)])
states = mutate(states, TotalInjuries = by_state_injures$Total_Injuries[match(by_state_injures$Report_State, states$state.name)])

by_2015 = byyear_state_crashes %>% filter(Report_Year==2015) 
states = mutate(states, T2015Crashes = by_2015$Total_Crashes[match(by_2015$Report_State, states$state.name)])
by_2014 = byyear_state_crashes %>% filter(Report_Year==2014) 
states = mutate(states, T2014Crashes = by_2014$Total_Crashes[match(by_2014$Report_State, states$state.name)])

by_2013 = byyear_state_crashes %>% filter(Report_Year==2013) 
missing_states = c(state.name[!(state.name %in% by_2013$Report_State)])

states = mutate(states, T2013Crashes = 0)

states = mutate(states, T2013Crashes = by_2013$Total_Crashes[match(by_2013$Report_State, states$state.name)])

choice <- names(states)[-1]

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarUserPanel("Shu Yan",
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
                  
                        box(title = "Column Chart", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        htmlOutput("columnChart")
                        )
                )
            ),
            tabItem(tabName = "data",
                    fluidRow(
                        box(htmlOutput("table"), width = 10)
                    )
            )
        )
    )
)

server <- function(input, output) {
    min_query <- min(states$Population)
    max_query <- max(states$Population)
    min_query2 <- min(states$Income)
    max_query2 <- max(states$Income)
    
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
    output$table <- renderGvis({
        gvisTable(states,
                  options=list(page='enable'))
    })
    
    output$columnChart <- renderGvis({
        if(input$searchButton) {
            isolate({
                if(input$searchText %in% states$state.name) {
                    gvisColumnChart(
                        data.frame(item=choice,
                                   value=as.numeric(states[input$searchText,-1])))
                } else {
                    gvisColumnChart(
                        data.frame(item=choice,
                                   value=sapply(states[,-1], mean)))
                }
            })
        }
    })
}

shinyApp(ui, server)
