choice <- names(states)[-1]

ui <- dashboardPage(
  dashboardHeader(title = "Crashes & Inspections"),
  dashboardSidebar(
    #        sidebarUserPanel("Satish Joshi"),
    #        sidebarSearchForm(textId = "searchText",
    ##                          buttonId = "searchButton",
    #                          label = "Search...",
    #                          icon = shiny::icon("search")),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Summary", tabName = "summary", icon = icon("list")),
      menuItem("Maps", tabName = "map", icon = icon("map-marker")),
      menuItem("Charts", tabName = "chart", icon = icon("bar-chart")),
      menuItem("Data Tables", tabName = "data", icon = icon("database"))
    ),
    sliderInput("crashrows",
                "Makes for crashes:",
                min = 1,  max = 65000, value = c(10000,65000)),
    sliderInput("insprows",
                "Makes for inspections:",
                min = 1,  max = 1900000, value = c(240000,1900000))
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
      tabItem(tabName = "summary",
              fluidRow(
                infoBoxOutput("TotalInspections"),
                infoBoxOutput("progressBox")
              ),
              fluidRow(
                infoBoxOutput("approvalBox"),
                infoBoxOutput("approvalBox2")
              ),
              fluidRow(
                infoBoxOutput("approvalBox3"),
                infoBoxOutput("approvalBox4")
              ),
              fluidRow(
                infoBoxOutput("approvalBox5"),
                infoBoxOutput("approvalBox6")
              ),
              fluidRow(
                infoBoxOutput("approvalBox7"),
                infoBoxOutput("approvalBox8")
              ),
              fluidRow(
                infoBoxOutput("approvalBox9"),
                infoBoxOutput("approvalBox10")
              ),
              fluidRow(
                infoBoxOutput("approvalBox11"),
                infoBoxOutput("approvalBox12")
              )        
      ),
      tabItem(tabName = "map",
              fluidRow(
                box( status = "info", solidHeader = TRUE,
                     collapsible = TRUE,
                     selectizeInput("selected",
                                    "Select Parameter to Display",
                                    selected = "TotalCrashes",
                                    choice)),
                box( status = "success", solidHeader = TRUE,
                     collapsible = FALSE,
                     htmlOutput("geoChart")),
                
                
                box( status = "info", solidHeader = TRUE,
                     collapsible = TRUE,
                     selectizeInput("selected2",
                                    "Select Paramter to Display",
                                    selected = "TotalFatalities",
                                    choice)),
                box( status = "success", solidHeader = TRUE,
                     collapsible = FALSE,
                     htmlOutput("geoChart2")),
                
                box( status = "info", solidHeader = TRUE,
                     collapsible = TRUE,
                     selectizeInput("selected3",
                                    "Select Parameter to Display",
                                    selected = "TotalInjuries",
                                    choice)),
                box( status = "success", solidHeader = TRUE,
                     collapsible = FALSE,
                     htmlOutput("geoChart3"))
                #title="Chart3",
                
              )
      ),
      tabItem(tabName = "chart",
              box(width = 12, 
                  title = "Total Crashes & Inspections over last 3 years", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  htmlOutput("crashChart"),
                  br(),
                  br(),
                  htmlOutput("inspectionChart")
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(htmlOutput("crashtable"), width = 10),
                box(htmlOutput("crashbymaketable"), width = 10),
                box(htmlOutput("inspectionbymaketable"), width = 10)
              )
      )
      
    ) # End of tabitems
  ) # End of dashboard body
) # End of dashpage

