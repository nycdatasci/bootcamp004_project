library(markdown)
library(ggplot2)
library(shinydashboard)
library(googleCharts)


dashboardPage(
  dashboardHeader(title = "Food Price"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Objectives", tabName = "objectives", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Plots", tabName = "plots", icon = icon("th")),
      menuItem("Summary", tabName = "summary", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "objectives",
              tags$h3(
              tags$ul(
                tags$li("Food price changes over time, from year 1974 - 2015."), 
                tags$li("Food price changes vs all-items prices changes. All-items include all consumer goods and services, including food."), 
                tags$li("Food price changes vs Producer price changes. Producer price changes measures the average change in prices paid to domestic producers for their output.")
              ))
      ) #end of tabItem objectives
      ,
      tabItem(tabName = "data", 
              DT::dataTableOutput("table"),
              hr(),
              tags$ul(
                style="font-size: 18px;",
                tags$li("Food price changes data and Producer price changes data: ", a(href = "http://www.ers.usda.gov/data-products/food-price-outlook.aspx", "http://www.ers.usda.gov/data-products/food-price-outlook.aspx")), 
                tags$li("All-item price changes data: ", a(href="http://www.bls.gov/cpi/data.htm", "http://www.bls.gov/cpi/data.htm"))
              )
              
      ) #end of tabItem data
      ,
      tabItem(tabName = "plots",
              #style = "height: 100%; width: 100%",
              tabsetPanel(type = "tabs", 
                          #style = "height: 50px; width: 100%",
                          tabPanel("Food Category Importance", 
                                   #tags$h3("Food Categories weighted by their relative importance or share of consumer expenditures."),
                                   htmlOutput("plot_pie"),
                                   hr(),
                                   htmlOutput('plot_category_percent_bar')
                          ),
                          tabPanel("Food Price Changes", 
                                   style = "height: 600px; width: 100%",
                                   htmlOutput("plot_food_price"),
                                   hr(),
                                   selectInput("foodCategory", "Food Categories (multiple allowed):",
                                               multiple = TRUE,
                                               colnames(select(food.price, -year)),
                                               selected = 'All.food'
                                            
                                   ),
                                   tags$ul(style="font-size: 18px;",
                                           tags$li("The Consumer Price Index (CPI) for food is a component of the all-items CPI."), 
                                           tags$li("The CPI measures the average change over time in the prices paid by consumers for goods and services."), 
                                           tags$li("The food-away-from-home: restaurant purchases"),
                                           tags$li("The food-at-home (grocery store or supermarket food items)"),
                                           tags$li("What happened during 1970's => Great Inflation")
                                   )

                                   
                          ), 
                          tabPanel("Food Price by Year", 
                                   htmlOutput("plot_food_price_bar"),
                                   sliderInput("leftYear", "Left:", 
                                               min=min(food.price$year), max=max(food.price$year), value=2014, step=1),
                                   sliderInput("rightYear", "Right:", 
                                               min=min(food.price$year), max=max(food.price$year), value=2015, step=1)
                                   
                          ),
                          tabPanel("Food vs All-item", 
                                   htmlOutput("plot_food_vs_all"),
                                   hr(),
                                   tags$ul(style="font-size: 18px;",
                                           tags$li("Food price changes mostly aligns with all-item price changes."), 
                                           tags$li("Food price inflation has outpaced the economy-wide inflation in recent years.")
                                   )
                                   ),
                          tabPanel("Food vs Producer", 
                                   style = "height: 600px; width: 100%",
                                   htmlOutput("plot_food_vs_producer"),
                                   hr(),
                                   selectInput("foodCategoryProducer", "Food Categories (multiple allowed):",
                                               multiple = TRUE,
                                               colnames(select(producer.price, -year)),
                                               selected = 'Finished.consumer.foods'
                                   ),
                                   tags$ul(style="font-size: 18px;",
                                           tags$li("Looking ahead to 2016, ERS predicts food-at-home (supermarket) prices to rise 2.0 to 3.0 percent—a rate of inflation that remains in line with the 20-year historical average of 2.5 percent."), 
                                           tags$li("These forecasts are based on an assumption of normal weather conditions"), 
                                           tags$li("Changes in farm-level and wholesale-level PPIs are of particular interest in forecasting food CPIs.")

                                   )
                                   
                                    
                          ),
                          tabPanel("Food Price Correlation", 
                                   #style = "height: 3000px;",
                                   sliderInput("zoom", "Zoom:", min=1, max=5, value=1, step=1),
                                   uiOutput("plot_food_correlation_ui")
                          )
              )
      ) #end of tabItem plots
      ,
      tabItem(tabName = "summary",
              h3(
                tags$ul(
                  tags$li("Food price has been increasing, in different amount of percentage. "), 
                  tags$li("Since 1990, food price changes keep under small percentage"), 
                  tags$li("The degree of food price inflation varies depending on the type of foods")
                ))
              
              
      ) #end of tabItem summary
    ) #end of tabItems
    ,
    style = "overflow:auto; width:100%; height:100%; "
    
  ) #end of dashboardBody
  ,
  skin = "yellow"
  
) # end of page


