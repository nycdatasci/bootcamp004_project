library(shinythemes)
library(googleVis)

shinyUI(
  navbarPage(
            title = toupper("US Food Commodity Explorer"),
            id = "nav",
            theme = shinytheme("cosmo"),
            

############################################################### INTRODUCTION TAB PANEL ############################################################################

      tabPanel("Introduction",
         fluidPage(theme = shinytheme("cosmo"),
                   mainPanel(
                     h4("Comparing Trade Balances and Consumption Data"),
                     br(),
                     h5("This project aims to visualize and explore food product data provided by the"), span(a(href="http://www.ers.usda.gov/data-products/food-availability-(per-capita)-data-system/summary-findings.aspx
", "US Department of Agriculture.")), 
                   h5("KEY QUESTION: What insights can be learned from from plotting imports, exports, production, and consumption values against one another other, over time, for different product categories?"),
                   br(),
                   img(src ='Title_Panel.jpg', width = "300px"
                   ),
                   wellPanel(
                     h3("More About the Data:"),
                     h5("  1. The original dataset uses a variant of the 1992 SITC trade classification."),
                     h5("  2. Consumption values originate from the US Department of Agriculture's Food Availability Per Capita Data System, which provides proxies for actual consumption."),
                     h5("  3. Production values come from official records of the US Department of Agriculture for select product categories."),
                     h5("  4. Trade Balance is treated, in this study, as simply (export - import).")
                   ),
                   
                   wellPanel(
                     h3("Tab Instructions:"),
                     h5("Data Explorer - plot different food categories against each other using the filters and compare what you see"), 
                     h5("Bubble Viz - choose different food categories to see how consumption, production, import, and export values change over time "),
                     h5("Data Table - see the sorted data by year and commodity type in table format")
          
                   )
         ))
        ),


#     
# ####################################################### DATA EXPLORER TAB ########################################################################
#     
    tabPanel("Data Explorer",    
             
               h4("Select the filters to compare import and export values to domestic food production and consumption"),
               fluidRow(
                 column(12,
                        wellPanel(
                          h6(toupper("Units:")),
                          h6("Imports, Exports, Production, Consumption, Trade Balance measured in Metric Tons (1000 MT)"),
                          h6("'Import_Consumption_Ratio' is defined here as Imports/Consumption = 100*Imports/Consumption"),
                          h6("'Production_Export_Ratio' is defined here as Production/Exports = Production/Exports")
                        )),
                 column(6,
                      wellPanel(
                          h5("Plot1"),
                          selectInput('xcol1', 'X Axis', choices = list("Market_Year" = "Market_Year",
                                                                        "Exports" = "Exports",
                                                                        "Imports" = "Imports", 
                                                                        "Production" = "Production",
                                                                        "Consumption" = "Consumption",
                                                                        "Trade_Balance" = "Trade_Balance",
                                                                        "Production_Export_Ratio" = "Production_Export_Ratio",
                                                                        "Import_Consumption_Ratio" = "Import_Consumption_Ratio"
                                                                        ),
                                        label = "Choose X-Axis", multiple = FALSE),
                          selectInput('ycol1', 'Y Axis', choices = list("Exports" = "Exports",
                                                                        "Market_Year" = "Market_Year",
                                                                        "Imports" = "Imports", 
                                                                        "Production" = "Production",
                                                                        "Consumption" = "Consumption",
                                                                        "Trade_Balance" = "Trade_Balance",
                                                                        "Production_Export_Ratio" = "Production_Export_Ratio",
                                                                        "Import_Consumption_Ratio" = "Import_Consumption_Ratio"
                          ),
                                      label = "Choose Y-Axis", multiple = FALSE),
                          selectizeInput('productcompare1', 'Choose Product(s)', 
                                         choices = list("Meal, Cottonseed" = "Meal, Cottonseed", 
                                                        "Meal, Fish" = "Meal, Fish",
                                                        "Meal, Peanut" = "Meal, Peanut",
                                                        "Meal, Rapeseed" = "Meal, Rapeseed",
                                                        "Meal, Soybean" = "Meal, Soybean",
                                                        "Meal, Sunflowerseed" = "Meal, Sunflowerseed",
                                                        "Meat, Beef and Veal" = "Meat, Beef and Veal",
                                                        "Meat, Swine" = "Meat, Swine",
                                                        "Oats" = "Oats",
                                                        "Oil, Coconut" = "Oil, Coconut",
                                                        "Oil, Cottonseed" = "Oil, Cottonseed", 
                                                        "Oil, Olive" = "Oil, Olive", 
                                                        "Oil, Palm" = "Oil, Palm", 
                                                        "Oil, Palm Kernel" = "Oil, Palm Kernel",
                                                        "Oil, Peanut" = "Oil, Peanut", 
                                                        "Oil, Rapeseed" = "Oil, Rapeseed", 
                                                        "Oil, Soybean" = "Oil, Soybean",
                                                        "All" = "All"), multiple = TRUE),
                          sliderInput("n_years1", label = "Date Range", min = 1960, max = 2016, value = c(1960:2016), ticks = TRUE, step = 1)
                      )
                    ),
      
                   column(6,
                      wellPanel(
                          h5("Plot 2"),
                          selectInput('xcol2', 'X Axis', choices = list("Market_Year" = "Market_Year",
                                                                        "Exports" = "Exports",
                                                                        "Imports" = "Imports", 
                                                                        "Production" = "Production",
                                                                        "Consumption" = "Consumption",
                                                                        "Trade_Balance" = "Trade_Balance",
                                                                        "Production_Export_Ratio" = "Production_Export_Ratio",
                                                                        "Import_Consumption_Ratio" = "Import_Consumption_Ratio"
                            
                          ), label = "Choose X-Axis", multiple = FALSE),
                          selectInput('ycol2', 'Y Axis', choices = list("Consumption" = "Consumption",
                                                                        "Market_Year" = "Market_Year",
                                                                        "Exports" = "Exports",
                                                                        "Imports" = "Imports", 
                                                                        "Production" = "Production",
                                                                        "Trade_Balance" = "Trade_Balance",
                                                                        "Production_Export_Ratio" = "Production_Export_Ratio",
                                                                        "Import_Consumption_Ratio" = "Import_Consumption_Ratio"
                            
                            
                            
                            
                            
                          ),
                                      label = "Choose Y-Axis", multiple = FALSE),
                          selectizeInput('productcompare2', 'Choose Product(s)', 
                                         choices = list("Meal, Cottonseed" = "Meal, Cottonseed", 
                                                        "Meal, Fish" = "Meal, Fish",
                                                        "Meal, Peanut" = "Meal, Peanut",
                                                        "Meal, Rapeseed" = "Meal, Rapeseed",
                                                        "Meal, Soybean" = "Meal, Soybean",
                                                        "Meal, Sunflowerseed" = "Meal, Sunflowerseed",
                                                        "Meat, Beef and Veal" = "Meat, Beef and Veal",
                                                        "Meat, Swine" = "Meat, Swine",
                                                        "Oats" = "Oats",
                                                        "Oil, Coconut" = "Oil, Coconut",
                                                        "Oil, Cottonseed" = "Oil, Cottonseed", 
                                                        "Oil, Olive" = "Oil, Olive", 
                                                        "Oil, Palm" = "Oil, Palm", 
                                                        "Oil, Palm Kernel" = "Oil, Palm Kernel",
                                                        "Oil, Peanut" = "Oil, Peanut", 
                                                        "Oil, Rapeseed" = "Oil, Rapeseed", 
                                                        "Oil, Soybean" = "Oil, Soybean",
                                                        "All" = "All"), multiple = TRUE),
                          sliderInput("n_years2", label = "Date Range", min = 1960, max = 2016, value = c(1960:2016), ticks = TRUE, step = 1)
                      )
                  )
                   
               ),
             fluidRow(
               column(6, textOutput("Plot1title"), plotOutput("plot1", height = "600px", hover = hoverOpts(id = "plot1_hover"), brush = brushOpts(id = "plot1_brush"))), 
               column(6, textOutput("Plot2title"), plotOutput("plot2", height = "600px", hover = hoverOpts(id = "plot2_hover"), brush = brushOpts(id = "plot2_brush")))
             ),
             fluidRow(column(12, h5("Plot 1 Points"), verbatimTextOutput("hover_info1"))),
             fluidRow(column(12, verbatimTextOutput("brush_info1"))),
             fluidRow(column(12, h5("Plot 2 Points"), verbatimTextOutput("hover_info2"))),
             fluidRow(column(12, verbatimTextOutput("brush_info2")))
        ),

################################################################### TAB 3 - BUBBLE VIZ ############################################

  tabPanel("Bubble Viz!",
           h4("Compare products over time"),
           fluidPage(theme = shinytheme("cosmo"), h4 = "Compare Products Over Time",
                     sidebarLayout(position = "left", fluid = TRUE,
                                   mainPanel = htmlOutput("motionviz", align = "center"),
                                   sidebarPanel(
                                     selectizeInput('productcompare', 'Choose Product(s)', choices = list("Meal, Cottonseed" = "Meal, Cottonseed", 
                                                     "Meal, Fish" = "Meal, Fish",
                                                     "Meal, Peanut" = "Meal, Peanut",
                                                     "Meal, Rapeseed" = "Meal, Rapeseed",
                                                     "Meal, Soybean" = "Meal, Soybean",
                                                     "Meal, Sunflowerseed" = "Meal, Sunflowerseed",
                                                     "Meal, Beef and Veal" = "Meal, Beef and Veal",
                                                     "Meat, Swine" = "Meat, Swine",
                                                     "Oats" = "Oats",
                                                     "Oil, Coconut" = "Oil, Coconut",
                                                     "Oil, Cottonseed" = "Oil, Cottonseed", 
                                                     "Oil, Olive" = "Oil, Olive", 
                                                     "Oil, Palm" = "Oil, Palm", 
                                                     "Oil, Palm Kernel" = "Oil, Palm Kernel",
                                                     "Oil, Peanut" = "Oil, Peanut", 
                                                     "Oil, Rapeseed" = "Oil, Rapeseed", 
                                                     "Oil, Soybean" = "Oil, Soybean",
                                                     "All" = "All"),
                                      multiple = TRUE, label = "Choose a Food Product", selected = "Meal, Soybean"),
                         selectizeInput('exportcompare', 'Bubble Size',
                                      choices = list("Imports", "Exports"), selected = "Imports"))
                      ))
                  
#                    fluidRow(
#                      shiny::column(4, offset = 4,
#                                     sliderInput("year", "Year",
#                                                 min = min(df$Market_Year), max = max(df$Market_Year),
#                                                 value = min(data$Year), animate = TRUE)
#                     )
                   ),
         

#                     ),
#                     
############################### DATA TABLE ####################################
tabPanel("Data Table",
         h4("Import, Export, Consumption, and Production values"),
         h6(toupper("Units:")),
         h6("Imports, Exports, Production, Consumption, Trade Balance measured in Metric Tons (1000 MT)"),
         h6("Imports/Consumption = 100*Imports/Consumption"),
         h6("Production/Exports = Production/Exports"),
         fluidPage(theme = shinytheme("cosmo"), 
                   sidebarLayout(
                     sidebarPanel(checkboxGroupInput('show_vars', 'Columns to Show:',
                                                     names(df), selected = names(df)),
                                  selectInput("Commodity_Description",
                                             "Commodity:",
                                             c("All",
                                               unique(as.character(df$Commodity_Description)))),
                                  
                                  selectInput("Market_Year",
                                              "Year:",
                                              c("All",
                                                unique(as.character(df$Market_Year))))
                                  ),
                     mainPanel(DT::dataTableOutput("table")), position = "left")
        )
),
                     
  
   

  
#################################################################### TAB 4 - ABOUT ME ##############################################
  tabPanel("About",
           fluidPage(theme = shinytheme("cosmo"), sidebarPanel(fluidRow(
               wellPanel(
                   h4("Ashwin Swamy"),
                   h5("Data Science Fellow"),
                   tags$span(
                     tags$a(href = "https://www.linkedin.com/in/ashwinswamy", icon("linkedin", "fa-2x")),
                     tags$a(href = "https://github.com/AshwinSwamy/AshwinSwamy.github.io", icon("github", "fa-2x"), style = "margin-left: 20px;")
                   ),
                   tags$hr(),
                   h6("This project was completed for the NYC Data Science Academy Winter 2016 Bootcamp."),
                   h6("More info at:"),
                   tags$a(href = "http://nycdatascience.com/", "NYC Data Science Academy", style = "font-size: 18px;"),
                   h6("All code available at GitHub location above.")
           ))
  )

))))



         


    


 


