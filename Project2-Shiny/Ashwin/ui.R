library(shinythemes)
library(googleCharts)

shinyUI(
  navbarPage(
            title = "Domestic Food Production Explorer",
            id = "nav",
            theme = shinytheme("cosmo"),
            position = "fixed-top",

    ############################################################### INTRODUCTION TAB PANEL ############################################################################
# 
tabPanel("Introduction",
         fluidPage(theme = shinytheme("cosmo"),
                   mainPanel(
                     h2(toupper("Trade Balances and Consumption Data")),
                     br(),
                     h5("This project aims to visualize and explore data provided by"), span(a(href="http://atlas.media.mit.edu/en/", "MIT Observatory of Economic Complexity (OEC)")), "and the", span(a(href="http://www.ers.usda.gov/data-products/food-availability-(per-capita)-data-system/summary-findings.aspx
", "US Department of Agriculture")), 
                   h5("What insights can be learned from from plotting imports, exports, production, and consumption against one another other over time, particularly for a single product category?"),
                   h5("The study looks at food products in particular, both in terms of cost (measured in USD dollars) (for import and export values) and mass (measured in metric tons.)"),
                   br(),
                   img(src =  ("Title_Panel.jpg"),
                       width = 16
                   ),
                   wellPanel(
                     h3("More About the Data:"),
                     h5("  1. Import and Export values from the OEC show trade flows for a variety of product categories going back to 1960. The original dataset uses the 1992 HS (Harmonized System) trade classification."),
                     h5("  2. Consumption values originate from the US Department of Agriculture's Food Availability Per Capita Data System, which provides proxies for actual consumption."),
                     h5("  3. Production values come from official records of the US Department of Agriculture for select product categories."),
                     h5("  4. Trade Balance is treated, in this study, as simply (export - import).")
                   ),
                   
                   wellPanel(
                     h3("Tab Instructions:"),
                     h5("Data Explorer - plot different food categories against each other using the filters and compare what you see"), 
                     h5("Bubble Viz - choose different food categories to see how consumption, production, import, and export values change over time "),
                     h5("Leaflet 1 - choose a year, import or export, and product to see how trade flows have changed over time for food products"),
                     h5("Leaflet 2 - choose a year and product to notice changes in trade partners and overlaps between import and export flows")
                   )
         ))),


#     
# ####################################################### DATA EXPLORER TAB ########################################################################
#     
    tabPanel("Data Explorer",    
             h4("Select the filters below to compare import and export variables surrounding domestic food production and consumption"),
             fluidRow(
               
                 column(6,
                    wellPanel(
                        h5("Plot1"),
                        selectInput('xcol1', 'X Axis', choices = colnames(df), label = "Choose X-Axis", multiple = TRUE),
                        selectInput('ycol1', 'Y Axis', choices = colnames(df), label = "Choose X-Axis", multiple = TRUE),
                        sliderInput("n_years1", label = "Date Range", min = 1960, max = 2016, value = 2000, step = 1))),
                 
                 column(6,
                    wellPanel(
                        h5("Plot2"),
                        selectInput('xcol2', 'X Axis', choices = names(df), label = "Choose X-Axis", multiple = TRUE),
                        selectInput('ycol2', 'Y Axis', choices = names(df), label = "Choose X-Axis", multiple = TRUE),
                        sliderInput("n_years2", label = "Date Range", min = 1960, max = 2016, value = 2000, step = 1)))
                 
             )),
  
                      
#               fluidRow(
#                 column(6, plotOutput("plot1", height = "600px")),
#                 column(6, plotOutput("plot2", height = "600px"))),
#               fluidRow(
#                 column(6, plotOutput("plot3", height = "600px")),
#                 column(6, plotOutput("plot4", height = "600px"))
#                 )
#             )
#   ),
################################################################### TAB 3 - BUBBLE VIZ ############################################
tabPanel("Bubble Viz!",
         fluidRow(
           theme = shinytheme("cosmo"))),
################################################################### TAB 4 - Leaflet 1 ############################################           
tabPanel("Trade Flows",
         fluidRow(
           theme = shinytheme("cosmo"))),

#################################################################### TAB 5 - Leaflet 2 #############################################
tabPanel("Trade Partners",
         fluidRow(
           theme = shinytheme("cosmo"))),
#################################################################### TAB 6 - ABOUT ME ##############################################
  tabPanel("About",
          fluidRow(
            theme = shinytheme("cosmo")
#            box(
#              title = "Author", status = "info", solidHeader = TRUE, width = 10,
#              h3("Ashwin Swamy"),
#              h5("Data Scientist")
# #              tags$span(
#                tags$a("href = https://www.linkedin.com/in/ashwinswamy)", "LinkedIn"),
#                tags$a("http://nycdatascience.com/alumni/)", "NYC Data Science Academy"
#                ))
             )
           )))

         


    


 


