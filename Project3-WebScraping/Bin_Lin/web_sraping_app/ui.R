library(markdown)
library(ggplot2)
library(shinydashboard)
#library(googleCharts)


dashboardPage(
  dashboardHeader(title = "Popularity of Online News",
                  titleWidth = 280
                  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Exploratory Visualization", tabName = "plots", icon = icon("th")),
      menuItem("Prediction Model", tabName = "prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "data", 
              tabsetPanel(type = "tabs", 
                          tabPanel("Web Scraping", 
                                   tags$ul(style="font-size: 18px;",
                                           tags$li("Web site: ", a(href="http://mashable.com", "http://mashable.com")), 
                                           tags$li("API: ", a(href="", "http://mashable.com/stories.json?hot_per_page={}&new_per_page={}&rising_per_page={}&new_after={}&hot_after={}&rising_after={}")),
                                           tags$li("Store a list of simplfied article json object into a file"),
                                           br(),
                                           tags$img(src = 'article_list_json.PNG', width = '100%', height = 'auto'),
                                           tags$li("Pull the article web page based on the link, store as html files to local disk. Ex: ", a(href="http://mashable.com/2016/02/24/internet-of-commerce-brandspeak/", "http://mashable.com/2016/02/24/internet-of-commerce-brandspeak/")),
                                           tags$li("Use BeautifulSoup is used for scraping with html.parser"),
                                           br(),
                                           tags$img(src = 'python_bs_scraping.PNG', width = '100%', height = 'auto'),
                                           tags$li("Store the article details into Article object, append to a set"),
                                           br(),
                                           tags$img(src = 'python_article_class.PNG', width = '70%', height = 'auto'),
                                           tags$li("Store all the Articles to JSON file. Convert back to CSV in R (library: jsonlite)"),
                                           br(),
                                           tags$img(src = 'article_detail_screen.PNG', width = '100%', height = 'auto')
                                   )
                                   
                          ),
                          tabPanel("Data", 
                                   DT::dataTableOutput("table")
                          )
              ) #end of tabsetPanel
      ) #end of tabItem data
      ,
      tabItem(tabName = "plots",
              tabsetPanel(type = "tabs", 
                          tabPanel("Topics", 
                                   fluidRow(
                                     box(
                                       width = 8, status = "info", solidHeader = TRUE,
                                       title = "Popularity",
                                       plotOutput('plot_topics', width = "100%", height = 500)
                                       #bubblesOutput("packagePlot", width = "100%", height = 600)
                                     ),
                                     box(
                                       width = 4, status = "info",
                                       title = "Hot Topics",
                                       DT::dataTableOutput("table_topic_count")
                                       #tableOutput("packageTable")
                                     )
                                   )
                          ), 
                          tabPanel("Channels", 
                                   checkboxGroupInput("typeCheckGroup", label = "Article Types", inline = TRUE,
                                                      choices = list("Hot" = "hot", "Rising" = "rising", "New" = "new"),
                                                      selected = c("hot","rising","new")),
                                   
                                   plotOutput('plot_channels')
                                   ),
                          tabPanel("Authors", 
                                   plotOutput('plot_author')
                          ),
                          tabPanel("Shares",
                                   plotOutput('plot_shares_density'),
                                   br(),
                                   plotOutput('plot_shares_over_time')
                          ),
                          tabPanel("Predictive Attributes",
                                   selectInput("attributes", 
                                               "Predictive Attributes (multiple allowed):",
                                               multiple = TRUE,
                                               colnames(select(articles.scaled, -shares)),
                                               selected = 'num_keywords'
                                   ),
                                   checkboxInput("isScaled", "Scaled", FALSE),
                                   plotOutput('plot_shares_vs_rest')
                          )
              )
      ) #end of tabItem plots
      ,
      tabItem(tabName = "prediction",
                tags$ul(style="font-size: 18px;",
                  tags$li("Predicting the Popularity (threshold is 1400 shares, > 1400 => YES else NO)"),
                  tags$li("Algorithms: Random Forest, Logistic Regression. Random Forest is slightly better, use the Random Forest model"), 
                  br(),
                  tags$img(src = 'training-summary2.PNG', width = '100%', height = 'auto'),
                  tags$li("Variables importance: more important ones => timedelta, sentiment, less important => channel "), 
                  br(),
                  tags$img(src = 'training-variables-importance2.PNG', width = '100%', height = 'auto'),
                  br(),
                  tags$li("Metrics"),
                  tags$img(src = 'training-metrics2.PNG', width = '100%', height = 'auto')
                )
      ) #end of tabItem summary
    ) #end of tabItems
    #,
    #style = "overflow:auto; width:100%; height:100%; "
    
  ) #end of dashboardBody
  ,
  skin = "yellow"
  
) # end of page


