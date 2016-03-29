###############################################################################
# [Proj3 ShinyApp]
# by Sung Pil Moon
###############################################################################
# cat("\014")
# getwd()
# setwd("~/Documents/Dropbox/DataScience/Projects/Project 3 - WebScrape/")
# #save.image(file="proj3_indeedUnsupervised.RData")

load('proj3_indeedUnsupervised.RData')

library(dplyr)
library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)
library(fmsb)
library(kernlab)


ui = dashboardPage( 

  header <- dashboardHeader(
    title = "Indeed DS Shiny App"
  ),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Indeed DataTable", tabName = "myIndeedDataTable", icon = icon("fa fa-table")),
      menuItem("Indeed Cluster Analysis", tabName = "myClsuterAnalysis", icon = icon("bar-chart-o")),
      menuItem("Indeed Sentiment Analysis", tabName = "myCompReviewAnalysis", icon = icon("fa fa-columns")),
      menuItem("Dice Keyword Analysis", tabName = "myDiceKeywordsAnalysis", icon = icon("fa fa-thumbs-o-up")),
      menuItem("About", tabName = "myTabForIntroduction", icon = icon("fa fa-compass"))
    )
  ),
  
  body <- dashboardBody(
    
    tabItems(
      
      ##########################################################
      # First contents (DataTable)
      ###########################################################
      tabItem("myIndeedDataTable",
              fluidRow(
                valueBoxOutput("boxForNumRowsInMap"),
                valueBoxOutput("boxForReviewCountInMap"),
                valueBoxOutput("boxForAvgRatingsInMap")
              ), 
              fluidRow(
                # box(
                #   title = "Indeed Job Map", solidHeader = TRUE,
                #   width=12, #status="info",
                #   collapsible = TRUE, collapsed = TRUE, 
                #   leafletOutput("myIndeedJobMap", width=900, height=550)
                # ),
                box(
                  title = "Indeed DataTable ", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  DT::dataTableOutput("myIndeedDataTable")
                )
              )
              
      ), # end of tabItem (indeedTable)
      
      
      ##########################################################
      # 2nd contents (Cluster Analysis)
      ###########################################################
      tabItem("myClsuterAnalysis",
              
              
              fluidRow( 
                box(
                  title = "Indeed Cluster Analysis ", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE, collapsed = FALSE,
                  plotOutput("myClusterAnalysisChart") 
                )
              ),
              
              fluidRow(
                
                box(
                  title = "Option Inputs for Cluster Analysis ", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  column(6, checkboxInput("checkboxForNormalize", label = "Normalize data for K-mean analysis", value = TRUE) ),
                  column(6, 
                         sliderInput("sliderForKmeanRange", "K Range:", min = 1, max = 15, value = 2, step = 1)
                  ),
                  br(),
                  column(6, 
                         selectizeInput('singleSelectizeForClusterAnalysis1', 'Cluster Analysis Variable 1:', 
                                        #c("Choose one"= '', "Overall Rating" = "overall_rating", "Work/Life Balance Rating" = "wl_bal_rating",
                                        c("Overall Rating" = "overall_rating", "Work/Life Balance Rating" = "wl_bal_rating",
                                          "Compensation / Benefits Rating" = "benefit_rating", "Culture Rating" = "culture_rating",
                                          "Job Security Rating" = "jsecurity_rating", "Management Rating" = "mgmt_rating"
                                        ),
                                        multiple = FALSE
                         )
                  ),
                  column(6, 
                         selectizeInput('singleSelectizeForClusterAnalysis2', 'Cluster Analysis Variable 2:', 
                                        c("Work/Life Balance Rating" = "wl_bal_rating", "Overall Rating" = "overall_rating", 
                                          "Compensation / Benefits Rating" = "benefit_rating", "Culture Rating" = "culture_rating",
                                          "Job Security Rating" = "jsecurity_rating", "Management Rating" = "mgmt_rating"
                                        ),
                                        multiple = FALSE
                         )
                  )
                ) # box
              ),
              
              
              
              fluidRow( 
                box(
                  title = "Scree Plot for choosing K", solidHeader = TRUE,
                  width=6, status="info",
                  collapsible = TRUE, collapsed = FALSE,
                  
                  plotOutput("myScreePlotForKmean"),
                  br(),
                  br(),
                  uiOutput("renderSelectizeInput"),
                  br()
                  #h6("The values in the right are the Within-Cluster variance (WCV) corresponding to the number of Clusters (K)")
                ),
                box(
                  title = "Within Cluster Variable Table", solidHeader = TRUE,
                  width=6, status="info",
                  collapsible = TRUE, collapsed = FALSE,
                  DT::dataTableOutput("myWCVdataTable")
                )
              ),
              
              fluidRow(
                valueBoxOutput("boxForAggOverallRating", width=6),
                valueBoxOutput("boxForAggWLBalanceRating", width=6),
                valueBoxOutput("boxForAggBenefitsRating", width=6),
                valueBoxOutput("boxForAggCultureRating", width=6),
                valueBoxOutput("boxForAggJobSecurityRating", width=6),
                valueBoxOutput("boxForAggManagementRating", width=6)
              ),
              fluidRow(
                box(
                  title = "Comparison: Overall dataset vs. Selected cluster", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  plotOutput("renderRadarChart")
                )
              ),
              fluidRow(
                box(
                  title = "Filtered Cluster Data Table", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  DT::dataTableOutput("myFilteredClusterDataTable")
                )
              )
              
              
      ), # end of 2nd tabItem (myClusterAnalysis)
      
      # 3rd tab item
      tabItem("myCompReviewAnalysis", 
              fluidRow(
                
                box(
                  title = "Word Cloud (Positive Reviews)", solidHeader = TRUE,
                  width=6, status="primary",
                  collapsible = TRUE,
                  plotOutput("wordCloudPlotPositive")
                ),
                
                box(
                  title = "Word Cloud (Negative Reviews)", solidHeader = TRUE,
                  width=6, status="danger",
                  collapsible = TRUE,
                  plotOutput("wordCloudPlotNegative")
                )
              )  ,
              
              fluidRow(
                box(
                  title = "Company Review Analysis", solidHeader = TRUE,
                  width=12, status="warning",
                  collapsible = TRUE,
                  column(4, 
                         sliderInput("sliderForReviewRange", "Review Rating Range:", 
                                     min = 1.0, max = 5.0, value = c(3.0, 4.5), step = 0.1)
                  ),
                  column(4, 
                         sliderInput("sliderForReviewFreq", "Minimum Frequency:",
                                     min = 1,  max = 40, value = 15)
                  ),
                  column(4, 
                         sliderInput("sliderForReviewMaxWord", "Maximum Number of Words:", 
                                     min = 1,  max = 200,  value = 100)
                  ) 
                )  
              ),
              
              fluidRow(
                box(
                  title = "Company Review Analysis", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  DT::dataTableOutput("myCompReviewAnalysisDataTable")
                )
              )
              
      ) # end of 3nd tabItem (myCompReviewAnalysis)
      ,
      
      # 4th tab item
      tabItem("myDiceKeywordsAnalysis", 
              fluidRow(
                box(
                  title = "Company Keywords", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  
                  plotOutput("wordCloudPlotTopWordsResume")
                ),
                
                box(
                  title = "WordClouds Options", solidHeader = TRUE,
                  width=12, status="warning",
                  collapsible = TRUE,
                  column(6, 
                         sliderInput("sliderForCompKeywordsFreq", "Minimum Frequency:",
                                     min = 1,  max = 40, value = 15)
                  ),
                  column(6, 
                         sliderInput("sliderForCompKeywordsMaxWord", "Maximum Number of Words:", 
                                     min = 1,  max = 200,  value = 100)
                  ) 
                )  
                
                ,
                box(
                  title = "Company keywords Frequency", solidHeader = TRUE,
                  width=12, status="info",
                  collapsible = TRUE,
                  
                  DT::dataTableOutput("myCompKeywordsDataTable") 
                )
              )
      ),
      
      ##########################################################
      # Intro contents 
      ###########################################################
      tabItem("myTabForIntroduction",
              
              fluidRow(
                box(
                  title = "About the Application", solidHeader = TRUE,
                  status="info", width=12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           "This, ",
                           tags$span(      # Creates an HTML span.
                             tags$strong("Interactive Indeed DS Shiny App"), 
                             ", is the shiny dashboard application which embeds k-means cluster analysis and sentiment analysis of the web scraped jobs and companies data from Indeed.com and dice.com.

                             Implemented components include: ",
                             #designed to provide ways to better understand k-means clustering methods with similar companies based on company ratings and
                             #provide sentiment analysis of the most frequently mentioned keywords in the job descriptions. 
                             #The data was web-scrapped data from Indeed.com and Dice.com using Python BeautifulSoup. 
                             
                             tags$br(),
                             tags$li("Indeedice Datatable"), # 
                             tags$li("Indeedice Cluster Analysis"), # 
                             tags$li("Indeed Sentiment Analysis"), #
                             tags$li("Dice Keyword Analysis"), #
                             tags$br()
                           )
                         ),
                         br()
                  )
                )
              ),
              
              fluidRow(
                
                box(
                  title = "About Me", solidHeader = TRUE,
                  status="warning", width=12,collapsible = TRUE, 
                  column(12, 
                         #h3(""),
                         tags$div(
                           tags$span(      # Creates an HTML span.
                             tags$strong("Sung Moon"), 
                             #" who is a current data scientist fellow at NYC Data Science Academy and starting to transtion from academia to data science industry."),  
                             " is the author of this application."),
                           tags$br(),
                           h5("Here are the links to :"),
                           tags$li(tags$a(href="http://monspo1.github.io/resume.html", "My Resume")),
                           tags$li(tags$a(href="monspo1.github.io/", "My Portfolio homepage")),
                           tags$li(tags$a(href="https://www.linkedin.com/in/sung-pil-moon-44074939", "My LinkedIn profile")),
                           tags$li(tags$a(href="http://blog.nycdatascience.com/author/monspo1/", "My Blog at NYC Data Science Academy")),
                           tags$br()
                         ),
                         "If you have any suggestion, question, or reviews for this app? Comments are open. Also, if any of the info above is incorrect or needs to be updated, please send an email to ",
                         tags$a(href="mailto:monspo1@gmail.com", "monspo1@gmail.com")
                         
                  )
                ),
                br()
              )
      )
      
    ) # end of tabItems
    
  ), skin = "yellow"
)
