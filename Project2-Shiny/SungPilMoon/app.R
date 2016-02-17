###############################################################################
# [ShinyApp: Salary Comparator]
# by Sung Pil Moon
###############################################################################
# cat('\014')
# setwd("~/Documents/Dropbox/DataScience/Projects/Project 2 - SalExp")

load('proj2_SungMoon_PrepareData.RData')

library(dplyr)
library(ggplot2)
library(googleVis)
library(ggvis)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)
Sys.setlocale('LC_ALL','C') 
options(shiny.trace=F)


header <- dashboardHeader(
  title = "DS Salary Explorer"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "myTabForIntroduction", icon = icon("fa fa-compass")),
    menuItem("Salary Scatter Plot", tabName = "myTabForScatterPlot", icon = icon("bar-chart-o")),
    menuItem("Salary Data Explorer", tabName = "myTabForDataTable", icon = icon("fa fa-table")),    
    menuItem("Salary Comparison Map", tabName = "myTabForGvisMap", icon = icon("fa fa-map-marker")),
    menuItem("Top Recruiters", tabName = "myTabForRecruitRanking", icon = icon("fa fa-list-ol")),
    menuItem("External Info", tabName = "myTabForExternalInfo", icon = icon("fa fa-external-link"))
  )
) 

body <- dashboardBody(
  
  tabItems(
    
    tabItem("myTabForIntroduction",
            
            fluidRow(
              box(
                title = "About the Application", solidHeader = TRUE,
                status="info", width=12, collapsible = TRUE,
                column(12, 
                  # h4("This, Data Science Salary Comparator, is the shiny application designed to explore and compare salary data among 8 professions:"),
                  tags$div(
                    "This, ",
                    tags$span(      # Creates an HTML span.
                      tags$strong("Data Science Salary Comparator"), 
                      ", is the shiny dashboard application designed to explore and compare salary data among 8 professions, including:",
                      tags$li("Data Scientist"), 
                      tags$li("Software Engineer"), 
                      tags$li("Data Analyst"), 
                      tags$li("Business Analyst"), 
                      tags$li("Management Consultant"), 
                      tags$li("Assistant Professor"), 
                      tags$li("Attorney"), 
                      tags$li("Teacher! "),
                      #tags$br(),
                      "* Please note that the data is from the United States Department of Labor, Employment & Training Administration and based on the prevailing wage data of foreign employers (The prevailing wage data of US natives are not included)",
                      tags$br()
                    )
                  ),
                  br()
                )
              )
            ),
            fluidRow(
              box(
                title = "About The Data Set", solidHeader = TRUE,
                status="primary", width=12, collapsible = TRUE, collapsed = TRUE,
                column(12, 
                       tags$div(
                         "This data set is from the ",
                         tags$span(      # Creates an HTML span.
                          tags$strong("United States Department of Labor, Employment & Training Administration. "), 
                           "It is about the prevailing wage data of foriegn employers seeking to file applications in the Permanent Labor Certification Program (PERM), the H-1B, H-1B1, and E-3 Professional and Specialty Occupation Programs, and the H-2B Non-agricultural Temporary Labor Certification Program.",
                          tags$li("Source: https://www.foreignlaborcert.doleta.gov/performancedata.cfm"),
                          tags$li("The filtered data for this application contains total 167,278 cases (in 19 columns) in 2015"),
                          br()
                         )
                       )
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
                         "I am ",
                         tags$span(      # Creates an HTML span.
                           tags$strong("Sung Pil Moon"), 
                           " who is a current data scientist fellow at NYC Data Science Academy and starting to transtion from academia to data science industry."),  
                           h5("Here are the links to :"),
                           tags$li(tags$a(href="http://monspo1.github.io/resume.html", "My Resume")),
                           tags$li(tags$a(href="monspo1.github.io/", "My Portfolio homepage")),
                           tags$li(tags$a(href="https://www.linkedin.com/in/sung-pil-moon-44074939", "My LinkedIn profile")),
                           tags$li(tags$a(href="http://blog.nycdatascience.com/author/monspo1/", "My Blog at NYC Data Science Academy")),
                           tags$br()
                         ),
                       "If you have any suggestion, question, or reviews for this app? Comments are open. Also, if any of the info above is incorrect or needs to be updated, please send an email to monspo1@gmail.com and reference this post's URL."
                        
                       )
                ),
                br()
              )
    ),
    tabItem("myTabForScatterPlot",
            h2("Salary Data Scatter Plot"),
            
            fluidRow(
              box(
                title = "How to use (Toggle the + button)", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h4("* Please be patient that it usually takes 10 seconds to load the scatter plot"),
                br(),
                h5("This 'Salary Scatter Plot' panel shows the salary distribution by 8 different jobs. It comprises of three sections: an option input section, a plot area section, and an aggreate summary box section."),
                br(),
                h5("The plot area basically shows two types of visualizations: a scatter plot showing all the salary data by 8 professions, and a box plot showing values of minimum, 25 percent quintile, median, 75 percent quintile, and maximum. Users can toggle the 'showing data points' option above the plot so that they can only see the boxplot alone. Users also can interactively change the options of the target states (All or one state among 50), and target salary range. Corresponding changes are updated on the plot area and the aggregate summary boxes as soon as users made any change."),
                br(),
                h5("Salary data of assistant professor are in red color, attroney salary are in orange color, business analyst salary are in light green color, data analyst salary are in green color, data scientist salary are in teal color , management consultant salary are in turkey blue color, software engineer salary are in purple and teacher salary are in red violet color. ")
              )
            ),
            
            fluidRow(
              column(4, 
                     selectizeInput('singleSelectForStatesForScatterPlot', 'States:', 
                                    c("All"= '', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = F)
                     ),
              column(4, 
                     sliderInput("sliderForSalaryRangeForScatterPlot", "Salary Range:", 
                                 min = 0, max = 350000, value = c(12000, 250000), step = 5000)
                     ),
              column(4, checkboxInput("checkboxForShowDataPoint", label = "Show data points", value = TRUE))
            ),
            br(),
            fluidRow( plotOutput("myQScatterChart") ), 
            #fluidRow( htmlOutput("mySummaryForScatterPlot") ) 
            #fluidRow(verbatimTextOutput("mySummaryForScatterPlot"))
            
            br(),
            fluidRow(
              # Dynamic infoBoxes
              infoBoxOutput("minBoxInScatterSummary"),
              infoBoxOutput("medBoxInScatterSummary"),
              infoBoxOutput("maxBoxInScatterSummary")
            ),
            fluidRow(
              infoBoxOutput("q1BoxInScatterSummary"),
              infoBoxOutput("meanBoxInScatterSummary"),
              infoBoxOutput("q3BoxInScatterSummary")
            ) 
    ),
    
    tabItem("myTabForDataTable",
            h2("DS Sarary Data Explorer"),
            
            fluidRow(
              box(
                title = "How to use (Toggle the + button)", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("This 'Salary Data Explorer' is a data table having features of filtering, paginating, searching, and sorting to explore the data of your interests. You can interactively choose the options, then the table shows updated result. The data of the table can be filtered by profession (multiple choices), state, salary range, and name (of city and employer)"),
                br()
              )
            ),
            
            fluidRow(
              column(3, checkboxInput("checkboxForDS", label = "Data Scientist", value = TRUE) ),
              column(3, checkboxInput("checkboxForSW", label = "Software Engineer", value = TRUE)),
              column(3, checkboxInput("checkboxForDA", label = "Data Analyst", value = TRUE)),
              column(3, checkboxInput("checkboxForBA", label = "Business Analyst", value = TRUE))
            ),
            fluidRow(
              column(3, checkboxInput("checkboxForAP", label = "Assistant Professor", value = TRUE)),
              column(3, checkboxInput("checkboxForMC", label = "Management Consultant", value = TRUE)),
              column(3, checkboxInput("checkboxForAT", label = "Attorney", value = TRUE)),
              column(3, checkboxInput("checkboxForTC", label = "Teacher", value = TRUE))
            ),
            fluidRow(
              column(6,
                     selectizeInput('multiSelectForStates', 'States:', 
                                    c("Choose multiple"= '', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = TRUE
                     )
              ),
              column(6, 
                     sliderInput("sliderForSalaryRange", "Salary Range:", 
                                 min = 0, max = 350000, value = c(12000, 250000), step = 5000)
              )
            ),
            
            fluidRow(
              column(6, textInput("searchInputForCity","City Search:","") ),
              column(6, textInput("searchInputForEmployer","Employer Name Search:",""))
            ),
            fluidRow(br()),
            fluidRow(
              DT::dataTableOutput("myTable")
            )
    ),
    
    tabItem("myTabForGvisMap",
            h2("Salary Comparison Map"),
            
            fluidRow(
              box(
                title = "How to use (Toggle the + button)", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("The Salary Comparison Map provides a way to compare salary distribution of two professions in the United States. You can choose two professions (job titles), then the distribution map and data table will show the updated result. You can also sort the results in the table by state, average salary and the number of jobs."),
                br(),
                h5("Note that when the panel is initialized, all the data which is not filtered yet by state, profession, average salary and the number of jobs."),
                br()
              )
            ),
            
            fluidRow(
              column(6, 
                     selectizeInput('singleSelectForJobTitleForComparison1', 'Choose the 1st Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     )),
              column(6, 
                     selectizeInput('singleSelectForJobTitleForComparison2', 'Choose the 2nd Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     ))
            ),
            fluidRow(
              box(
                title = "Map 1", solidHeader = TRUE,
                collapsible = TRUE, 
                htmlOutput("myGvisMap1") 
                
              ),
              box(
                title = "Map 2", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("myGvisMap2") 
              )
            ),
            fluidRow(
              box(
                title = "DataTable for Map 1", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle1")
                
              ),
              box(
                title = "DataTable for Map 2", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle2")
              )
            )
    ),
    
    tabItem("myTabForRecruitRanking",
            h2("Top Recruiters"),
            
            fluidRow(
              box(
                title = "How to use (Toggle the + button)", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("The Top Recruiter Tables panel comprises of 5 salary data tables showing who the top recruiters are for each profession. Each table contains employer names, the number of jobs, average salary, the minimum salary, the 25% quantile salary, median salary, the 75% quantile salary, and the maximum salary. "),
                br(),
                h5("The first table intentionally shows the salary data without distinguishing the profession to provide an overall idea who the top recruiters are regardless of a profession across the United States. "),
                br(),
                h5("However, other four remaining tables provide summary tables filtering options by states and specific professions: data scientist, software engineer, data analyst, and other professions. (The tables are sorted by the number of jobs and the average salary in descending order."),
                br()
              )
            ),
            
            fluidRow(
              box(
                title = "Top Recruiters (All positions)", solidHeader = TRUE,
                width=12, status="info",
                collapsible = TRUE,
                DT::dataTableOutput("myTableForOverallRank")
              )
            ),
            fluidRow(
              box(
                title = "Top Recruiters for Data Scientist", solidHeader = TRUE,
                status="primary", width=12, collapsible = TRUE, collapsed = TRUE,
                column(4, 
                       selectizeInput('singleSelectForStatesForTopRecruiterDS', 'States:', 
                                      c("All", "Alabama", "Alaska","Arizona", "Arkansas", 
                                        "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                                        "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
                                        "Indiana", "Iowa","Kansas", "Kentucky", "Louisiana", "Maine",
                                        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                        "New Jersey", "New Mexico", "New York","North Carolina", "North Dakota",
                                        "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
                                        "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
                                        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                        "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                        "Wyoming"),
                                      multiple = F)
                ),
                DT::dataTableOutput("myTableForDataScientistRank")
              ) 
            ),
            fluidRow(
              box(
                title = "Top Recruiters for Software Engineer", solidHeader = TRUE,
                width=12, status="info", collapsible = TRUE, collapsed = TRUE,
                column(4, 
                       selectizeInput('singleSelectForStatesForTopRecruiterSW', 'States:', 
                                      c("All", "Alabama", "Alaska","Arizona", "Arkansas", 
                                        "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                                        "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
                                        "Indiana", "Iowa","Kansas", "Kentucky", "Louisiana", "Maine",
                                        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                        "New Jersey", "New Mexico", "New York","North Carolina", "North Dakota",
                                        "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
                                        "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
                                        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                        "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                        "Wyoming"),
                                      multiple = F)
                ),
                DT::dataTableOutput("myTableForSoftwareRank")
              )
            ),
            fluidRow(
              box(
                title = "Top Recruiters for Data Analyst", solidHeader = TRUE,
                width=12, status="primary", collapsible = TRUE, collapsed = TRUE,
                column(4, 
                       selectizeInput('singleSelectForStatesForTopRecruiterDA', 'States:', 
                                      c("All", "Alabama", "Alaska","Arizona", "Arkansas", 
                                        "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                                        "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
                                        "Indiana", "Iowa","Kansas", "Kentucky", "Louisiana", "Maine",
                                        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                        "New Jersey", "New Mexico", "New York","North Carolina", "North Dakota",
                                        "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
                                        "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
                                        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                        "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                        "Wyoming"),
                                      multiple = F)
                ),
                DT::dataTableOutput("myTableForDataAnalystRank")
              )
            ),
            fluidRow(
              box(
                title = "Top Recruiters for Other Job Titles", solidHeader = TRUE,
                width=12, status="info", collapsible = TRUE, collapsed = TRUE,
                column(4, 
                       selectizeInput('singleSelectForStatesForTopRecruiterOT', 'States:', 
                                      c("All", "Alabama", "Alaska","Arizona", "Arkansas", 
                                        "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                                        "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
                                        "Indiana", "Iowa","Kansas", "Kentucky", "Louisiana", "Maine",
                                        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                        "New Jersey", "New Mexico", "New York","North Carolina", "North Dakota",
                                        "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
                                        "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
                                        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                        "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                        "Wyoming"),
                                      multiple = F)
                ),
                DT::dataTableOutput("myTableForOthersRank")
              )
            )
    ),
    
    tabItem("myTabForExternalInfo",
            h2("External sources"),
            
            fluidRow(
              box(
                title = "How to use (Toggle the + button)", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("This External Resources panel shows a collection of valuable and meaningful information from external sources. The external resources are embedded or regenerated for better readibility and interactivity. (All sources and author names of the external resources are included)"),
                br()
              )
            ),
            
  
            fluidRow(
              box(
                title = "Top Recruiter rank (by Data Science Centeral)", solidHeader = TRUE,
                tags$div(
                  "Source from ",
                  tags$span(      
                    tags$a(href="http://www.datasciencecentral.com/profiles/blogs/7500-companies-hiring-data-scientists", "7500 companies hiring data scientists"),
                    "by Vincent Granville on January 26, 2015, Data Science Centeral",
                    tags$br(),
                    tags$br()
                  )
                ),
                status="info", collapsible = TRUE, width=12, 
                DT::dataTableOutput("myTableForExternalLinkDSC")
              )
            )
    )
  )
)

server <- function(input, output) { 
 
  options("scipen"=10) 
  
  ##########################################################
  # Data manipulation (for Salary DataTable)
  ###########################################################
  updateInputDataForDTable <- reactive({  

    dataFilteredForDTable <- salary_refined
    
    #dataTmp <- dataFilteredForDTable[1:500, ]
    #dataTmp <- dataTmp %>% group_by(JOB_TITLE_SUBGROUP)
  
    if(input$checkboxForDS != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data scientist"),]        
    } 
    if(input$checkboxForSW != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "software engineer"),]        
    } 
    if(input$checkboxForDA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "data analyst"),]        
    } 
    if(input$checkboxForBA != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "business analyst"),]        
    } 
    if(input$checkboxForAP != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "assistant professor"),]        
    } 
    if(input$checkboxForMC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "management consultant"),]        
    } 
    if(input$checkboxForAT != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "attorney"),]        
    } 
    if(input$checkboxForTC != T){
      dataFilteredForDTable <- dataFilteredForDTable[(dataFilteredForDTable$JOB_TITLE_SUBGROUP != "teacher"),]        
    } 
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStates) ){
      targetStates <- unlist(strsplit(input$multiSelectForStates," "))
      dataFilteredForDTable <- dataFilteredForDTable %>% filter(WORK_STATE_ABBREVIATION %in% targetStates)
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCity != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForCity,dataFilteredForDTable$WORK_CITY, ignore.case = TRUE)) 
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Employer (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForEmployer != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForEmployer, dataFilteredForDTable$EMPLOYER_NAME, ignore.case = TRUE)) 
    }
    
    
    dataFilteredForDTable
  })
  
  
  ##########################################################
  # Data manipulation (for Salary Scatter Plot)
  ###########################################################
  updateInputDataForScatterPlot <- reactive({  
    dataFilteredForScatterPlot <- salary_refined
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot %>% group_by(JOB_TITLE_SUBGROUP) 
    
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$sliderForSalaryRangeForScatterPlot[1] <= dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR &
                                                              dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR <= input$sliderForSalaryRangeForScatterPlot[2]),]
    
    if(input$singleSelectForStatesForScatterPlot != ""){
      dataFilteredForScatterPlot <<- dataFilteredForScatterPlot[(input$singleSelectForStatesForScatterPlot == dataFilteredForScatterPlot$WORK_STATE_ABBREVIATION),]
    }
    
    dataFilteredForScatterPlot <<- dataFilteredForScatterPlot %>% mutate(JOB_GROUP_CODE = ifelse(JOB_TITLE_SUBGROUP == "assistant professor", 1,
                                                        ifelse(JOB_TITLE_SUBGROUP == "attorney", 2,
                                                               ifelse(JOB_TITLE_SUBGROUP == "business analyst", 3,
                                                                      ifelse(JOB_TITLE_SUBGROUP == "data analyst", 4,
                                                                             ifelse(JOB_TITLE_SUBGROUP == "data scientist", 5,
                                                                                    ifelse(JOB_TITLE_SUBGROUP == "management consultant", 6,
                                                                                           ifelse(JOB_TITLE_SUBGROUP == "software engineer", 7,
                                                                                                  ifelse(JOB_TITLE_SUBGROUP == "teacher", 8)))))))))
    
    
    manualQuartile <- function(x) {
      x <- sort(x)
      n <- length(x)
      m <- (n+1)/2
      if (floor(m) != m) { l <- m-1/2; u <- m+1/2
      } else { l <- m-1; u <- m+1 }
      c(Min=min(x), Q1=median(x[1:l]), Median = median(x[1:n]), Mean=mean(x), Q3=median(x[u:n]), Max=max(x))
    }
    res_mq <<- manualQuartile(dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR)
    #cat(res_mq['Min']) # Print --> 10504
    #res_mq['Min'] # Print --> Min 10504 
    
    
    output$minBoxInScatterSummary <- renderInfoBox({
      infoBox( "MIN:", res_mq['Min'], icon = icon("fa fa-exclamation-circle"), color = "blue" ) })
    
    output$meanBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEAN:", res_mq['Median'], icon = icon("fa fa-info-circle"), color = "teal" ) })
    
    output$maxBoxInScatterSummary <- renderInfoBox({ 
      infoBox( "MAX:", res_mq['Max'], icon = icon("fa fa-exclamation-circle"), color = "aqua" ) })
      
    output$q1BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q1:", res_mq['Q1'], icon = icon("fa fa-exclamation-circle"), color = "blue" ) })
    
    output$medBoxInScatterSummary <- renderInfoBox({
      infoBox( "MEDIAN:", res_mq['Median'], icon = icon("fa fa-info-circle"), color = "teal" ) })
    
    output$q3BoxInScatterSummary <- renderInfoBox({
      infoBox( "Q3:", res_mq['Q3'], icon = icon("fa fa-exclamation-circle"), color = "aqua" ) })
    
    
    dataFilteredForScatterPlot
  })
  
  
  ##########################################################
  # Data manipulation (for Salary Comparison Maps)
  ###########################################################
  
  #////////////////////////////////////////////////////////////////////////////////
  # For Avg.Overall Salary (NOT useful) ex. Avg.salary of each state.. NOT our interest.
  #////////////////////////////////////////////////////////////////////////////////
  updateInputDataForMapOverall <- reactive({  
    dataFilteredForMap <- salary_refined
    dataFilteredForMap <- dataFilteredForMap %>% group_by(WORK_STATE) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2))
    dataFilteredForMap
    
  })

  
  updateInputDataForMapByJobTitle1 <- reactive({  
    dataFilteredForMapByJobTitle1 <- salary_refined
    dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())

    if(input$singleSelectForJobTitleForComparison1 != ""){
      dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1[(input$singleSelectForJobTitleForComparison1 == dataFilteredForMapByJobTitle1$JOB_TITLE_SUBGROUP),]
    }
    
    dataFilteredForMapByJobTitle1
  })
  
  updateInputDataForMapByJobTitle2 <- reactive({  
    dataFilteredForMapByJobTitle2 <- salary_refined
    dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison2 != ""){
      dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2[(input$singleSelectForJobTitleForComparison2 == dataFilteredForMapByJobTitle2$JOB_TITLE_SUBGROUP),]
    } 
    
    dataFilteredForMapByJobTitle2
  })

  
  
  
  ####################################################################################################################
  # Rendering Section
  #####################################################################################################################

  #////////////////////////////////////////////////////////////////////////////////
  # DataTable
  #////////////////////////////////////////////////////////////////////////////////
  # http://rstudio.github.io/DT/extensions.html
  output$myTable <- DT::renderDataTable(DT::datatable({ 
      dataForDTable <- updateInputDataForDTable()
      #dataForDTable <- dataForDTable[, c(1,2,4,5,7,9,11:17)]
      # colnames(dataForDTable) <- c("JOB_TITLE_GROUP","JOB_TITLE","EMPLOYER", "WAGE_YEARLY", "CITY", "STATE", 
      #                          "FULLTIME?", "REQ_EXPERIENCE?", "REQ_EXP_MONTH", "REQ_EDU_LEVEL", "REQ_UNIV_MAJOR?", "VISA", "COUNTRY_OF_CITIZENSHIP")
      dataForDTable <- dataForDTable[, c(1,2,4,5,7,9,12,14:15)]
      colnames(dataForDTable) <- c("JOB_TITLE_GROUP","JOB_TITLE","EMPLOYER", "WAGE_YEARLY", "CITY", "STATE", "REQ_EXPERIENCE?", "REQ_EDU_LEVEL", "REQ_UNIV_MAJOR")
      
      dataForDTable
      
    }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
        deferRender = TRUE,  
        searching = F,
        dom = 'RC<"clear">lfrtip',
        colVis = list(activate = 'mouseover'),
        lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
          "}")
    ) 
  ) %>% formatCurrency(c('WAGE_YEARLY'), "$") ) 
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  output$myGvisMap1 <- renderGvis({
    
    mapData <- updateInputDataForMapByJobTitle1() # View(mapData)
    gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                 options=list(region="US", displayMode="regions", resolution="provinces", 
                              width="100%", #height=200, 
                              colorAxis="{colors:['#36648b', '#9a162c']}",
                              backgroundColor="gray"
                 )
    )
  })  
  
  output$myGvisMap2 <- renderGvis({
    
    mapData <- updateInputDataForMapByJobTitle2() # View(mapData)
    gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                 options=list(region="US", displayMode="regions", resolution="provinces", 
                              width="100%", #height=400, 
                              #colorAxis="{colors:['#91BFDB', '#FC8D59']}",
                              colorAxis="{colors:['#5adbf2', '#fe9128']}",
                              backgroundColor="gray"
                              #colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 )
    )
  })  
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # DataTables for googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  output$myComparisonTableByJobTitle1 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable1 <- updateInputDataForMapByJobTitle1()
    colnames(dataForDTable1) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable1
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  output$myComparisonTableByJobTitle2 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable2 <- updateInputDataForMapByJobTitle2()
    colnames(dataForDTable2) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable2
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # ScatterPlot (qplot)
  #////////////////////////////////////////////////////////////////////////////////
  output$myQScatterChart <- renderPlot({
    
    dataForScatterPlot <- updateInputDataForScatterPlot()
    
    if(input$checkboxForShowDataPoint == T){
      qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the number of fans") + 
        geom_jitter(position=position_jitter(width=.9), size=1, alpha=.3) +  
        theme(legend.position="none")  
    } else{
      qplot(JOB_TITLE_SUBGROUP, PAID_WAGE_PER_YEAR, data=dataForScatterPlot, geom="boxplot", group = JOB_TITLE_SUBGROUP, color=JOB_TITLE_SUBGROUP) + 
        labs(title = "Total review counts by the number of fans") + 
        theme(legend.position="none")
    }
  })
  
  
  ###########################################################
  # DataTables for Overall Recruitment Ranking
  ###########################################################
  output$myTableForOverallRank <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTableOverall <- rankingSummaryByEmployer
    dataForDTableOverall <- dataForDTableOverall %>% arrange(desc(JOBS))
    dataForDTableOverall
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  
  updateInputDataForDTableForDS <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterDS != "All"){
      dataForDTableByEmployerJobTitleDS <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleDS <- dataForDTableByEmployerJobTitleDS[(input$singleSelectForStatesForTopRecruiterDS == dataForDTableByEmployerJobTitleDS$WORK_STATE),]
      
    } else {
      dataForDTableByEmployerJobTitleDS <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleDS <- dataForDTableByEmployerJobTitleDS %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleDS
  })
  
  output$myTableForDataScientistRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleDS <- updateInputDataForDTableForDS()
    dataLocalForDTableByEmployerJobTitleDS <- dataLocalForDTableByEmployerJobTitleDS[dataLocalForDTableByEmployerJobTitleDS$JOB_TITLE_SUBGROUP == "data scientist",]
    dataLocalForDTableByEmployerJobTitleDS <- dataLocalForDTableByEmployerJobTitleDS %>% arrange(desc(JOBS))
    
    dataLocalForDTableByEmployerJobTitleDS # colnames(dataForDTableByEmployerJobTitleDS)
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 

  updateInputDataForDTableForSW <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterSW != "All"){
      dataForDTableByEmployerJobTitleSW <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleSW <- dataForDTableByEmployerJobTitleSW[(input$singleSelectForStatesForTopRecruiterSW == dataForDTableByEmployerJobTitleSW$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleSW <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleSW <- dataForDTableByEmployerJobTitleSW %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleSW
  })
  
  output$myTableForSoftwareRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleSW <- updateInputDataForDTableForSW()
    dataLocalForDTableByEmployerJobTitleSW <- dataLocalForDTableByEmployerJobTitleSW[dataLocalForDTableByEmployerJobTitleSW$JOB_TITLE_SUBGROUP == "software engineer",]
    dataLocalForDTableByEmployerJobTitleSW <- dataLocalForDTableByEmployerJobTitleSW %>% arrange(desc(JOBS))
    
    dataLocalForDTableByEmployerJobTitleSW # colnames(dataForDTableByEmployerJobTitleDS)
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
   
  
  updateInputDataForDTableForDA <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterDA != "All"){
      dataForDTableByEmployerJobTitleDA <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleDA <- dataForDTableByEmployerJobTitleDA[(input$singleSelectForStatesForTopRecruiterDA == dataForDTableByEmployerJobTitleDA$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleDA <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleDA
  })
  
  output$myTableForDataAnalystRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleDA <- updateInputDataForDTableForDA() 
    dataLocalForDTableByEmployerJobTitleDA <- dataLocalForDTableByEmployerJobTitleDA[dataLocalForDTableByEmployerJobTitleDA$JOB_TITLE_SUBGROUP == "data analyst",]
    dataLocalForDTableByEmployerJobTitleDA
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  
  updateInputDataForDTableForDA <- reactive({
    
    if(input$singleSelectForStatesForTopRecruiterOT != "All"){
      dataForDTableByEmployerJobTitleOT <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployerJobTitleOT <- dataForDTableByEmployerJobTitleOT[(input$singleSelectForStatesForTopRecruiterOT == dataForDTableByEmployerJobTitleOT$WORK_STATE),]
    } else {
      dataForDTableByEmployerJobTitleOT <- rankingSummaryByEmployerJobTitle        
    }
    
    dataForDTableByEmployerJobTitleOT <- dataForDTableByEmployerJobTitleOT %>% arrange(desc(JOBS))
    dataForDTableByEmployerJobTitleOT
  })
  output$myTableForOthersRank <- DT::renderDataTable(DT::datatable({ 
    
    dataLocalForDTableByEmployerJobTitleOT <- updateInputDataForDTableForDA() 
    dataLocalForDTableByEmployerJobTitleOT <- dataLocalForDTableByEmployerJobTitleOT[(
      dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "data scientist" &
        dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "software engineer" &
        dataLocalForDTableByEmployerJobTitleOT$JOB_TITLE_SUBGROUP != "data analyst"
    ),]
    dataLocalForDTableByEmployerJobTitleOT <- dataLocalForDTableByEmployerJobTitleOT %>% arrange(desc(JOBS))
    dataLocalForDTableByEmployerJobTitleOT
    
  }, rownames = FALSE, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 

  
  
  ###########################################################
  # DataTables for External Links
  ###########################################################
  output$myTableForExternalLinkDSC <- DT::renderDataTable(DT::datatable({ 
      dataForExternalLinkDSC <- externalData
      dataForExternalLinkDSC
      
  }, rownames = F, extensions = c('ColVis','ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    dom = 'RC<"clear">lfrtip',
    colVis = list(activate = 'mouseover'),
    lengthMenu = list(c(50, 5, 10, 15, 25, 25, 100), c('50', '5', '10', '15', '20', '25','100')),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  ))) 
  

}



##########################################################
# ShinyApp main function
###########################################################
ui = dashboardPage(header, sidebar, body, skin = "black")
shinyApp(ui, server)

# shinyApp(
#   ui = dashboardPage(header, sidebar, body, skin = "black"), # skin in c(???blue???, ???black???, ???purple???, ???green???, ???red???, ???yellow???)
#   server <- server
# )

# Fpr 
# app = shinyApp(
#   ui = dashboardPage(header, sidebar, body, skin = "black"),
#   server <- server
# )
# runApp(app, host = '0.0.0.0', port = 3168)

