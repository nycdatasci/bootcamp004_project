library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("NY Times API "),
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
       radioButtons("radio", label = h3("Select "),
            choices = list("Original Hit" = 1, "Hit normalized" = 2,"Hit About" = 3), 
            selected = 1), 
  
        textInput("text", label = h4("Search input"), value = ""),
        #actionButton("goButton", "Enter"),
        numericInput("year", label = h4("Ending Year:"), 2016, min = 1945, max = 2016),
        actionButton("goButton", "Enter")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
      numericInput("yearMonth", label = h4("Year for month:"), 2016, min = 1945, max = 2016),
      actionButton("yearMonthButton", "Enter")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
         numericInput("yearInterest", label = h4("The Year of interest:"), 2016, min = 1945, max = 2016),
         radioButtons("radioMonth", label = h4("Select"),
                      choices = list("By Year" = 1, "By Month" = 2),
                      selected = 1),
         conditionalPanel(condition = "input.radioMonth== 2",
                      numericInput("monthInterest", label = h4("The month of interest"), 01, min = 1, max = 12)
                
          ),
         numericInput("pageInput", label = h4("Type the page"), 01, min = 1, max = 1000),
         actionButton("goArticle", "Enter")
    ),
    conditionalPanel(condition="input.conditionedPanels==4",
                     helpText("This is incoming java script object")
    ),
    conditionalPanel(condition="input.conditionedPanels==5",
                     helpText("Content Panel 4")
    ),
    conditionalPanel(condition="input.conditionedPanels==6",
                     helpText("Content Panel 5")
    ),
    conditionalPanel(condition="input.conditionedPanels==7",
                     helpText("Content Panel 5")
    ),
    conditionalPanel(condition="input.conditionedPanels==8",
                     tags$textarea(id="typeAdd", rows=3, cols=40, ""),
                     #textInput("typeJob", label = h5("Search job"), value = ""),
                     actionButton("goLonLat", "Enter")
    )
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("NY Times Article API", value=1, plotOutput("graphHit") ), #textOutput("textAll"), dataTableOutput
      tabPanel("NY Times Article API  by Month", value=2, plotOutput("graphMonth") ),
      tabPanel("NY Times Article Example", value=3, dataTableOutput("articleExam") ),
      tabPanel("NY Times raw JavaScript Ojbects", value=4, textOutput("rawAPI") ),
      tabPanel("NY Times raw JavaScript Code", value=5, textOutput("codeText") ),
      tabPanel("Foursquare Search", value=6, textOutput("test02")),
      tabPanel("Presidential election finance", value=7, textOutput("test03")),
      tabPanel("Job Search", value=8, dataTableOutput("textGeo")),
      id = "conditionedPanels"
    ),# end of tabsetPanel
    tags$a(href="http://github.com/nycdatasci/bootcamp004_project/blob/master/Project3-WebScraping/WansagLim/prac05/ui.R", "ur.R Link"),
    tags$br(),
    tags$a(href="https://github.com/nycdatasci/bootcamp004_project/blob/master/Project3-WebScraping/WansagLim/prac05/server.R", "server.R Link"),
    tags$br(),
    tags$a(href="http://www.billboard.com/charts/hot-100", "Billboard Singles")
  )
))