
library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("NYC Job Posting Day"),
  
  # Sidebar with a slider input for number of bins
  radioButtons("radio", label = h3("Whole or Part"),
               choices = list("Whole Histogram" = 1, "Histogram by Level" = 2,
                              "Outlier or Not" = 3, "Salary by Time" = 4), 
               selected = 1),  
  
  sidebarLayout(
    sidebarPanel( 
      conditionalPanel(condition = "input.radio == 1",
        sliderInput("bins",
          "Number of bins:",
           min = 1,
           max = 50,
           value = 30),
        selectInput("salary", "Choose a salary",
           choices = c("minimum", "maximum", "average"))
      ),
      conditionalPanel(condition = "input.radio == 2",
        selectInput("salaryByLevel", "Choose a salary",
        choices = c("minimum", "maximum", "average"))
      ),
      conditionalPanel(condition = "input.radio == 3",
        selectInput("OutNot", "Choose outlier or not",
        choices = c("Outlier", "No Outlier"))
      ),
      conditionalPanel(condition = "input.radio == 4",
         radioButtons("radioTime", label = h3("Whole or Part"),
                      choices = list("Whole Data" = 1, "Part Data" = 2 ), 
                      selected = 1),
         #selectInput("wholePart", "Choose whole data or part data",choices = c("whole", "part")),
         sliderInput("rowNum",
                     "Start Year/Month:",
                     min = 1,
                     max = 29,
                     value = 1),
         selectInput("salaryTime", "Choose a number or salary",
                     choices = c("total number", "minimum", "maximum", "average"))
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
