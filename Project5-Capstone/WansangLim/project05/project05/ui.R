library(shiny)


shinyUI(fluidPage(
  headerPanel("Conditional Panels"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     textInput("twitSearch", label = h5("Search Twitter"), value = ""),
                     textInput("twitLocation", label = h5("Twite locatoin"), value = ""),
                     numericInput("locationRange", "mile range", 50, min = 10, max= 200),
                     numericInput("NumberTwitts", "Number of Twitts", 10, min = 5, max = 1000),
                     actionButton("goButton", "Enter")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText("Content Panel 2"),
                     numericInput("obs", "Observations:", 1),
                     radioButtons("Select", "Posi or Nega",
                                  c("positive" = "positive", "negative" = "negative"), selected = "positive")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     helpText("Content Panel 3")
    ), 
    conditionalPanel(condition="input.conditionedPanels==4",
                     textInput("inputLocation", label = h5("Compare Location"), value = ""),
                     actionButton("goLocation", "Enter"),
                     helpText("Content Panel 4")
    ),
    conditionalPanel(condition="input.conditionedPanels==5",
                     helpText("Content Panel 5")
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Panel 1", value=1, dataTableOutput("twitSearch")), 
      tabPanel("Panel 2", value=2, textOutput("classify")),
      tabPanel("Panel 3", value=3, textOutput("senti")),
      tabPanel("Panel 4", value=4, textOutput("BigCity")),
      tabPanel("Panel 5", value=5, textOutput("")),
      id = "conditionedPanels"
    )
  )
  
))