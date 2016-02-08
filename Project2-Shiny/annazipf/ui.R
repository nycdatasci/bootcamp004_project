library(shiny)
library(leaflet)


shinyUI(navbarPage("Medicare doctors in the USA", id="nav",

                   
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Medicare explorer"),
                    
                    selectInput("specialty", "Select a specialty", names, multiple = TRUE),
                    selectInput("state", "State", stateselect, selected = "NY", multiple = TRUE),
                    checkboxGroupInput('sex', "Gender", gender, 
                                       selected = gender)   , 
                    plotOutput("timep", height = 250)
              
      )
    )
  ),
  
  tabPanel("Find a doctor",
           fluidRow(
             column(3,
selectInput("statestab", "States", stateselect, selected = 'NY', multiple=FALSE)
             ),
             column(3,
         conditionalPanel("City",
selectInput("cities", "Cities", cities ,  multiple=TRUE)
                    )
             ),
             column(3,
selectInput("specials", "Specialty", names, multiple=TRUE)
             ),
            column(3,
       selectInput("medschool", "Medical school", medschool, multiple=TRUE)
              ),
              hr(), 
            DT::dataTableOutput("drtable")
           )
        ) ,


tabPanel("Density map",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ), 
                        leafletOutput("denmap", width="100%", height="100%") ) ),


tabPanel("Stats",
         div(class="outer",
            plotOutput("plot", height = 350),
            plotOutput("schoolp", height = 350)
             ) ),

tabPanel("Changes/Disparity",
         div(class="outer",
             plotOutput("timespp", height = 350),
             plotOutput("genderp", height = 350)
         ) ),
tabPanel("Stability",
         div(class="outer",
             plotOutput("changep", height = 350),
             plotOutput("changepisolate", height = 350)
         ) )
# tabPanel("Fun plots",
#          div(class="outer",
#              selectInput("specialplot", "Select a specialty", names, multiple = FALSE),
#              plotOutput("selfplot", height = 500)
#          ) )

  
)
  )


