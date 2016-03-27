library(shiny)
library(leaflet)
library(DT)


states <- data.frame(state.name, state.x77)
choice <- names(states)[-1]

shinyUI(
  fluidPage(
  headerPanel("Wild American Ginseng Analysis"),
  sidebarPanel(
    width = 2,
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("Where were these samples collected?")
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     helpText("Chemical Analysis"),
                     fileInput('chemFile', 'Choose file to upload',
                               accept = c(
                                 'text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                               )
                     ) 
    ),
    conditionalPanel(condition="input.conditionedPanels==4",
         fileInput('file1', 'Choose file to upload',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   )
          ) ,
         # tags$hr(),
         #checkboxInput('header', 'Header', TRUE),
         # radioButtons('sep', 'Separator',
         #              c(Comma=',', Semicolon=';', Tab='\t'), ','),
         # radioButtons('quote', 'Quote',
         #              c(None='','Double Quote'='"','Single Quote'="'"),'"'),
         # tags$hr(),
         radioButtons('bestWholeVec', 'Choose Best Vector or Whole', 
                      c("Best10" = "Best10", "Whole" = "Whole"), selected = "Whole"),
         
         numericInput("randomSet", "Set set.seed:", 0),
         numericInput("svmCost", "Set cost:", 1, min = 0.0001, max = 1000000),
         fileInput('customer', 'Choose file to upload',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     'text/tab-separated-values',
                     'text/plain',
                     '.csv',
                     '.tsv'
                   )
         )
    ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Sample Collection", value=1, 
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
                 #includeScript("gomap.js")
               ),
               leafletOutput("mymap", width = "1000px", height = "700px"), #p()
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 110, left = "auto", right = 20, bottom = "auto",
                     width = 330, height = "auto",

                     h3("Sample Number"),
                     plotOutput("sampleHIs", height = 200),
                     imageOutput("ginImage01", width = "100%", height = "100%", click = NULL, 
                        dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                        brush = NULL, clickId = NULL, hoverId = NULL, inline = FALSE)
       )
      ),# End of tabPanel,
      tabPanel("Preliminary Analaysis", value=2,
               imageOutput("title", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = FALSE),
               imageOutput("anova", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = TRUE),
               tags$hr(),
               imageOutput("soil", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = TRUE),
               tags$hr(),tags$hr(),tags$hr(),tags$hr(),tags$hr(),
               imageOutput("mediTitle", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = TRUE),
               imageOutput("gelPhoto", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = TRUE),
               imageOutput("gelPCA", width = "100%", height = "100%", click = NULL, 
                           dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
                           brush = NULL, clickId = NULL, hoverId = NULL, inline = TRUE),
               dataTableOutput("rawMole")
      ),#End of tabPael 
      ##########################################################################33
      tabPanel("Phyto chemical analysis", value=3,
               helpText("Where were these samples collected?"),
               textOutput("chemGLM")
      ),#End of tabPael 
      tabPanel("Molecular Analysis", value=4,
               fluidRow(
                 column(width = 8,
                        dataTableOutput("bestVector"),
                        dataTableOutput("vecFirstTable"),
                        textOutput("customerMol")
                 ),
                 column(width = 3, #offset = 2,
                        "Percent of Correctly Predict",
                        textOutput("hitRate"),
                        h4("%", style = "color: red; display:inline "),
                        textOutput("summary01"),
                        tags$head(tags$style("#hitRate{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                )
                        
                        ),# End of tags$head,
                        textOutput("test01")
                 )# End of column
               )
               
      ),#End of tabPael   
      id = "conditionedPanels"
    )#End of tabsetPanel
  )#End of Main Panel
  
))