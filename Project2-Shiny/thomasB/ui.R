library(shiny)
library(leaflet)

library(markdown)

setwd("/Users/boulenge/Desktop/Projects/Project2 - Shiny/Shiny-TomB")

#source("/Users/boulenge/Desktop/Projects/Project2 - Shiny/Shiny-TomB/helpers.R")
source("helpers.R")

 # Interactive map variables
 varInt <- c("Institution name" = "INSTNM", 
           "Proportion of Women" = "female",
           "Admission Rate" = "ADM_RATE", 
           "US born students" = "pct_born_us")
 #Map variables
 varM <- c("Average Family Income" = "faminc", 
             "Average Tuition" = "TUITFTE",
             "3 year Default Rate" = "CDR3", 
             "Median Loan debt" = "DEBT_MDN",
             "Predominant degree type" = "PREDDEG")
 varM2 = c('Avg.Income', 'Avg.Tuition', 
           'Avg.Default.Rate', 'Median.Loan.Debt', 'Avg.Degree.Type')

# edu200 base for State Averages variables
varSt = as.character(unique(states.map200$region))
# edu200 base for Overview variables
varCty = as.character(unique(edu200$CITY))
varSt2 = as.character(unique(edu200$STABBR))
# edu200 base for Explorer variables
# without the X column
numVar = c(1:11, 24:39)
groupVar = c(12:23)
#temp = edu200[, -c(1:6)]
# with the X column in left_join
temp = edu200[, -c(1:7)]
varT = colnames(temp[,numVar])
varG = colnames(temp[,groupVar])

#sample
varSamp = c("200", "500", "1000", "Full")





### ### ui ### ###

shinyUI(navbarPage("US Education",
                   tabPanel('State Averages', 
                            plotOutput("map"),
                            hr(),
                            inputPanel(
                              selectInput('varM', "Variable", varM),
                              selectizeInput('varSt', "Select a State", choices = varSt),
                              checkboxInput('varECZ', "East-Coast Zoom", value = FALSE),
                              #checkboxInput('varZip', "Zip code detail", value = FALSE),
                              selectizeInput('varSamp', "Select the sample size", choices = varSamp)
                                    ),
                            h4(htmlOutput('textMap'), align = "center")
                            ),
                   tabPanel("Data", dataTableOutput('varTab')),
                   tabPanel('Overview', 
                            leafletOutput("map2"),
                            inputPanel(
                            selectizeInput('varInt', "Information", varInt),
                            selectizeInput('varSt2', "Select a State", varSt2, multiple = TRUE),
                            textInput('varCty', "Select a City"),
                            selectizeInput('varSamp2', "Select the sample size", choices = varSamp)
                                      )
                            ),
                   tabPanel('Explorer', 
                            plotOutput("map4"),
                            hr(),
                            inputPanel(
                              selectInput('varT1', "Variable 1", varT),
                              selectInput('varT2', "Variable 2", varT),
                              selectInput('varTG', "Group variable", varG),
                              selectizeInput('varSamp3', "Select the sample size", choices = varSamp)
                            ),
                            hr(),
                            helpText(
                              h4(htmlOutput("textExpl"), align = "center")
                            ),
                            hr(),
                            inputPanel(
                              uiOutput("slider1"),
                              br(),
                              uiOutput("slider2")
                            )
                   ),
                   tabPanel('Tuition', plotOutput("map3"),
                            #splitLayout(
                            inputPanel(
                                  radioButtons("varTui", "Tuition against:",
                                         c("3-year cohort default rate" = "CDR3",
                                           "Tuition in state students" = "TUITIONFEE_IN",
                                           "75th percentile for Math SAT" = "SATMT75",
                                           "Number of students enrolled in Fall" = "UGDS",
                                           "Median loan debt" = "DEBT_MDN",
                                           "Median loan debt (completed students)" = "GRAD_DEBT_MDN")),
                                  radioButtons("varTyp", "School's type:",
                                               c("All" = "all",
                                                 "Public" = "pub",
                                                 "Private non-profit" = "priN",
                                                 "Private for profit" = "priP")),
                                  inputPanel(
                                  uiOutput("sliderTui1"),
                                  br(),
                                  uiOutput("sliderTui2")
                                        )
                            )
                   )#,
                   #tabPanel('Histogram', plotOutput("map5"))
))