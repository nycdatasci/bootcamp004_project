shinyUI(
  navbarPage(
    title = "Airbnb Visual Search: New York",
    id = "nav",
    theme = shinytheme("United"),
    tabPanel("Map the Listing",
             div(class="outer",
            tags$head(
            includeCSS("style.css"),
            includeScript("gomap.js")
              ),

          leafletOutput("map",width="100%", height="100%"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
              draggable = TRUE, top = 20, left =30 , right ="auto" , bottom = "auto",
              width = 280, height = "auto",
                      h2(img(src = "airbnb.png", height =40),
                        "Listing in NYC"),
                      checkboxGroupInput("neighbour",
                                         h4("Select one neighborhood:"),
                                         choices = neighbour,selected = neighbour
                      ),
                      checkboxGroupInput("room",
                                         h4("Select room type:"),
                                         choices = room,selected = room
                      ),
                      sliderInput("price", h4("Price"), min = 1, 
                                  max = 511, value = c(1, 511)),
                      sliderInput("review",h4("Num of Reviews"), min = 0, 
                                  max = 257, value = c(0, 257)),
                      sliderInput("rating",h4("Scores Rating"), min = 0, 
                                  max = 100, value = c(0, 100)),
                    h4("Data: http://insideairbnb.com/"),
                    h6("Data complied by 01 September, 2015")
                     
            
               
             ),
            absolutePanel(id = "plots", class = "panel panel-default", fixed = FALSE,
                          draggable = TRUE, top = 300, left ="auto" , right =20 , bottom = "auto",
                          width = 300, height = "auto",
                          plotOutput("histroom",height = 150),
                          plotOutput("medprice",height = 150),
                          plotOutput("avgrating",height = 150)
                          )
            )),
    
        tabPanel("Listings, Neighbourhood and Hosts",    
                 fluidRow(
                   column(3,
                          h3("Active Listings by Neighbourhood"),
                          br(),
                          br(),
                          br(),
                          selectInput("listformat", "Showing as:", choices = c("Count","Percentage")),
                          sliderInput("listrating",h4("Scores Rating"), min = 0, 
                                      max = 100, value = c(0, 100)),
                          br(),
                          br(),
                          br(),
                          br(),
                          hr(),
                          h3("Active Listings by Host"),
                          sliderInput("hostn",h4("Top n Super hosts"), min = 0, 
                                      max = 200, value = 50),
                          dateRangeInput('hostt',
                                         h5("Host Since:"),
                                         start = "2008-06-26", end = "2015-08-31",
                                         min = "2008-6-26", max = "2015-08-31",
                                         separator = " to ", format = "yy/mm/dd",
                                         startview = 'week',  weekstart = 1)
                          
                          ),
                   column(9,
                         htmlOutput("activelist",width=700,height=400),
                         hr(),
                         DT::dataTableOutput("ziptable")
                          )
                 )
                 ),
    
          tabPanel("Reviews by Time",    
                   
                     titlePanel("Number of Reviews Over Time"),
                   fluidRow(
                     column(3,
                            h4("the numer of reviews"),
                           
                        checkboxInput("showgrid", label = "Show Grid ?", value = TRUE),
                            br(),
                         selectInput("tm",
                                     h5("Plot By: "),
                                     choices = choice,
                                     selected = "Year"
                                     ),
                         dateRangeInput('tr',
                                        h5("Select Date Range:"),
                                        start = "2008-10-06", end = "2015-09-02",
                                        min = "2008-10-06", max = "2015-09-02",
                                        separator = " to ", format = "yy/mm/dd",
                                        startview = 'week',  weekstart = 1)),
                   
                       column(9,
                         dygraphOutput("dygraph"),
                         hr(),
                         htmlOutput("geoChart")
                     )
                   )),
    tabPanel("Word Cloud: Reviews",   
             
             fluidRow(
               column(3,
                      sliderInput("rfreq",
                                  "Minimum Frequency:",
                                  min = 1500,  max = 271428, value = c(5000,259086)),
                      sliderInput("rmax",
                                  "Maximum Number of Words:",
                                  min = 1,  max = 1077,  value = 400)),
               
               column(9,
                      h2("What are the words people use the most,"),
                      h3("when they leave airbnb reviews? "),
                      plotOutput("wordcloud",height=500)
               )
             )),
    tabPanel("About the Dataset",    
             img(src = "dataset.png", height =700,weight =700)
             
               )
            
    
                   ))
          