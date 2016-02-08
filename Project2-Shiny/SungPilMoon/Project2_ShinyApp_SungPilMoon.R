###############################################################################
# [YelpSpoon ShinyApp]
# by Sung Pil Moon
###############################################################################
# cat("\014")
# setwd("~/Desktop/Dataset for NYC DSA ")
# #save.image(file="proj2_YelpSpoon.RData")
# load('proj2_YelpSpoon.RData')

library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)
library(leaflet)

# View(myRes) 
# View(ylpBizDataSml_display) 

header <- dashboardHeader(
  title = "YelpSpoon Shiny App"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("YelpSpoon Map", tabName = "ylpSpoonMap", icon = icon("fa fa-table")),
    menuItem("YelpSpoon Grid", tabName = "ylpSpoonGrid", icon = icon("bar-chart-o"))
  )
)

body <- dashboardBody(

  tabItems(
    
    ##########################################################
    # First contents (YlpSpoon Map & DataTable)
    ###########################################################
    tabItem("ylpSpoonMap",
            div(class="outer",
                
                fluidRow(
                  valueBoxOutput("boxForNumRowsInMap"),
                  valueBoxOutput("boxForReviewCountInMap"),
                  valueBoxOutput("boxForAvgRatingsInMap")
                ), 
                
                fluidRow(
                  column(4,
                         selectizeInput('multiSelectForCategories', 'Categories:', 
                                        c("Choose multiple"= '', 
                                          "Food", "Restaurant",
                                          "Health", "Education",
                                          "Coffee", "Cafe",
                                          "Pubs", "Bars", "Brewery", 
                                          "Shopping", "Fashion",
                                          "Italian", "Mexican","Korean", "Taiwanese", "Local", 
                                          "Japanese","French", "Indian", "Brazilian", 
                                          "Pizza", "Fast Food", "Burger", "Steak", 
                                          "Desserts", "Seafood","Vegetarian","Sushi", "Salad",
                                          "Asian", "American", "African", "European", "Caribbean",
                                          "Dance","Salsa", "Aerobic","Yoga", 
                                          "Pilates"
                                        ), 
                                        multiple = TRUE
                         )
                  ),
                  column(4,
                         selectizeInput('multiSelectForStates', 'States:', 
                                        c("Choose multiple"= '', "California" = "CA", "Georgia" = "GA", "Illinois" = "IL", "Indiana" = "IN", 
                                          "Massachusetts"="MA", "Maryland"="MD", "Michigan"="MI", "North Carolina"="NC", "New Jersey"="NJ", 
                                          "New York"="NY", "Ontario"="ON", "Pennsylvania"="PA", "Rhode Island"="RI", "Texas"="TX", 
                                          "Virginia"="VA", "Washington"="WA"),
                                        multiple = TRUE
                         )
                  ),
                  column(4,
                         selectizeInput("multiSelectForSchools", "Schools:", 
                                        c("Choose One"= '', "Brown University", "California Institute of Technology",
                                          "California Polytechnic State University", "Carnegie Mellon University", 
                                          "Columbia University", "Cornell University", "Georgia Institute of Technology",
                                          "Harvard University","Harvey Mudd College", "Massachusetts Institute of Technology", 
                                          "Princeton University","Purdue University","Rensselaer Polytechnic Institute",
                                          "Rice University", "Stanford University", "University of California at Berkeley", 
                                          "University of California - Los Angeles", "University of California - San Diego", 
                                          "University of Illinois - Urbana-Champaign", "University of Maryland - College Park", 
                                          "University of Massachusetts - Amherst", "University of Michigan - Ann Arbor",
                                          "University of North Carolina - Chapel Hill",  "University of Pennsylvania",
                                          "University of Southern California", "University of Texas - Austin",
                                          "University of Washington", "University of Waterloo", "Virginia Tech"
                                        ),
                                        multiple = F
                         )
                  )
                ),
                fluidRow(
                  column(4, textInput("searchInputForCategory","Category Search:","")),
                  column(4, textInput("searchInputForCity","City Search:","") ),
                  column(4, textInput("searchInputForBizName","Business Name Search:",""))
                ),
                fluidRow(
                  column(4, sliderInput("sliderForRating", "Avg.Rating Range:", 
                              min = 1.0, max = 5.0, value = c(3.0, 4.5), step = 0.5)
                  ),
                  column(4, sliderInput("sliderForReviewCount", "Review Count Range:", 
                              min = 1, max = 2874, value = c(10, 2500), step = 1)
                  ),
                  column(4)
                ),
                
                leafletOutput("myYelpMap", width=900, height=550),
                
                #br(),
                DT::dataTableOutput("myYelpDataTable")
            )
            
    ),
    
    ##########################################################
    # Second content (YlpSpoon Grid & Table)
    ###########################################################
    tabItem("ylpSpoonGrid",
            
            fluidRow(
              valueBoxOutput("boxForNumRows"),
              valueBoxOutput("boxForReviewCount"),
              valueBoxOutput("boxForAvgRatings")
            ), 
            
            
            fluidRow(
              column(4,
                     selectizeInput('multiSelectForCategoriesForGrid', 'Categories:', 
                                    c("Choose multiple"= '', 
                                      "Food", "Restaurant",
                                      "Health", "Education",
                                      "Coffee", "Cafe",
                                      "Pubs", "Bars", "Brewery", 
                                      "Shopping", "Fashion",
                                      "Italian", "Mexican","Korean", "Taiwanese", "Local", 
                                      "Japanese","French", "Indian", "Brazilian", 
                                      "Pizza", "Fast Food", "Burger", "Steak", 
                                      "Desserts", "Seafood","Vegetarian","Sushi", "Salad",
                                      "Asian", "American", "African", "European", "Caribbean",
                                      "Dance","Salsa", "Aerobic","Yoga", 
                                      "Pilates"
                                    ), 
                                    multiple = TRUE
                     )
              ),
              
              column(4,
                     selectizeInput('multiSelectForStatesForGrid', 'States:', 
                                    c("Choose multiple"= '', "California" = "CA", "Georgia" = "GA", "Illinois" = "IL", "Indiana" = "IN", 
                                      "Massachusetts"="MA", "Maryland"="MD", "Michigan"="MI", "North Carolina"="NC", "New Jersey"="NJ", 
                                      "New York"="NY", "Ontario"="ON", "Pennsylvania"="PA", "Rhode Island"="RI", "Texas"="TX", 
                                      "Virginia"="VA", "Washington"="WA"),
                                    multiple = TRUE
                     )
              ),
              column(4,
                     selectizeInput("multiSelectForSchoolsForGrid", "Schools:", 
                                    c("Choose One"= '', "Brown University", "California Institute of Technology",
                                      "California Polytechnic State University", "Carnegie Mellon University", 
                                      "Columbia University", "Cornell University", "Georgia Institute of Technology",
                                      "Harvard University","Harvey Mudd College", "Massachusetts Institute of Technology", 
                                      "Princeton University","Purdue University","Rensselaer Polytechnic Institute",
                                      "Rice University", "Stanford University", "University of California at Berkeley", 
                                      "University of California - Los Angeles", "University of California - San Diego", 
                                      "University of Illinois - Urbana-Champaign", "University of Maryland - College Park", 
                                      "University of Massachusetts - Amherst", "University of Michigan - Ann Arbor",
                                      "University of North Carolina - Chapel Hill",  "University of Pennsylvania",
                                      "University of Southern California", "University of Texas - Austin",
                                      "University of Washington", "University of Waterloo", "Virginia Tech"
                                    ),
                                    multiple = F
                     )
              )
              
            ),
            
            fluidRow(
              column(4, textInput("searchInputForCategoryForGrid","Category Search:","")), 
              column(4, textInput("searchInputForCityForGrid","City Search:","") ),
              column(4 #, #textInput("searchInputForBizNameForGrid","Business Name Search:","")
              )
            ),
            fluidRow(
              column(4,
                     sliderInput("sliderForRatingForGrid", "Avg.Rating Range:", 
                                 min = 1.0, max = 5.0, value = c(2.5, 5), step = 0.5)
              )
            ),
            
            plotOutput("yelpSpoonGridPlot", height="700"),
            
            DT::dataTableOutput("myYelpDataTableForGrid")
            
    )
  )
)

server <- function(input, output) { 
  
  
  ##########################################################
  # Data manipulation (for YlpSpoon Map & DataTable)
  ###########################################################
  updateInputDataForMapTable <- reactive({  
    
    dataFiltered <- ylpBizDataSml
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Business Name
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForBizName != ""){
      dataFiltered <- dataFiltered %>% 
        filter(grepl(input$searchInputForBizName,dataFiltered$name, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCategory != ""){
      dataFiltered <- dataFiltered %>% 
        filter(grepl(input$searchInputForCategory,dataFiltered$categories, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Multiple Selectize)
    # (OR operations. --> Ex) find 'business' in both 'FOOD' and 'Italian'
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForCategories) ){
      targetCategories <- unlist(strsplit(input$multiSelectForCategories," "))
      
      filteredCategories <- function(targetCategories){
        tmpDT <- dataFiltered
        for(i in 1:length(targetCategories)){
          tmpDT <- tmpDT %>% filter(grepl(targetCategories[i], tmpDT$categories, ignore.case = TRUE)) 
        }
        return(tmpDT)
      } 
      dataFiltered <- filteredCategories(targetCategories)
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStates) ){
      targetStates <- unlist(strsplit(input$multiSelectForStates," "))
      dataFiltered <- dataFiltered %>% filter(state %in% targetStates)
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCity != ""){
      dataFiltered <- dataFiltered %>% 
        filter(grepl(input$searchInputForCity,dataFiltered$city, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by School Name
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForSchools) ){
      
      targetSchools <- input$multiSelectForSchools

      dataFiltered <- dataFiltered %>% 
        filter(grepl(input$multiSelectForSchools, dataFiltered$schools, ignore.case = TRUE)) 
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Average Ratings & by Review Counts (with Slider Range values)
    #////////////////////////////////////////////////////////////////////////////////
    dataFiltered <- dataFiltered[(input$sliderForRating[1] <= dataFiltered$stars & 
                                    dataFiltered$stars <= input$sliderForRating[2]),]
    dataFiltered <- dataFiltered[(input$sliderForReviewCount[1] <= dataFiltered$review_count & 
                                    dataFiltered$review_count <= input$sliderForReviewCount[2]),]
    
    rowNumInMap <- nrow(dataFiltered)
    output$boxForNumRowsInMap <- renderValueBox({
      valueBox( value = rowNumInMap, subtitle = "Business", icon = icon("fa fa-building-o"),
                color = if(5000 < rowNumInMap) "red" else 
                  if(2000 < rowNumInMap & rowNumInMap <= 5000) "yellow" else 
                    if(1000 < rowNumInMap & rowNumInMap <= 2000) "aqua" else
                      if(500 < rowNumInMap & rowNumInMap <= 1000) "teal" else
                        if(100 < rowNumInMap & rowNumInMap <= 500) "olive" else
                          if(20 < rowNumInMap & rowNumInMap <= 100) "blue" else "purple"
      )  
    })
    
    rCountForMap <- sum(dataFiltered$review_count)
    output$boxForReviewCountInMap <- renderValueBox({  
      valueBox( value = rCountForMap, subtitle = "Reviews", icon = icon("fa fa-comment-o"),
                color = if(250000 < rCountForMap) "red" else 
                  if(150000 < rCountForMap & rCountForMap <= 250000) "yellow" else 
                    if(50000 < rCountForMap & rCountForMap <= 150000) "aqua" else
                      if(5000 < rCountForMap & rCountForMap <= 50000) "teal" else
                        if(500 < rCountForMap & rCountForMap <= 5000) "olive" else
                          if(50 < rCountForMap & rCountForMap <= 500) "blue" else "purple"
      )  
    })
    
    aRatingForMap <- round((sum(dataFiltered$stars)/rowNumInMap), 2)
    output$boxForAvgRatingsInMap <- renderValueBox({
      valueBox( value = aRatingForMap, subtitle = "Avg.ratings", icon = icon("star-o"),
                color = if(4.5 <= aRatingForMap) "red" else 
                  if(4.0 <= aRatingForMap & aRatingForMap < 4.5) "orange" else 
                    if(3.5 <= aRatingForMap & aRatingForMap < 4.0) "olive" else
                      if(3.0 <= aRatingForMap & aRatingForMap < 3.5) "teal" else
                        if(2.0 <= aRatingForMap & aRatingForMap < 3.0) "blue" else "purple"
      )  
    })
    
    dataFiltered
    
  })
  
  
  ##########################################################
  # Data manipulation 2-a (for YlpSpoon Grid)
  ###########################################################
  updateInputDataForGrid <- reactive({  
    
    dataFilteredForGrid <<- ylpBizDataSml 
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCategoryForGrid != ""){
      dataFilteredForGrid <<- dataFilteredForGrid %>% 
        filter(grepl(input$searchInputForCategoryForGrid, dataFilteredForGrid$categories, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Multiple Selectize)
    # (OR operations. --> Ex) find 'business' in both 'FOOD' and 'Italian'
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForCategoriesForGrid) ){
      targetCategories <- unlist(strsplit(input$multiSelectForCategoriesForGrid," "))
      
      filteredCategories <<- function(targetCategories){
        tmpDT <- dataFilteredForGrid
        for(i in 1:length(targetCategories)){
          tmpDT <- tmpDT %>% filter(grepl(targetCategories[i], tmpDT$categories, ignore.case = TRUE)) 
        }
        return(tmpDT)
      } 
      dataFilteredForGrid <<- filteredCategories(targetCategories)
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStatesForGrid) ){
      targetStates <- unlist(strsplit(input$multiSelectForStatesForGrid," "))
      dataFilteredForGrid <<- dataFilteredForGrid %>% filter(state %in% targetStates)
      
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCityForGrid != ""){
      dataFilteredForGrid <<- dataFilteredForGrid %>% 
        filter(grepl(input$searchInputForCityForGrid,dataFilteredForGrid$city, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by School Name
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForSchoolsForGrid) ){
      
      targetSchools <- input$multiSelectForSchoolsForGrid
      dataFilteredForGrid <<- dataFilteredForGrid %>% 
        filter(grepl(input$multiSelectForSchoolsForGrid, dataFilteredForGrid$schools, ignore.case = TRUE)) 
    }
    
    dataFilteredForGrid <<- dataFilteredForGrid %>% 
      filter(state != '') %>%
      mutate(tsum = n()) %>% group_by(state, stars) 
    
    dataFilteredForGrid <- dataFilteredForGrid %>% 
      summarise(totalByStar = n()) %>% arrange(desc(stars)) %>% 
      mutate(total = sum(totalByStar)) %>% mutate(percent = round((totalByStar / total)*100, 1)) %>%
      mutate(percentWeight = ifelse(percent >= 20, percent * 2.5, # custom column to weight the percent for size on the plot
                                    ifelse(percent < 20 & percent >= 15, percent * 1.2, 
                                           ifelse(percent < 15 & percent >= 10, percent,
                                                  ifelse(percent < 10 & percent >= 5, percent * 0.8, 1)))))
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Average Ratings & by Review Counts (with Slider Range values)
    #////////////////////////////////////////////////////////////////////////////////
    dataFilteredForGrid <<- dataFilteredForGrid[(input$sliderForRatingForGrid[1] <= dataFilteredForGrid$stars & 
                                                   dataFilteredForGrid$stars <= input$sliderForRatingForGrid[2]),]
    
    dataFilteredForGrid
    
    
  })
  
  
  ##########################################################
  # Data manipulation 2-b (for YlpSpoon Grid Table)
  ###########################################################
  updateInputDataForGridTable <- reactive({  
    
    dataFilteredForGridTable <<- ylpBizDataSml # original

    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCategoryForGrid != ""){
      dataFilteredForGridTable <<- dataFilteredForGridTable %>% 
        filter(grepl(input$searchInputForCategoryForGrid, dataFilteredForGridTable$categories, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Categories (with Multiple Selectize)
    # (OR operations. --> Ex) find 'business' in both 'FOOD' and 'Italian'
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForCategoriesForGrid) ){
      targetCategories <- unlist(strsplit(input$multiSelectForCategoriesForGrid," "))
      
      filteredCategories <- function(targetCategories){
        tmpDT <- dataFilteredForGridTable
        for(i in 1:length(targetCategories)){
          tmpDT <- tmpDT %>% filter(grepl(targetCategories[i], tmpDT$categories, ignore.case = TRUE)) 
        }
        return(tmpDT)
      } 
      dataFilteredForGridTable <<- filteredCategories(targetCategories)
    }
    
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStatesForGrid) ){
      targetStates <- unlist(strsplit(input$multiSelectForStatesForGrid," "))
      dataFilteredForGridTable <<- dataFilteredForGridTable %>% filter(state %in% targetStates)
      
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCityForGrid != ""){
      dataFilteredForGridTable <<- dataFilteredForGridTable %>% 
        filter(grepl(input$searchInputForCityForGrid,dataFilteredForGridTable$city, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by School Name
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForSchoolsForGrid) ){
      
      targetSchools <- input$multiSelectForSchoolsForGrid
      dataFilteredForGridTable <<- dataFilteredForGridTable %>% 
        filter(grepl(input$multiSelectForSchoolsForGrid, dataFilteredForGridTable$schools, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Average Ratings & by Review Counts (with Slider Range values)
    #////////////////////////////////////////////////////////////////////////////////
    dataFilteredForGridTable <<- dataFilteredForGridTable[(input$sliderForRatingForGrid[1] <= dataFilteredForGridTable$stars & 
                                                             dataFilteredForGridTable$stars <= input$sliderForRatingForGrid[2]),]
    
    rowNum <- nrow(dataFilteredForGridTable)
    output$boxForNumRows <- renderValueBox({
      valueBox( value = rowNum, subtitle = "Business", icon = icon("fa fa-building-o"), 
                color = if(5000 < rowNum) "red" else 
                  if(2000 < rowNum & rowNum <= 5000) "yellow" else 
                    if(1000 < rowNum & rowNum <= 2000) "aqua" else
                      if(500 < rowNum & rowNum <= 1000) "teal" else
                        if(100 < rowNum & rowNum <= 500) "olive" else
                          if(20 < rowNum & rowNum <= 100) "blue" else "purple"
      )  
    })
    
    rCountForGrid <- sum(dataFilteredForGridTable$review_count)
    output$boxForReviewCount <- renderValueBox({
      valueBox( value = rCountForGrid, subtitle = "Reviews", icon = icon("fa fa-comment-o"),
                color = if(250000 < rCountForGrid) "red" else 
                  if(150000 < rCountForGrid & rCountForGrid <= 250000) "yellow" else 
                    if(50000 < rCountForGrid & rCountForGrid <= 150000) "aqua" else
                      if(5000 < rCountForGrid & rCountForGrid <= 50000) "teal" else
                        if(500 < rCountForGrid & rCountForGrid <= 5000) "olive" else
                          if(50 < rCountForGrid & rCountForGrid <= 500) "blue" else "purple"
      )   
    })
    
    aRatingForGrid <- round((sum(dataFilteredForGridTable$stars)/rowNum), 2)
    output$boxForAvgRatings <- renderValueBox({
      valueBox( value = aRatingForGrid, subtitle = "Avg.ratings", icon = icon("star-o"),
                color = if(4.5 <= aRatingForGrid) "red" else 
                  if(4.0 <= aRatingForGrid & aRatingForGrid < 4.5) "orange" else 
                    if(3.5 <= aRatingForGrid & aRatingForGrid < 4.0) "olive" else
                      if(3.0 <= aRatingForGrid & aRatingForGrid < 3.5) "teal" else
                        if(2.0 <= aRatingForGrid & aRatingForGrid < 3.0) "blue" else "purple"
      )   
    })
    dataFilteredForGridTable
    
  })
  
  rowCount <- reactive({
    nrow(dataFilteredForGridTable)
  })
  
  #######################################
  # Component 1: Yelp Spoon Map
  ########################################
  output$myYelpMap <- renderLeaflet({
    
    dataFiltered <- updateInputDataForMapTable() 
    dataForLeafMap <- dataFiltered[1:500, ]
    
    leaflet(data = updateInputDataForMapTable()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(name), 
                 clusterOptions = markerClusterOptions())
  })
  
  observe({
    leafletProxy("myYelpMap", data = updateInputDataForMapTable()) #%>% clearShapes() 
  })
  
  
  #######################################
  # Component 2: Yelp Spoon Data Table
  ########################################
  output$myYelpDataTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable <- updateInputDataForMapTable() 
    dataForDTable <- dataForDTable[,c(2,5,3:4,8,6:7,11,13)]
    
  }, options = list(
    searching = F,
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100'))
    #, pageLength = 20
  )
  ))
  
  #######################################
  # Component 3: Yelp Spoon Grid 
  ########################################
  output$yelpSpoonGridPlot <- renderPlot({  
    
    dataWeightedGroupByStateStarLoc <- updateInputDataForGrid()
    #alphaLv <- newDotAlphaOfGrid()
    
    gridPlot <- ggplot(data = dataWeightedGroupByStateStarLoc,
                       aes(x = state, y = stars, label = percent)) +
      geom_point(aes(size = percentWeight*2, colour = stars, alpha=.05)) + 
      geom_text(hjust = 0.4, size = 4) +
      scale_size(range = c(1,30), guide = 'none') + 
      scale_color_gradient( low = "darkblue", high = "red") + 
      labs(title="Avg.rating proportion of business by state ", x = "by state", y="Avg.Ratings") +
      scale_y_continuous(breaks=seq(1, 5, 0.5)) +
      theme(legend.title=element_blank())
    
    print(gridPlot)
  })
  
  
  
  #######################################
  # Component 4: Yelp Spoon Data Table in Grid
  ########################################
  output$myYelpDataTableForGrid <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTableForGrid <- updateInputDataForGridTable() 
    dataForDTableForGrid <- dataForDTableForGrid[,c(2,5,3:4,8,6:7,11,13)]
    
  }, options = list(
    searching = F,
    lengthMenu = list(c(10, 5, 15, 25, 25, 50, 100), c('10', '5', '15', '20', '25','50','100'))
    #, pageLength = 20
  )
  ))
  
}

shinyApp(
  ui = dashboardPage(header, sidebar, body , skin = "red" #, #c(“blue”, “black”, “purple”, “green”, “red”, “yellow”)
                     
  ),
  server <- server
)
