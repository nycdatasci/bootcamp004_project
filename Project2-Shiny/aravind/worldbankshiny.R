library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
require(datasets)
library(RColorBrewer)
library(xts)
library(googleCharts)

setwd('C://dataset')
worldbank=read.csv('worldbankplay2.csv',header=TRUE, stringsAsFactors = FALSE)
worldbank=tbl_df(worldbank)
countries=unique(worldbank$Country.Name)
indicators=unique(worldbank$Indicator.Name)
ind.cat=unique(worldbank[,c('Indicator.Name','Indicator.Category')])

#ind.category=unique(worldbank$Indicator.Category)


ui <- dashboardPage(
  dashboardHeader(title = "World bank indicator explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Timeseries", tabName = "timeseries", icon = icon("map")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Motion chart", tabName = "motionchart", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeseries",
              fluidRow(
                box(title = "Select Item", status = "info", solidHeader = TRUE,
                    collapsible = TRUE,
                    selectizeInput("country", "Select Country", countries,multiple=TRUE),
                    selectizeInput("IndicatorCategory", "Select Indicator Category", unique(ind.cat[,2])),
                    uiOutput("indicator")
                    #selectizeInput("indicator", "Select Indicator", indicators)
                )
              ),
              fluidRow(
                dygraphOutput("dygraph")
              )
              
      ),
      
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Select Item", status = "info", solidHeader = TRUE,
                    collapsible = TRUE,
                    selectizeInput("IndiCategory", "Select Indicator Category", unique(ind.cat[,2])),
                    uiOutput("indicator2"),
                    sliderInput("year", "Year",
                                min = 1960, max =2015,
                                value =2013, animate = TRUE)
                    #selectizeInput("indicator", "Select Indicator", indicators)
                )
              ),
              
              
              fluidRow(
                box(title = "Geo Chart", status = "success", solidHeader = TRUE,width = 500, height = 800,
                    collapsible = FALSE,
                    htmlOutput("geoChart")
                    
                    )
              ),
              
              fluidRow(
                box(      
                    htmlOutput("top10"),
                    htmlOutput("bottom10"))

                )
      ),
      
      tabItem(tabName = "motionchart",
              fluidRow(
                box(title = "Select Item", status = "info", solidHeader = TRUE,
                    collapsible = TRUE,
                    selectizeInput("varx", "Select X variable", indicators),
                    selectizeInput("vary", "Select Y variable", indicators),
                    selectizeInput("size", "Select X variable", indicators),
                    sliderInput("year1", "Year",
                                min = 1960, max =2015,
                                value =2013, animate = TRUE)
                   
                )
              
              
              
              
               ),
              
              fluidRow(
                box(title = "Bubble Chart", status = "success", solidHeader = TRUE,width = 500, height = 800,
                    collapsible = FALSE,
                    htmlOutput("bubble")
                    
                )
              )
    
      
      
      
      )
  )
))

server <- function(input, output) {
  
  var1 <- reactive({
    myIndicator2 <<- input$indicator2
    year <<-input$year
    
    temp = filter(worldbank, Indicator.Name==myIndicator2 & Region!=0)
    var=select_(temp, "Country.Name", paste("X",year,sep=''))
    colnames(var)=c("Country","indicator")
    return (var)
  })
  
  
  
  output$bubble<- renderGvis({
    vx<<- input$varx
    vy<<- input$vary
    siz<<-input$size
    yr1<<-input$year1
    
    x=filter(worldbank, Indicator.Name==vx & Region!=0)
    x=select_(x, "Country.Name","Region", paste("X",yr1,sep=''))
    colnames(x)=c("Country","Region",vx)
      
    y=filter(worldbank, Indicator.Name==vy & Region!=0)
    y=select_(y, paste("X",yr1,sep=''))
    colnames(y)=vy
    
    s=filter(worldbank, Indicator.Name==siz & Region!=0)
    s=select_(s, paste("X",yr1,sep=''))
    colnames(s)=siz
    
    final=cbind(x,y,s)
    
    gvisBubbleChart(final, idvar="Country", xvar=vx, yvar=vy,
                    colorvar="Region", sizevar=siz,options=list(width=1000, height=600))
    
  })
  
  
  
  
  output$top10 <- renderGvis({
    top10=arrange(var1(),desc(indicator))[1:10,]
    gvisBarChart(top10)
    
  })
  
  
  output$bottom10 <- renderGvis({
    bot10=arrange(var1(),indicator)[1:10,]
    gvisBarChart(bot10)
    
  })
  
  
  
  output$geoChart <- renderGvis({
    #print(var1())


    gvisGeoChart(var1(), locationvar='Country', colorvar="indicator"
                 ,options=list(displayMode="Markers",
                               colorAxis="{colors:['purple', 'red', 'orange', 'grey']}",
                               backgroundColor="lightblue",width=1100, height=700))
   
  })
  
  
  
  output$dygraph <- renderDygraph({
    myCountry <<- input$country
    myIndicator <<- input$indicator1
   
    #myDataSource = updateInputData()
    print(myCountry)
    print(length(myCountry))
    if(length(myCountry) == 1) {
      y = filter(worldbank, Indicator.Name == myIndicator ,Country.Name == myCountry)
      y=t(y);
      y=y[8:length(y),]
      y=as.numeric(y)
      year=1960:2015
      yc=cbind(y,year)
      
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      ##yc=yc[complete.cases(yc),])
      colnames(yc)=c(myCountry,"year")
      ts=xts(yc[,1],order.by =yc[,2])
      dygraph(ts)%>% dyRangeSelector()%>%
        dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2"))
      
    }
    
    else {
      y = filter(worldbank, Indicator.Name == myIndicator ,Country.Name %in% myCountry)
      
      
      y=t(y)
      y=y[8:nrow(y),]
      y=apply(y, 2, as.numeric)
      year=1960:2015
      yc=cbind(y,year)
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      colnames(yc)=c(sort(myCountry),"year")
      
      ##yc=yc[complete.cases(yc),])
      
      ts=xts(yc[,-ncol(yc)],order.by =yc[,ncol(yc)])
      dygraph(ts, main = myIndicator) %>% dyRangeSelector()%>%
        dyOptions( pointSize = 2,fillGraph = TRUE, fillAlpha = 0.05,colors =RColorBrewer::brewer.pal(ncol(yc), "Set2"))
      
    }
    # dySeries(ts, label = "vbh") %>%
    # dyOptions(drawGrid = input$showgrid)
  })
  
  output$indicator <- renderUI({
    
    choices = filter(ind.cat,Indicator.Category==input$IndicatorCategory)[,1]
    selectizeInput("indicator1", "Select Indicator", choices)
    #checkboxGroupInput("cities", "Choose Cities", cities)
  })
  
  
  output$indicator2 <- renderUI({
  
    choices2 = filter(ind.cat,Indicator.Category==input$IndiCategory)[,1]
    selectizeInput("indicator2", "Select Indicator", choices2)
    #checkboxGroupInput("cities", "Choose Cities", cities)
  })
  
  
}

shinyApp(ui, server)

