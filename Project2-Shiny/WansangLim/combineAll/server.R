library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  #data input
  dataSalHis <- read.csv("../dataSalHis.csv", stringsAsFactors=FALSE) #1 Graph
  dataLevel <- read.csv("../levelPlotDa.csv", stringsAsFactors=TRUE) # 2 Graph
  compareOut <- read.csv("../compareOut.csv") # 3 Graph
  salaryByYYMM <- read.csv("../salaryByYYMM.csv")


  output$distPlot <- renderPlot({
    
    startYYMM <-input$rowNum
    endYYMM <- startYYMM + 9
    partData <- salaryByYYMM[startYYMM:endYYMM,]
    
    salaryInput <- reactive( {
      switch(
        input$salary,
        "minimum" = 2,
        "maximum" = 3,
        "average" = 4)
      })  
  
    salaryLevelInput <- reactive( {
      switch(
        input$salaryByLevel,
        "minimum" = 4,
        "maximum" = 5,
        "average" = 6)
    })
    
    outOrNot <- reactive({
      switch(
        input$OutNot,
        "Outlier" = 1, 
        "No Outlier" = 2
      )
    })
    
    salaryTimeInput <- reactive( {
      switch(
        input$salary,
        "total number" = "total",
        "minimum" = "SalaryFromAve" ,
        "maximum" = "SalaryToAve" ,
        "average" = "SalaryAveAve" )
    })
    
    aa <- salaryInput()
    x    <- dataSalHis[, aa] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    columnSel <- salaryLevelInput()
    choice <- outOrNot()
    colName <- salaryTimeInput()
  
  #Graph output
    if (input$radio == 1) {
      hist(x, breaks = bins, col = 'darkgray', border = 'white') #whole hist 1
    } else if (input$radio == 2) {
        plot(dataLevel[, 3], dataLevel[, columnSel]) # hist by Level
    } else if (input$radio == 3) {
        if (choice == 1) {
          barGraph <- ggplot(compareOut, aes(x=OTitle, y=OAver)) + geom_bar(stat="identity")
          barGraph + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), panel.background = element_blank())
      } else if (choice == 2) {
          barGraph <- ggplot(compareOut, aes(x=NTitle, y=NAver)) + geom_bar(stat="identity")
          barGraph + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), panel.background = element_blank())
      }
    } else if (input$radio == 4) {
      if (input$radioTime == 1) {
        if (colName == "total") {
          graphYYMM <- qplot(Posting.YYMM, total, data = salaryByYYMM)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryFromAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryFromAve, data = salaryByYYMM)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryToAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryToAve, data = salaryByYYMM)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryAveAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryAveAve, data = salaryByYYMM)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        }
        
      } else if (input$radioTime == 2) {
        
        if (colName == "total") {
          graphYYMM <- qplot(Posting.YYMM, total, data = partData)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryFromAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryFromAve, data = partData)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryToAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryToAve, data = partData)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        } else if (colName == "SalaryAveAve") {
          graphYYMM <- qplot(Posting.YYMM, SalaryAveAve, data = partData)
          graphYYMM + theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            panel.background = element_blank())
        }
      }
    }
  
  }) #output$distPlot

})
