library(ggplot2)
library(ggthemes)
library(dplyr) 
library(googleVis) 
library(tidyr)
library(shiny)

shinyServer(function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    print(input$n_years1)
    df1=df[df$Commodity_Description %in%input$productcompare1, c(input$xcol1, input$ycol1, "Commodity_Description")]
   
  
  })
  
  selectedData2 <- reactive({
    print(input$xcol2)
    print(input$ycol2)
    df2=df[df$Commodity_Description %in%input$productcompare2, c(input$xcol2, input$ycol2, "Commodity_Description")]
    
    
  })
  
 
  
  VizData2 <- reactive({
    df2=df[df$Commodity_Description %in%input$productcompare2,]
    print(df2)
    df2})
  
  VizData2 <- reactive({
    df3=df[df$Commodity_Description %in%input$productcompare3,]
    print(df3)
    df3})
  
  ###################################### Data Explorer ######################
  
  
  output$Plot1title <- renderText({ 
    paste(input$xcol2, "vs", input$ycol2, "for Selected Food Products")
  })
  
  output$Plot2title <- renderText({ 
    paste(input$xcol1, "vs", input$ycol1, "for Selected Food Products")
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(data=selectedData())+geom_point(aes_string(x = input$xcol1, y = input$ycol1, colour="Commodity_Description")) + theme_tufte() + theme(
      axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) 
   })
  
  output$plot2 <- renderPlot({
 
    ggplot(data=selectedData2())+geom_point(aes_string(x = input$xcol2, y = input$ycol2, colour = "Commodity_Description")) + theme_tufte() +
      theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) 

  })
  
  output$hover_info1 <- renderPrint({
    str(c(input$plot1_hover$x, input$plot1_hover$y))
  })
  
  output$hover_info2 <- renderPrint({
    str(c(input$plot2_hover$x, input$plot2_hover$y))
  })
  
  output$brush_info1 <- renderPrint({
    brushedPoints(df, input$plot1_brush)
  })  
  output$brush_info2 <- renderPrint({
    brushedPoints(df, input$plot2_brush)
  })
#   output$plot3 <- renderPlot({
#  
#     ggplot(data=selectedData())+geom_point(aes_string(x = input$xcol1, y = input$ycol1, colour="Commodity_Description")) + theme(
#       axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
#     
#   })
#   output$plot4 <- renderPlot({
#     
#     ggplot(data=selectedData())+geom_bar(aes_string(x = input$xcol2, y = input$xcol2))+
#       theme(
#         axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
#     
#   })
# 
#   ggplot(edu, aes(x = edu[,input$varT1], 
#                   y = edu[,input$varT2], 
#                   colour = edu[,input$varTG])) +
#     theme_bw() + geom_point() + geom_smooth() +
#     ggtitle(paste(input$varT2, "against  ", input$varT1)) +
#     xlab(input$varT1) + ylab(input$varT2) +
#     labs(colour = input$varTG) +
#     xlim(input$inSlider1[1], input$inSlider1[2]) +
#     ylim(input$inSlider2[1], input$inSlider2[2]) +
#     theme(plot.title = element_text(size=22))
  
  ########################################################### Plot Clicks ######################
  
  ########################################################### Data Table #######################

  output$table <- DT::renderDataTable({
    DT::datatable(df[, input$show_vars, drop = FALSE])
    data <- df
    if (input$Commodity_Description != "All") {
      data <- df[df$Commodity_Description == input$Commodity_Description,]
    }
    if (input$Market_Year != "All") {
      data <- df[df$Market_Year == input$Market_Year,]
    }
    data
  })
  
######################################## Google Viz ###############################

  
  VizData <- reactive({
    df4=df[df$Commodity_Description %in%input$productcompare,]
    print(df4)
    df4 })
  
  
output$motionviz <- renderGvis({
  gvisMotionChart(
    data = VizData(), 
    idvar = "Commodity_Description", 
    timevar = "Market_Year",
    xvar = "Production", 
    yvar = "Consumption",
    colorvar = "Trade_Balance",
    sizevar = input$exportcompare,
    options = list(width = 850, showChartButtons=TRUE))})

})


    
    