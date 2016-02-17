library(ggplot2)
library(ggthemes)
library(dplyr) 
library(googleVis) 
library(magrittr)
library(tidyr)

shinyServer(function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    df=df[, c(input$xcol1, input$ycol1)]
    df
  })
  
  output$plot1 <- renderPlot({
    ggplot(data=selectedData())+geom_point(aes_string(x = input$xcol1, y = input$ycol1))+
      theme(
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks = element_blank())
  })
  output$plot2 <- renderPlot({
    ggplot(data=selectedData())+geom_point(aes_string(x = input$xcol2, y = input$ycol2))+
      theme(
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks = element_blank())

})
  output$plot3 <- renderPlot({
    ggplot(data=selectedData())+geom_bar(aes_string(x = input$n_years1, y = input$ycol1))+
      theme(
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks = element_blank())
    
  })
  output$plot4 <- renderPlot({
    ggplot(data=selectedData())+geom_bar(aes_string(x = input$n_years2, y = input$ycol2))+
      theme(
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), axis.ticks = element_blank())
    
  })

})