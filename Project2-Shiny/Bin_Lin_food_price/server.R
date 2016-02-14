library(googleCharts)
library(ggplot2)
library(googleVis)

source("helper.R")

#options(shiny.trace=TRUE)

shinyServer(function(input, output, session) {
  
  chart_data <- prepare_dataframe_google()
  
  foodCategoryProducer <- reactive({
    return (append(c("All.food"), input$foodCategoryProducer))
  })
    
  foodCategory <- reactive({
    c = if (is.null(input$foodCategory)) c("All.food") else input$foodCategory
    return (c)
  })
  
  output$plot_pie <- renderGvis({
    Pie <- gvisPieChart(category_percent,
                        options=list(
                          title="Food Categories weighted by their relative importance or share of consumer expenditures.",
                          width="100%",
                          height="300px"
                        )
                        )
  })
  
  output$plot_category_percent_bar <- renderGvis({
    
    Column <- gvisColumnChart(category_percent,
                              options=list(
                                title="Food Categories weighted by their relative importance or share of consumer expenditures.",
                                vAxis="{title:'Percent'}",
                                hAxis="{title:'Food'}",
                                width="100%",
                                height="350px",
                                legend= "{position: 'none'}"
                              )
    )
    
  })
  
  
  food_price_bar_data <- reactive({
    
    filter(food.price, year %in% c(input$leftYear, input$rightYear) );

  })
  
  output$plot_food_price_bar <- renderGvis({
    
    Column <- gvisColumnChart(food_price_bar_data(),
                              options=list(
                                vAxis="{title:'Percent Change'}",
                                hAxis="{title:'Year'}",
                                width="100%",
                                height="350px"
                              )
                              )
    
  })
  
  #object for food prices line
  output$plot_food_price <- renderGvis({
    
    Line <- gvisLineChart(food.price, xvar="year", yvar=foodCategory(),
                          options=list(
                            title="Price Change in food",
                            width="100%",
                            height="300px",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
                          )
    
    return (Line)
    
  })
  
  
  output$plot_food_correlation_ui <- renderUI({
    
    #cor_zoom_width = cor_zoom_width * input$zoom
    #cor_zoom_height = cor_zoom_height * input$zoom
    
    #plotOutput("plot", width = cor_zoom_width * input$zoom, height = cor_zoom_height * input$zoom)
    
    plotOutput("plot_food_correlation",
               width = cor_zoom_width * input$zoom, 
               height = cor_zoom_height * input$zoom
    )
  })
  
  output$plot_food_correlation <- renderPlot({
    
    plot(food.category.cor)
    
  })
  
  # food price vs producer prie
  output$plot_food_vs_producer <- renderGvis({
    
    Line <- gvisLineChart(food_producer_join, xvar="year", yvar=foodCategoryProducer(),
                          options=list(
                            title="Price Change in all-item and in food",
                            width="100%",
                            height="300px",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
    )
    
  })
  
  # food price vs producer prie
  output$plot_food_vs_all <- renderGvis({
    
    food.and.all = inner_join(food.price, rename(all.price, All.item = percent.change), by = c("year"))
    
    Line <- gvisLineChart(food.and.all, xvar="year", yvar=c('All.item','All.food'),
                          options=list(
                            title="Price Change in all-item and in food",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
                          )
  })
  
  
  output$summary <- renderPrint({
    
  })
  
   output$table <- DT::renderDataTable({
     DT::datatable(food.price)
   })
  
})