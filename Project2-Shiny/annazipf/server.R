library(shiny)
library(leaflet)
# library(RColorBrewer)
# library(scales)
# library(lattice)
# library(dplyr)
 



shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################
  
  
  # Create the reactive map
   output$map <- renderLeaflet({
  leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
    setView(lat = 39.82, lng = -98.58, zoom = 4) %>%
    addMarkers(lng =selectdrs()$Longitude, lat = selectdrs()$Latitude,
                 clusterOptions = markerClusterOptions(), 
               popup = selectspec()
               )
  })

   #creating reactive functions for selected specialties and state
      selectdrs <- reactive({
     filter(drscity, Primary.specialty %in% input$specialty & State %in% input$state 
                      & Gender %in% input$sex)
        })
      
      selectspec <-reactive({
   specs = paste("<strong>", selectdrs()$First.Name, selectdrs()$Last.Name,"</strong>", selectdrs()$Credential,
                 "</br>", selectdrs()$address,  "</br>", 
                      "Specialty:", selectdrs()$Primary.specialty,  "</br>", 
                      "Secondary specialty:", selectdrs()$Secondary.specialty.1, "</br>",
                      "Clinic:", selectdrs()$Organization.legal.name, "</br>",
                      "Medical school:", selectdrs()$Medical.school.name,  "</br>",
                      "Years of experience:",  selectdrs()$experience, sep = " ")
      })
      
      #call the density map
      output$denmap <- renderLeaflet({
        leaflet(usa) %>%
          addTiles() %>%
          setView(lat = 39.82, lng = -98.58, zoom = 4) %>%
        addPolygons(data = usa, fillColor = ~colorQuantile("Blues", NULL, n = 10)(colornum), 
                      fillOpacity = 0.6,
                      weight = 2,
                      color = "white",
                      popup = polygon_popup) %>%
          addLegend("bottomright", pal = colorNumeric( palette = pal, domain = usa$percent ), values = ~percent,
                    title = "State doctors per 100 people",
                    opacity = 1)     
    })
      
      #creating the database explorer
      output$drtable <- DT::renderDataTable({
        df = cleantable %>%
          dplyr::filter(
            is.null(input$specials) | Specialty %in% input$specials,
            is.null(input$statestab) | State %in% input$statestab, 
            is.null(input$cities) | City %in% input$cities,
            is.null(input$medschool) | Medical.school %in% input$medschool
          ) %>%
          dplyr::arrange(desc(Years.experience))
      })
 
     
     #Plot outputs 
      output$plot = renderPlot(plot)     
      output$schoolp = renderPlot(schoolp)
      output$timep = renderPlot(timep)
      output$genderp = renderPlot(genderp)
      output$changep = renderPlot(changep)
      output$timespp = renderPlot(timespp)
      output$changepisolate = renderPlot(changepisolate)
} )

      
      #interactive plot
#       
#       finalInput <- reactive({
#         changefilter = dplyr::filter(change, Primary.specialty %in% subset)
#         
#       })
#       
#       output$plot <- renderPlot({
#         data <- getSymbols(input$symb, src = "yahoo", 
#                            from = input$dates[1],
#                            to = input$dates[2],
#                            auto.assign = FALSE)
#         
#         chartSeries(data, theme = chartTheme("white"), 
#                     type = "line", log.scale = input$log, TA = NULL)
#       })
      
