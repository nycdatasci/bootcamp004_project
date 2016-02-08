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
        densitymap   })
      
      #creating the database
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
      

# 
#   ## Data Explorer ###########################################
# 
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectInput(session, "cities", choices = cities,
#       selected = stillSelected)
#   })
# 
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected)
#   })
# 
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
# 
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df)
# 
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
})
