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
      output$genderp = renderPlot({genderp})
      output$changep = renderPlot(changep)
      output$timespp = renderPlot(timespp)
      output$changepisolate = renderPlot(changepisolate)
      
      
      output$hover_info1 = renderText({
        paste0("Percent=", input$plot_hover1$x)
      })
      
      output$hover_info2 = renderText({
        if(!is.null(input$plot_hover2)) {
        paste0("Number of new professionals=", input$plot_hover2$y, 
               "\n Year=", round(input$plot_hover2$x)) }
      })
      
       
      
      addPopover(session=session, id="timespp", title="Gender based growth", 
                 content='Plot of new male/female medical graduates in medicare from 1950-2015. Females 
                 entered later: both experience a decline around 2007.', placement = "bottom",
                 trigger = "hover", options = NULL)
      addPopover(session=session, id="genderp", title="Male/female dominated fields", 
                 content='Top 26 most gender-segregated medical fields.', placement = "top",
                 trigger = "hover", options = NULL)
      
      
      addPopover(session=session, id="changep", title="Variance in growth of all 77 medicare sectors", 
                 content="Visualizing the number of new medicare professionals who join
the industry each year. All 77 disciplines are plotted simultaneously:
  we can pull out the top 10 most volatile fields in the lower graph.
Note that the y-axis is not cumulative, so each drop/raise represents
a shift in the number of new doctors.", placement = "bottom",
                 trigger = "hover", options = NULL)
      
      addPopover(session=session, id="changepisolate", title="Most volatile sectors", 
                 content="Growth of sectors with highest variance.", placement = "top",
                 trigger = "hover", options = NULL)
      
      
      addPopover(session=session, id="schoolp", title = "Popular schools",
content="USA schools producing highest number of medicare
                 doctors since 1951", 
                placement = "top",
                 trigger = "hover", options = NULL)
      
      addPopover(session=session, id="plot", content="Largest medical fields", 
                 title = " ",
                placement = "bottom",
                 trigger = "hover", options = NULL)
      

})

