library(shiny)
library(shinydashboard)
library(googleVis)
setwd("/Users/satishjoshi/DataBootcamp/ShinyProject")
#shiny::runApp(host="0.0.0.0",port=3168)


server <- function(input, output) {
  # TotalInspections = nrow(inspection_data)
  # TotalCrashes = nrow(crash_data)
  # 
  # TotalFatalities = sum(crash_data$Fatalities)
  # TotalInjuries = sum(crash_data$Injuries)
  
  output$TotalInspections <- renderInfoBox({
    infoBox(
      "Total Inspections", TotalInspections, icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      "Total Crashes", TotalCrashes, icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Fatalities", TotalFatalities, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Injuries", TotalInjuries, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  # PercentTowaway = (nrow(crash_data[crash_data$Tow_Away == "Yes",])/TotalCrashes) * 100
  # PercentHazmat = (nrow(crash_data[crash_data$Hazmat_released == "Yes",])/TotalCrashes) * 100
  
  output$approvalBox3 <- renderInfoBox({
    infoBox(
      "Tow Away", paste0(PercentTowaway,"%"), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  output$approvalBox4 <- renderInfoBox({
    infoBox(
      "Hazmat Involved", paste0(PercentHazmat,"%"), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  # TotalHazOOSViol = sum(inspection_data$HAZMAT_OOS_TOTAL)
  # TotalOOSViol = sum(inspection_data$OOS_TOTAL)
  # TotalVehOOSViol = sum(inspection_data$VEHICLE_OOS_TOTAL)
  # TotalDriverOOSViol = sum(inspection_data$DRIVER_OOS_TOTAL)
  
  output$approvalBox5 <- renderInfoBox({
    infoBox(
      "Total OOS Violation", TotalOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  output$approvalBox6 <- renderInfoBox({
    infoBox(
      "Hazmat OOS Violation", TotalHazOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  output$approvalBox7 <- renderInfoBox({
    infoBox(
      "Vehicle OOS Violation", TotalVehOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  output$approvalBox8 <- renderInfoBox({
    infoBox(
      "Driver OOS Violation", TotalDriverOOSViol, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  # TotalUnsafeDrivingInsp = nrow(inspection_data[inspection_data$UNSAFE_INSP == "Y",])
  # TotalAlcoholDrivingInsp = nrow(inspection_data[inspection_data$SUBT_ALCOHOL_INSP == "Y",])
  # TotalFatugueDrivingInsp = nrow(inspection_data[inspection_data$FATIGUED_INSP == "Y",])
  # TotalMaintenanxeInsp = nrow(inspection_data[inspection_data$VH_MAINT_INSP == "Y",])
  output$approvalBox9 <- renderInfoBox({
    infoBox(
      "Vehicle Maintenance Insp", TotalMaintenanxeInsp, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  output$approvalBox10 <- renderInfoBox({
    infoBox(
      "Fatigued Driving Insp", TotalFatugueDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  output$approvalBox11 <- renderInfoBox({
    infoBox(
      "Alcohol Substance Insp", TotalAlcoholDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  output$approvalBox12 <- renderInfoBox({
    infoBox(
      "Unsafe Driving Insp", TotalUnsafeDrivingInsp, icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })  
  output$geoChart <- renderGvis({
    gvisGeoChart(states, "state.name", input$selected, 
                 options=list(region="US", 
                              title="Chart1",
                              displayMode="regions", 
                              resolution="provinces",
                              width=360, height=225,
                              backgroundColor="lightbrown",
                              colorAxis="{colors:['pink', 'red']}"
                 ))
  })
  output$geoChart2 <- renderGvis({
    gvisGeoChart(states, "state.name", input$selected2,
                 options=list(region="US", 
                              title="Chart2",
                              displayMode="regions", 
                              resolution="provinces",
                              width=360, height=225,
                              backgroundColor="lightbrown",
                              colorAxis="{colors:['lightblue', 'darkblue']}"
                 ))
  })    
  output$geoChart3 <- renderGvis({
    gvisGeoChart(states, "state.name", input$selected3,
                 options=list(region="US", 
                              title="Chart3",
                              displayMode="regions", 
                              resolution="provinces",
                              backgroundColor="lightbrown",
                              width=360, height=225,
                              colorAxis="{colors:['lightgreen', 'darkgreen']}"
                 ))
  })
  output$crashtable <- renderGvis({
    gvisTable(states[order(states$Population, decreasing = T),],
              options=list(page='enable', width=800, height=300, color='blue'))
  })
  output$crashbymaketable <- renderGvis({
    gvisTable(crashes_by_makeyear[order(crashes_by_makeyear$Crashes, decreasing = T),],
              options=list(page='enable',width=500, height=300))
  })
  output$inspectionbymaketable <- renderGvis({
    gvisTable(inspections_by_makeyear[order(inspections_by_makeyear$Inspections, decreasing = T),],
              options=list(page='enable',width=500, height=300))
  })
  options=list(width=200, height=300)
  output$crashChart <- renderGvis({
    
    gvisColumnChart(
      crashes_by_makename[crashes_by_makename$Crashes >= input$crashrows[1] & 
                            crashes_by_makename$Crashes <= input$crashrows[2],],
      
      #head(crashes_by_makename[order(crashes_by_makename$Crashes, decreasing = T),], n=input$crashrows),
      
      
      #crashes_by_makename_subset, 
      xvar="Make", yvar= "Crashes",
      options=list(title="Crashes by Make",
                   titleTextStyle="{color:'red', 
                   fontName:'Courier', 
                   fontSize:16}", 
                   vAxis="{title:'Crash Count'}",
                   hAxis="{title:'Make'}",
                   color = "green",
                   legend = "{position: 'none'}",
                   hAxis.textPosition = "in",
                   hAxis.slantedTextAngle = 90,
                   backgroundColor="#D3D3D3", 
                   width=600, height=300 )
    )
    
})
  output$inspectionChart <- renderGvis({
    
    gvisColumnChart(
      inspections_by_makename[inspections_by_makename$Inspections >= input$insprows[1] & 
                                inspections_by_makename$Inspections <= input$insprows[2],],
      
      #head(inspections_by_makename[order(inspections_by_makename$Inspections, decreasing = T),], n=input$insprows),
      #gvisColumnChart(inspections_by_makename_subset, 
      xvar="Make", yvar= "Inspections", 
      options=list(title="Inspections by Make",
                   titleTextStyle="{color:'red', 
                   fontName:'Courier', 
                   fontSize:16}", 
                   vAxis="{title:'Inspection Count'}",
                   hAxis="{title:'Make'}",
                   color = "blue",
                   legend = "{position: 'none'}",
                   backgroundColor="#D3D3D3", 
                   width=600, height=300 )
    )
    
})    
}

shinyApp(ui, server)
