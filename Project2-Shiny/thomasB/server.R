library(shiny)
library(leaflet)
library(grDevices)
library(dplyr)
library(Hmisc)
library(maps)
library(ggplot2)

#source("/Users/boulenge/Desktop/Projects/Project2 - Shiny/Shiny-TomB/helpers.R")
source("helpers.R")

test = read.csv("/Users/boulenge/Shiny/ShinyTomB/data/edusample.csv")

edu200 = read.csv('edu200.csv')
edu500 = read.csv('edu500.csv')
edu1000 = read.csv('edu1000.csv')
edufull_red = read.csv('edufull_red.csv')

######## trim function #########
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
################################


shinyServer(function(input, output, session) {
  
  
  ############################################
  #State Averages
  #Zoom = reactive({input$varECZ})
  
  state_map = reactive({
    ###### choose sample ######
    if (input$varSamp == '200') {
      states.map = states.map200
    } else if (input$varSamp == '500') {
      states.map = states.map500
    } else if (input$varSamp == '1000') {
      states.map = states.map1000
    } else {
      states.map = states.mapfull_red
    }
    states.map
  })
  
  edu_zip = reactive({
    ###### choose sample ######
    if (input$varSamp == '200') {
      edu.zip = edu.zip200
    }
    else if (input$varSamp == '500') {
      edu.zip = edu.zip500
    }
    else if (input$varSamp == '1000') {
      edu.zip = edu.zip1000
    }
    else {
      edu.zip = edu.zipfull_red
    }
    edu.zip
  })
  
  output$map <- renderPlot({
    
    states.map = state_map()
    
#  if (input$varZip == FALSE) {
    
    if (input$varECZ == TRUE) {
       states.map = states.map[states.map$region %in% ec.st,]
    }
    
    if (input$varM == 'faminc') {    
      p = ggplot(data = states.map) +
            geom_polygon(aes(x = long, y = lat, fill = faminc, group = group), colour = 'black') + 
            coord_fixed(1.3) +
            ggtitle('Average family income of College students') +
            theme(plot.title = element_text(size=22)) +
            scale_fill_gradient(low = "blue", high = "red")
    }
    if (input$varM == 'TUITFTE') {
      p = ggplot(data = states.map) +
        geom_polygon(aes(x = long, y = lat, fill = TUITFTE, group = group), colour = 'black') + 
        coord_fixed(1.3) +
        ggtitle('Average Tuition per State') +
        theme(plot.title = element_text(size=22)) +
        scale_fill_gradient(low = "blue", high = "red")
    }
    if (input$varM == 'CDR3') {
      p = ggplot(data = states.map) +
        geom_polygon(aes(x = long, y = lat, fill = CDR3, group = group), colour = 'black') + 
        coord_fixed(1.3) +
        ggtitle('Average 3-year cohort Default Rate') +
        theme(plot.title = element_text(size=22)) +
        scale_fill_gradient(low = "blue", high = "red")
    }
    if (input$varM == 'DEBT_MDN') {
      p = ggplot(data = states.map) +
        geom_polygon(aes(x = long, y = lat, fill = DEBT_MDN, group = group), colour = 'black') + 
        coord_fixed(1.3) +
        ggtitle("Median College students' Loan Debt" ) +
        theme(plot.title = element_text(size=22)) +
        scale_fill_gradient(low = "blue", high = "red")
    }
    if (input$varM == 'PREDDEG') {
      p = ggplot(data = states.map) +
        geom_polygon(aes(x = long, y = lat, fill = PREDDEG, group = group), colour = 'black') + 
        coord_fixed(1.3) +
        ggtitle('Predominant Degree Type') +
        theme(plot.title = element_text(size=22)) +
        scale_fill_gradient(low = "blue", high = "red")
    }
    p
    #ggplot(data = states.map) +
    #  geom_polygon(aes(x = long, y = lat, fill = input$varM, group = group), colour = 'black') + 
    #  coord_fixed(1.3) +
    #   theme(legend.position="bottom")
#  }
#   else {
#     
#     edu.zip = edu_zip()
#     
#     data(zip.regions)
#    if (input$varM == 'faminc') {     
#       states.map2 = as.data.frame(edu.zip) %>%
#         dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#         rename(., region = zip, value = faminc) %>%
#         filter(., abb %in% ec.st.abb) %>%
#         select(., region, value) %>%
#         filter(., region %in% zip.regions$region)
#       
#         states.map2 = states.map2[!duplicated(states.map2$region), ]
#       
#         zip_choropleth(states.map2, state_zoom = ec.st,
#                      title = "Average Family incomes (yearly) per school and zip code",
#                      legend = "Income")
#     }
#     if (input$varM == 'TUITFTE') {
#       states.map2 = as.data.frame(edu.zip) %>%
#         dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#         rename(., region = zip, value = TUITFTE) %>%
#         filter(., abb %in% ec.st.abb) %>%
#         select(., region, value) %>%
#         filter(., region %in% zip.regions$region)
#       
#       states.map2 = states.map2[!duplicated(states.map2$region), ]
#       
#       zip_choropleth(states.map2, state_zoom = ec.st,
#                      title = "Average Tuition (yearly) per school and zip code",
#                      legend = "Average Tuition")
#     }
#     if (input$varM == 'CDR3') {
#       states.map2 = as.data.frame(edu.zip) %>%
#         dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#         rename(., region = zip, value = CDR3) %>%
#         filter(., abb %in% ec.st.abb) %>%
#         select(., region, value) %>%
#         filter(., region %in% zip.regions$region)
#       
#       states.map2 = states.map2[!duplicated(states.map2$region), ]
#       
#       zip_choropleth(states.map2, state_zoom = ec.st,
#                      title = "3 year Default Rate per school and zip code",
#                      legend = "Default Rate")
#     }
#     if (input$varM == 'DEBT_MDN') {
#       states.map2 = as.data.frame(edu.zip) %>%
#         dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#         rename(., region = zip, value = DEBT_MDN) %>%
#         filter(., abb %in% ec.st.abb) %>%
#         select(., region, value) %>%
#         filter(., region %in% zip.regions$region)
#       
#       states.map2 = states.map2[!duplicated(states.map2$region), ]
#       
#       zip_choropleth(states.map2, state_zoom = ec.st,
#                      title = "Median Loan debt per school and zip code",
#                      legend = "Median Loan debt")
#     }
#     if (input$varM == 'PREDDEG') {
#       states.map2 = as.data.frame(edu.zip) %>%
#         dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#         rename(., region = zip, value = PREDDEG) %>%
#         filter(., abb %in% ec.st.abb) %>%
#         select(., region, value) %>%
#         filter(., region %in% zip.regions$region)
#       
#       states.map2 = states.map2[!duplicated(states.map2$region), ]
# 
#       zip_choropleth(states.map2, state_zoom = ec.st,
#                title = "Predominant degree type per school and zip code",
#                legend = "Degree type")
#     }
    
    
    
#    data(zip.regions)
#    states.map2 = as.data.frame(edu.zip) %>%
#      dplyr::mutate(., zip = sapply(zip, function(x)unlist(strsplit(x, '-'))[1])) %>%
#      dplyr::rename(., region = zip, value = switch(input$varM, 
#                                              faminc = faminc,
#                                              TUITFTE = TUITFTE,
#                                              CDR3 = CDR3, 
#                                              DEBT_MDN = DEBT_MDN,
#                                              PREDDEG = PREDDEG)) %>%
#        filter(., abb %in% ec.st.abb) %>%
#        select(., region, value) %>%
#        filter(., region %in% zip.regions$region)
#     
#   zip_choropleth(states.map2, state_zoom = ec.st,
#                  title = paste(switch(input$varM,
#                                  "faminc" = "Average Family incomes (yearly)",                                 "TUITFTE" = "Average Tuition (yearly)",
#                                 "CDR3" = "3 year Default Rate", 
#                                 "DEBT_MDN" = "Median Loan debt",
#                                  "PREDDEG" = "Predominant degree type"), "per school and zip code"),
#                   legend = switch(input$varM,
#                                   "faminc" = "Income",
#                                   "TUITFTE" = "Average Tuition",
#                                   "CDR3" = "Default Rate", 
#                                   "DEBT_MDN" = "Median Loan debt",
#                                   "PREDDEG" = "Degree type"))
#  }
  })
  
  output$textMap <- renderUI({
    
    ###### choose sample ######
    if (input$varSamp == '200') {
      states.map = states.map200
    }
    if (input$varSamp == '500') {
      states.map = states.map500
    }
    if (input$varSamp == '1000') {
      states.map = states.map1000
    }
    if (input$varSamp == 'Full') {
      states.map = states.mapfull_red
    }
    
    str0 <- paste("")
    
    df_temp = switch(input$varSamp,
           "200" = edu200,
           "500" = edu500,
           "1000" = edu1000,
           "Full" = edufull_red)
    
    str1 <- paste(
      trim(paste(capitalize(as.character(input$varSt)), "'s")), 
      trim(paste(capitalize(as.character(names(varM[varM == input$varM]))), "(over",
                 length(df_temp$STABBR[df_temp$STABBR == state.abb[tolower(state.name) == input$varSt]])
                 ,"schools):"))
                 )
    
    str2 <- paste(unique(round(states.map[states.map$region == input$varSt, input$varM])), 
                  switch(input$varM,
                         "faminc" = "$ per year" ,
                         "TUITFTE" = "$ per year",
                         "CDR3" = "% default rate",
                         "DEBT_MDN" = "$",
                         "PREDDEG" = paste("degree score", "\n", "(0 = NA, 1 = certificate,
                                           2 = associate, 3 = bachelor, 4 = graduate")
                  ))
    HTML(paste(str0, str1, str2, sep = '<br/>'))
    
  })
  
  
  
  ############################################
  #Data
  output$varTab = renderDataTable(edufull_red)
  
  
  
  
  ############################################
  #Overview
  output$map2 <- renderPlot({
    ggplot(data = states.fam) +
      geom_polygon(aes(x = long, y = lat, fill = Avg.Income, group = group), colour = 'black') + 
      coord_fixed(1.3) +
      ggtitle('Average family income of College students per State') +
      theme(legend.position="bottom")
  })
    
    
  output$map2 =renderLeaflet({
    
    ###### choose sample ######
    if (input$varSamp2 == '200') {
      edu = edu200
    }
    if (input$varSamp2 == '500') {
      edu = edu500
    }
    if (input$varSamp2 == '1000') {
      edu = edu1000
    }
    if (input$varSamp2 == 'Full') {
      edu = edufull_red
    }
    
    ###### select city ######
    if (input$varCty != "") {
      edu = edu[edu$CITY == input$varCty,]
    }
    if (!is.null(input$varSt2) & input$varCty == "") {
      edu = edu[edu$STABBR == input$varSt2,]
    }
    
    popup_func = function(x) {
      switch(input$varInt,
             "INSTNM" = x,
             "female"  = paste("Proportion of Women = ", x),
             "ADM_RATE" = paste("Admission rate = ", x),
             "pct_born_us" = paste("US born = ", x, " %")
            )
    }
    
    leaflet() %>%
      addTiles() %>%
      setView(lat = 39.82, lng = -98.58, zoom = 4) %>%
      addMarkers(lng = edu$LONGITUDE, lat = edu$LATITUDE,
                 popup = popup_func(edu[, input$varInt]), 
                 clusterOptions = markerClusterOptions())    
  })





############################################
#Tuition


output$sliderTui1 <- renderUI({
  edu.var = edufull_red
  
  sliderInput("inSliderTui1", "Tuition Range", min = min(edu.var$TUITFTE, na.rm = T), 
              max = max(edu.var$TUITFTE, na.rm = T), 
              value = c(min(edu.var$TUITFTE, na.rm = T),
                        max(edu.var$TUITFTE, na.rm = T)))
})

output$sliderTui2 <- renderUI({
  edu.var = edufull_red
  edu.var[, input$varTui] = as.numeric(as.character(edu.var[, input$varTui]))
  
  sliderInput("inSliderTui2", "Variable Range", min = min(edu.var[, input$varTui], na.rm = T), 
              max = max(edu.var[, input$varTui], na.rm = T), 
              value = c(min(edu.var[, input$varTui], na.rm = T),
                        max(edu.var[, input$varTui], na.rm = T)))
})

output$map3 <- renderPlot({
  #Subsetting edu by type of school: 1 = public, 2 = private non-profit, 3 = private for profit
  
  edu.var = edufull_red
  
  if (input$varTyp == 'pub') {
    edu.var = subset(edu.var, CONTROL == 1)
  }
  if (input$varTyp == 'priN') {
    edu.var = subset(edu.var, CONTROL == 2)
  }
  if (input$varTyp == 'priP') {
    edu.var = subset(edu.var, CONTROL == 3)
  }
  #Subbsetting edu by variables to plot against Tuition
    
  #p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
  #  geom_point() +
  #  geom_smooth(se = FALSE) +
  #  coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T)))
  
  if (input$varTui == 'CDR3') {
    edu.var = subset(edu.var, !is.na(edu.var$CDR3) & edu.var$CDR3 != 0)
    #p = p + coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T)))
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
  }
  if (input$varTui == 'TUITIONFEE_IN') {
    edu.var$TUITIONFEE_IN = as.numeric(as.character(edu.var$TUITIONFEE_IN))
    edu.var = subset(edu.var, !is.na(edu.var$TUITIONFEE_IN) & edu.var$TUITIONFEE_IN != 0)
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) + 
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      #xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      #ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
  }
  if (input$varTui == 'SATMT75') {
    edu.var$SATMT75 = as.numeric(as.character(edu.var$SATMT75))
    edu.var = subset(edu.var, !is.na(edu.var$SATMT75) & edu.var$SATMT75 != 0)
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
  }
  if (input$varTui == 'UGDS') {
    edu.var$UGDS = as.numeric(as.character(edu.var$UGDS))
    edu.var = subset(edu.var, !is.na(edu.var$UGDS) & edu.var$UGDS != 0)
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +   
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
  }
  if (input$varTui == 'DEBT_MDN') {
    edu.var = subset(edu.var, !is.na(edu.var$DEBT_MDN) & edu.var$DEBT_MDN != 0)
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +  
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
  }
  if (input$varTui == 'GRAD_DEBT_MDN') {
    edu.var$GRAD_DEBT_MDN = as.numeric(as.character(edu.var$GRAD_DEBT_MDN))
    edu.var = subset(edu.var, !is.na(edu.var$GRAD_DEBT_MDN) & edu.var$GRAD_DEBT_MDN != 0)
    p = ggplot(data = edu.var, aes(x = TUITFTE, y = edu.var[, input$varTui])) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_smooth(se = FALSE) +
      theme_bw() +
      #geom_point() +
      #geom_smooth(se = FALSE) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      #coord_cartesian(xlim = c(0, 20000), ylim = c(0, max(edu.var[, input$varTui], na.rm = T))) +
      xlim(input$inSliderTui1[1], input$inSliderTui1[2]) +
      ylim(input$inSliderTui2[1], input$inSliderTui2[2]) +
      ylab(colnames(var.info[var.info == input$varTui]))
  }
  
  p
})



############################################
#Explorer

dataInput = reactive({
  ###### choose sample ######
  if (input$varSamp3 == '200') {
    edu = edu200
  }
  if (input$varSamp3 == '500') {
    edu = edu500
  }
  if (input$varSamp3 == '1000') {
    edu = edu1000
  }
  if (input$varSamp3 == 'Full') {
    edu = edufull_red
  }
  
  edu = edu[, -c(1:6)]
  for (j in 1:ncol(edu)) {
    edu[, j] = as.numeric(as.character(edu[, j]))
  }
  return(edu)
})

output$slider1 <- renderUI({
  #edu = dataInput()
  
  ###### choose sample ######
  if (input$varSamp3 == '200') {
    edu = edu200
  }
  if (input$varSamp3 == '500') {
    edu = edu500
  }
  if (input$varSamp3 == '1000') {
    edu = edu1000
  }
  if (input$varSamp3 == 'Full') {
    edu = edufull_red
  }
  
  edu = edu[, -c(1:6)]
  for (j in 1:ncol(edu)) {
    edu[, j] = as.numeric(as.character(edu[, j]))
  }
  
  sliderInput("inSlider1", "Variable 1 Range", min = min(edu[, input$varT1], na.rm = T), 
                                       max = max(edu[, input$varT1], na.rm = T), 
                                       value = c(min(edu[, input$varT1], na.rm = T),
                                                 max(edu[, input$varT1], na.rm = T)))
})

output$slider2 <- renderUI({
  #edu = dataInput()
  
  ###### choose sample ######
  if (input$varSamp3 == '200') {
    edu = edu200
  }
  if (input$varSamp3 == '500') {
    edu = edu500
  }
  if (input$varSamp3 == '1000') {
    edu = edu1000
  }
  if (input$varSamp3 == 'Full') {
    edu = edufull_red
  }
  
  edu = edu[, -c(1:6)]
  for (j in 1:ncol(edu)) {
    edu[, j] = as.numeric(as.character(edu[, j]))
  }
  
  sliderInput("inSlider2", "Variable 2 Range", min = min(edu[, input$varT2], na.rm = T), 
              max = max(edu[, input$varT2], na.rm = T), 
              value = c(min(edu[, input$varT2], na.rm = T),
                        max(edu[, input$varT2], na.rm = T)))
})


output$map4 <- renderPlot({
  
  #edu = dataInput()
  
   ###### choose sample ######
  if (input$varSamp3 == '200') {
    edu = edu200
  }
  if (input$varSamp3 == '500') {
    edu = edu500
  }
  if (input$varSamp3 == '1000') {
    edu = edu1000
  }
  if (input$varSamp3 == 'Full') {
    edu = edufull_red
  }
  
  edu = edu[, -c(1:6)]
  for (j in 1:ncol(edu)) {
    edu[, j] = as.numeric(as.character(edu[, j]))
  }
  
  ggplot(edu, aes(x = edu[,input$varT1], 
                            y = edu[,input$varT2], 
                            colour = edu[,input$varTG])) +
      theme_bw() + geom_point() + geom_smooth() +
      ggtitle(paste(input$varT2, "against  ", input$varT1)) +
      xlab(input$varT1) + ylab(input$varT2) +
      labs(colour = input$varTG) +
      xlim(input$inSlider1[1], input$inSlider1[2]) +
      ylim(input$inSlider2[1], input$inSlider2[2]) +
      theme(plot.title = element_text(size=22))
  
})

output$textExpl = renderUI({
  ###### choose sample ######
  if (input$varSamp3 == '200') {
    edu = edu200
  }
  if (input$varSamp3 == '500') {
    edu = edu500
  }
  if (input$varSamp3 == '1000') {
    edu = edu1000
  }
  if (input$varSamp3 == 'Full') {
    edu = edufull_red
  }
  
  print(sum(is.na(as.numeric(as.character(edu[, input$varT1])))))
  
  str1 <- paste(input$varT1, " = ", names(var.info[var.info == input$varT1]), "(",
           round(100*sum(is.na(as.numeric(as.character(edu[, input$varT1]))))/length(edu[, input$varT1])), 
                                                                                "% NA)")
  str2 <- paste(input$varT2, " = ", names(var.info[var.info == input$varT2]), "(",
           round(100*sum(is.na(as.numeric(as.character(edu[, input$varT2]))))/length(edu[, input$varT2])), 
                                                                                "% NA)")
  str3 <- paste(input$varTG, " = ", names(var.info[var.info == input$varTG]), "(",
           round(100*sum(is.na(as.character(edu[, input$varT1])))/length(edu[, input$varTG])), "% NA)")
  HTML(paste(str1, str2, str3, sep = '<br/>'))
  
})





############################################
#Histograms

  
  
  })