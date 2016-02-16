library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(RColorBrewer)
require(datasets)

mlb_data = read.csv('mlb_data.csv')
mlb_data$day_of_week = factor(mlb_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                                                         "Thursday", "Friday", "Saturday", "Sunday"))
mlb_data$month = factor(mlb_data$month, levels = c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT"))

function(input, output) {
  
  new_data = reactive({
    mlb_data = subset(mlb_data, 
                      home_team == input$selected &
                      temp >= input$temp[1] &
                        temp <= input$temp[2] &
                        skies %in% input$weather &
                        promotion %in% input$promo &
                        day_night %in% input$day_v_night &
                        visitor_W.L. >= input$opp_perc[1] &
                        visitor_W.L. <= input$opp_perc[2])
    })
    
  output$capacity = renderValueBox({
    valueBox(new_data()$Capacity[1], "Ballpark Capacity", icon = icon('ticket'), color = 'green')
  })
  output$capacity2 = renderValueBox({
    valueBox(new_data()$Capacity[1], "Ballpark Capacity", icon = icon('ticket'), color = 'green')
  })
  output$capacity3 = renderValueBox({
    valueBox(new_data()$Capacity[1], "Ballpark Capacity", icon = icon('ticket'), color = 'green')
  })
  
  output$average_attend = renderValueBox({
    valueBox(prettyNum(round(mean(new_data()$attend)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
    })
  output$average_attend2 = renderValueBox({
    valueBox(prettyNum(round(mean(new_data()$attend)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$average_attend3 = renderValueBox({
    valueBox(prettyNum(round(mean(new_data()$attend)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$max_attend = renderValueBox({
    valueBox(prettyNum(round(max(new_data()$attend)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$max_attend2 = renderValueBox({
    valueBox(prettyNum(round(max(new_data()$attend)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$max_attend3 = renderValueBox({
    valueBox(prettyNum(round(max(new_data()$attend)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$min_attend = renderValueBox({
    valueBox(prettyNum(round(min(new_data()$attend)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$min_attend2 = renderValueBox({
    valueBox(prettyNum(round(min(new_data()$attend)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$min_attend3 = renderValueBox({
    valueBox(prettyNum(round(min(new_data()$attend)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$text1 = renderText({
    paste(new_data()$Stadium.Name[1],'in',new_data()$City[1])
  })
  
  output$text2 = renderText({
    paste(new_data()$Stadium.Name[1],'in',new_data()$City[1])
  })
  
  output$text3 = renderText({
    paste(new_data()$Stadium.Name[1],'in',new_data()$City[1])
  })
  
  output$plot1 = renderPlot({
    ggplot(new_data(), aes(x = (attend / 1000), y = opponent, color = day_night, shape = promotion)) + 
      geom_point(size = 3) +
      geom_vline(aes(xintercept = (mean(mlb_data$attend))/1000), lty = 2,
                                 color = 'red') +
      geom_text(aes(31, 0, label = "MLB Average", vjust = -1)) +
      theme_bw() +
      scale_color_manual(values = c('skyblue2', 'black'), guide = guide_legend(title = 'game time')) +
      xlab('Attendance (thousands)') +
      ylab('Opponent') +
      scale_x_continuous(limits = c(5,55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55))
  })
  
  output$plotDOW = renderPlot({
    qplot(day_of_week, (attend/1000), data = new_data(), 
          geom = 'boxplot', fill = I('lightgray')) +
      theme_bw() +
      xlab('') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(5,55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55))
  })
  
  output$plotMonth = renderPlot({
    qplot(month, (attend/1000), data = new_data(), 
          geom = 'boxplot', fill = I('lightgray')) +
      theme_bw() +
      xlab('') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(5,55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55))
  })
  
  output$plotSeason = renderPlot({
    ggplot(new_data(), aes(GameNum, (attend/1000))) + geom_line(stat = 'identity', 
                                                        color = 'darkgreen') +
      geom_point(color = 'black') +
      geom_hline(aes(yintercept = (mean(mlb_data$attend))/1000), lty = 2,
                 color = 'red') +
      geom_text(aes(83, 30, label = "MLB Average", vjust = -1 )) +
      theme_bw() +
      xlab('Home Game Number') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(5,55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55))
  })
  
  output$table2 = renderGvis({
    gvisTable(mlb_data[-1],
              options = list(page='enable'))
    })
}

