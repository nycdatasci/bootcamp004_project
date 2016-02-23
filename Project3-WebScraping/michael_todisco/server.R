library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(ggvis)
require(datasets)
library(DT)
library(dplyr)

nyy_scraped = read.csv('nyy.csv')
yankees = read.csv('yankees.csv')
next_season = read.csv('yankees_2016.csv')

###Adding Column For Opponent's League###
AL = c('Baltimore Orioles', 'Boston Red Sox', 'Chicago White Sox', 'Cleveland Indians', 'Detroit Tigers',
       'Houston Astros', 'Kansas City Royals', 'Los Angeles Angels', 'Minnesota Twins', 'New York Yankees', 
       'Oakland Athletics', 'Seattle Mariners', 'Tampa Bay Rays', 'Texas Rangers', 'Toronto Blue Jays')

league_func = function(x){
  if(x %in% AL) return('American League')
  else
    return('National League')
}

yankees$Opp_league = sapply(yankees$Opp, league_func)

###Adding Column for Opponent's Division###
AL_East = c('New York Yankees', 'Boston Red Sox', 'Tampa Bay Rays', 'Toronto Blue Jays', 'Baltimore Orioles')
AL_Central = c('Cleveland Indians', 'Chicago White Sox', 'Detroit Tigers', 'Kansas City Royals', 'Minnesota Twins')
AL_West = c('Houston Astros', 'Kansas City Royals', 'Los Angeles Angels', 'Seattle Mariners', 'Oakland Athletics')

NL_East = c('New York Mets', 'Atlanta Braves', 'Miami Marlins', 'Washington Nationals', 'Philadelphia Phillies')
NL_Central = c('Chicago Cubs', 'Cincinnati Reds', 'Pittsburgh Pirates', 'Milwaukee Brewers', 'St. Louis Cardinals')
NL_West = c('San Diego Padres', 'Los Angeles Dodgers', 'Arizona Diamondbacks', 'San Francisco Giants', 'Colorado Rockies')

division_func = function(x){
  if(x %in% AL_East) return('AL East')
  else if(x %in% AL_Central) return('AL Central')
  else if(x %in% AL_West) return('AL West')
  else if(x %in% NL_East) return('NL East')
  else if(x %in% NL_Central) return('NL Central')
  else 
    return('NL West')
}

yankees$Opp_division = sapply(yankees$Opp, division_func)

###Merge Scraped Promotion Data###
yankees = merge(yankees, nyy_scraped, by = 'Date', all.x = TRUE)

#Drop opponent column
yankees$Opponent = NULL

###Add Yes/No Column for Promotion###
yankees$Promo = ifelse(is.na(yankees$Promotion) == TRUE, 'NO', 'YES')

yankees$Month = factor(yankees$Month, levels = c('March', 'April', 'May', 'June', 'July', 'August', 'September', 'October'))
yankees$DOW = factor(yankees$DOW, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

######MODELING#####
#Train and test set
set.seed(0)
training_test = c(rep(1, length = trunc((2/3) * nrow(yankees))),
                  rep(2, length = (nrow(yankees) - trunc((2/3) * nrow(yankees)))))
yankees$training_test = sample(training_test) #random permutation
yankees$training_test = factor(yankees$training_test,
                               levels = c(1,2), labels = c("TRAIN", "TEST"))
yankees.train = subset(yankees, training_test == 'TRAIN')
yankees.test = subset(yankees, training_test == 'TEST' & Month != 'March')

#Model
train.model = lm(Attendance ~ Opening_Day + Month + DOW + Year + DayNight + Opp + Promo, data = yankees.train)
summary(train.model)
yankees.train$predict_attend = predict(train.model)

yankees.test$predict_attend = predict(train.model, newdata = yankees.test)

next_season$predict_attend = predict(train.model, newdata = next_season)

next_season$Pred_Attendance = prettyNum(round(next_season$predict_attend), big.mark = ',')

################################
#############SHINY##############
################################

function(input, output){

  yanks_data = reactive({subset(yankees,
                                Year == input$year &
                                Temp >= input$temp[1] &
                                Temp <= input$temp[2] &
                                Sky %in% input$weather &
                                Promo %in% input$promo &
                                DayNight %in% input$day_v_night)
  })
  
  next_season_filter = reactive({subset(next_season,
                                        Game_ID >= input$home_num1 &
                                          Game_ID <= input$home_num2)
  })

  output$average_attend = renderValueBox({
    valueBox(prettyNum(round(mean(yanks_data()$Attendance)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
    })
  
  output$average_attend2 = renderValueBox({
    valueBox(prettyNum(round(mean(yanks_data()$Attendance)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
    })

  output$average_attend3 = renderValueBox({
    valueBox(prettyNum(round(mean(yanks_data()$Attendance)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
    })
  
  output$average_attend4 = renderValueBox({
    valueBox(prettyNum(round(mean(next_season$predict_attend)), big.mark = ','), "Average Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$max_attend = renderValueBox({
    valueBox(prettyNum(round(max(yanks_data()$Attendance)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$max_attend2 = renderValueBox({
    valueBox(prettyNum(round(max(yanks_data()$Attendance)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$max_attend3 = renderValueBox({
    valueBox(prettyNum(round(max(yanks_data()$Attendance)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$max_attend4 = renderValueBox({
    valueBox(prettyNum(round(max(next_season$predict_attend)), big.mark = ','), "Maximum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$min_attend = renderValueBox({
    valueBox(prettyNum(round(min(yanks_data()$Attendance)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$min_attend2 = renderValueBox({
    valueBox(prettyNum(round(min(yanks_data()$Attendance)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$min_attend3 = renderValueBox({
    valueBox(prettyNum(round(min(yanks_data()$Attendance)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$min_attend4 = renderValueBox({
    valueBox(prettyNum(round(min(next_season$predict_attend)), big.mark = ','), "Minimum Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$total_attend = renderValueBox({
    valueBox(prettyNum(round(sum(yanks_data()$Attendance)), big.mark = ','), "Total Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$total_attend2 = renderValueBox({
    valueBox(prettyNum(round(sum(yanks_data()$Attendance)), big.mark = ','), "Total Attendance", icon = icon('ticket'), color = 'yellow')
  })
  output$total_attend3 = renderValueBox({
    valueBox(prettyNum(round(sum(yanks_data()$Attendance)), big.mark = ','), "Total Attendance", icon = icon('ticket'), color = 'yellow')
  })
  
  output$total_attend4 = renderValueBox({
    valueBox(prettyNum(round(sum(next_season$predict_attend)), big.mark = ','), "Total Attendance", icon = icon('ticket'), color = 'yellow')
  })

  output$plot1 = renderPlot({
    ggplot(yanks_data(), aes(x = (Attendance / 1000), y = Opp, color = DayNight, shape = Promo)) + 
      geom_point(size = 3) +
      theme_bw() +
      scale_color_manual(values = c('skyblue2', 'black'), guide = guide_legend(title = 'game time')) +
      xlab('Attendance (thousands)') +
      ylab('Opponent') +
      scale_x_continuous(limits = c(30,50), breaks = c(30, 35, 40, 45, 50))
  })
  
  output$info = renderText({
    paste0("Attendance = ", round(input$plot_click$x * 1000))
  })
  
  
  output$plotDOW = renderPlot({
    ggplot(yanks_data(), aes(DOW, (Attendance/1000))) +
      geom_boxplot() +
      xlab('') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(30,50), breaks = c(30, 35, 40, 45, 50)) +
      scale_x_discrete(limits = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    
  })
  output$plotMonth = renderPlot({
    ggplot(yanks_data(), aes(Month, (Attendance/1000))) +
      geom_boxplot() +
      xlab('') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(30,50), breaks = c(30, 35, 40, 45, 50)) +
      scale_x_discrete(limits = c('March', 'April', 'May', 'June', 'July', 'August', 'September', 'October'))
  })
  
  output$plotSeason = renderPlot({
    ggplot(yanks_data(), aes(Game_ID, (Attendance/1000))) + geom_line(stat = 'identity', 
                                                                color = 'darkgreen') +
      geom_point(color = 'black') +
      theme_bw() +
      xlab('Home Game Number') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(30,50), breaks = c(30, 35, 40, 45, 50)) +
      scale_x_continuous(limits = c(0,80), breaks = c(0, 5, 10, 15, 20, 25, 30, 35,
                                                      40, 45, 50, 55, 60, 65, 70, 75, 80
                                                      ))
  })
  
  output$plotPrediction = renderPlot({
    ggplot(next_season, aes(Game_ID, (predict_attend/1000))) + geom_line(stat = 'identity', 
                                                                         color = 'darkgreen') +
      geom_point(size = 2) +
      theme_bw() +
      scale_color_manual(values = c('skyblue2', 'black'), guide = guide_legend(title = 'game time')) +
      xlab('Home Game Number') +
      ylab('Attendance (thousands)') +
      scale_y_continuous(limits = c(30,50), breaks = c(30, 35, 40, 45, 50)) +
      scale_x_continuous(limits = c(0,80), breaks = c(0, 5, 10, 15, 20, 25, 30, 35,
                                                      40, 45, 50, 55, 60, 65, 70, 75, 80
                                                      ))
  })
  
  output$PredTable = renderGvis({
    gvisTable(next_season_filter()[,c(-2,-4, -10, -11)], options = list(page = 'enable'))
  })
  
  output$table2 = renderGvis({
    gvisTable(yankees[-1],
              options = list(page='enable'))
  })
  
  
}
  



