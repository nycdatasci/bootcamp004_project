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

################################
#############SHINY##############
################################

dashboardPage(
  dashboardHeader(title = 'New York Yankees'),
  dashboardSidebar(
    sidebarUserPanel("Michael Todisco",
                     image = 'http://www.findthatlogo.com/wp-content/gallery/new-york-yankee-logos/yankees-ball-logo.jpg'),
    sidebarMenu(
      menuItem("Attendance by Opponent", tabName = 'teamAttendance', icon = icon('bar-chart-o')),
      menuItem("Month and Day", tabName = 'DOW', icon = icon('calendar')),
      menuItem('Season Trending', tabName = 'season', icon = icon('line-chart')),
      
      selectizeInput("year", "Season",
                     choices = list('2009', '2010', '2011', '2012', '2013', '2014', '2015'), selected = '2015'),
      checkboxGroupInput('day_v_night', 'Game Time', choices = list('Day', 'Night'),
                         selected = c('Day', 'Night')),
      sliderInput('temp', "Temperature", 30, 110, value = c(30,110), step = NULL, round = FALSE),
      checkboxGroupInput('weather', 'Weather', choices = list('Clear', 'Cloudy', 'Rain'),
                         selected = c('Clear', 'Cloudy', 'Rain')),
      checkboxGroupInput('promo', 'Promotion', choices = list('YES', 'NO'),
                         selected = c('YES', 'NO')),
      menuItem('2016 Prediction', tabName = 'prediction', icon = icon('bar-chart-o')),
      menuItem("Data", tabName = 'data', icon = icon('database'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName= 'teamAttendance',
              fluidRow(
                
                valueBoxOutput('average_attend', width = 3),
                valueBoxOutput('max_attend', width = 3),
                valueBoxOutput('min_attend', width = 3),
                valueBoxOutput('total_attend', width = 3),
                
                box(title = 'New York Yankees Attendance', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plot1', click = 'plot_click'),
                    verbatimTextOutput('info'),
                    DT::dataTableOutput('plot_clicked_points'))
              )),
      tabItem(tabName = 'DOW',
              fluidRow(
                
                valueBoxOutput('average_attend2', width = 3),
                valueBoxOutput('max_attend2', width = 3),
                valueBoxOutput('min_attend2', width = 3),
                valueBoxOutput('total_attend2', width = 3),
                
                box(title = 'Day and Month', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotDOW')),
                box(title = 'Month', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotMonth'))
              )),
      tabItem(tabName = 'season',
              fluidRow(
                
                valueBoxOutput('average_attend3', width = 3),
                valueBoxOutput('max_attend3', width = 3),
                valueBoxOutput('min_attend3', width = 3),
                valueBoxOutput('total_attend3', width = 3),
                
                box(title = 'Season Trending Attendance', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotSeason'))
              )),
      tabItem(tabName = 'prediction',
              fluidRow(
                valueBoxOutput('average_attend4', width = 3),
                valueBoxOutput('max_attend4', width = 3),
                valueBoxOutput('min_attend4', width = 3),
                valueBoxOutput('total_attend4', width = 3),
                
                box(title = '2016 Predicted Attendance', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotPrediction')),
                
                box(numericInput('home_num1', 'Home Game Start', value = 1), width = 2),
                box(numericInput('home_num2', 'Home Game End', value = 10), width = 2),
                
                box(htmlOutput('PredTable'), width = 8)
              )),
      tabItem(tabName = 'data', 
              fluidRow(
                box(htmlOutput('table2'), width = 20, height = 400)
              ))
    )
  )
)