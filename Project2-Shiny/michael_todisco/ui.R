library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(ggvis)
require(datasets)

mlb_data = read.csv('mlb_data.csv')

dashboardPage(
  dashboardHeader(title = 'MLB 2012 Attendance'),
  dashboardSidebar(
    sidebarUserPanel("Michael Todisco",
                     image = 'http://www.userlogos.org/files/logos/johnboy41/mlb%20black.png'),
    sidebarMenu(
      selectizeInput("selected", "Home Team",
                     choices = list('Baltimore Orioles', 'Boston Red Sox', 'Chicago White Sox', 'Cleveland Indians', 'Detroit Tigers',
                                    'Houston Astros', 'Kansas City Royals', 'Los Angeles Angels', 'Minnesota Twins', 'New York Yankees', 
                                    'Oakland Athletics', 'Seattle Mariners', 'Tampa Bay Rays', 'Texas Rangers', 'Toronto Blue Jays',
                                    'New York Mets', 'Atlanta Braves', 'Miami Marlins', 'Washington Nationals', 'Philadelphia Phillies',
                                    'Chicago Cubs', 'Cincinnati Reds', 'Pittsburgh Pirates', 'Milwaukee Brewers', 'St. Louis Cardinals',
                                    'San Diego Padres', 'Los Angeles Dodgers', 'Arizona Diamondbacks', 'San Francisco Giants', 'Colorado Rockies')),
      menuItem("Team Attendance", tabName = 'teamAttendance', icon = icon('bar-chart-o')),
      menuItem("Day of the Week", tabName = 'DOW', icon = icon('calendar')),
      menuItem('Season', tabName = 'season', icon = icon('bar-chart-o')),
      checkboxGroupInput('day_v_night', 'Game Time', choices = list('Day', 'Night'),
                             selected = c('Day', 'Night')),
      sliderInput('temp', "Temperature", 30, 110, value = c(30,110), step = NULL, round = FALSE),
      checkboxGroupInput('weather', 'Weather', choices = list('Clear', 'Cloudy', 'Rainy', 'Dome'),
                         selected = c('Clear', 'Cloudy', 'Rainy', 'Dome')),
      checkboxGroupInput('promo', 'Promotion', choices = list('YES', 'NO'),
                         selected = c('YES', 'NO')),
      sliderInput('opp_perc', "Opponent\'s Winning Percentage", .300, .700, value = c(.300,.7000), step = NULL, round = FALSE),
      menuItem("Data", tabName = 'data', icon = icon('database'))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName= 'teamAttendance',
              fluidRow(
                h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                    textOutput('text1'))),
                
                valueBoxOutput('capacity', width = 3),
                valueBoxOutput('average_attend', width = 3),
                valueBoxOutput('max_attend', width = 3),
                valueBoxOutput('min_attend', width = 3),
                
                box(title = '2012 Gameday Attendance', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plot1'))
              )),
      tabItem(tabName = 'DOW',
            fluidRow(
              h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                     textOutput('text2'))),
              
              valueBoxOutput('capacity2', width = 3),
              valueBoxOutput('average_attend2', width = 3),
              valueBoxOutput('max_attend2', width = 3),
              valueBoxOutput('min_attend2', width = 3),
              
              box(title = 'Day of the Week', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                  plotOutput('plotDOW')),
              box(title = 'Month', width = 6, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                  plotOutput('plotMonth'))
            )),
      tabItem(tabName = 'season',
              fluidRow(
                h4(box(width = 12, height = 50, status = 'info', solidHeader = TRUE, collapsible = FALSE,
                       textOutput('text3'))),
                
                valueBoxOutput('capacity3', width = 3),
                valueBoxOutput('average_attend3', width = 3),
                valueBoxOutput('max_attend3', width = 3),
                valueBoxOutput('min_attend3', width = 3),
                
                box(title = 'Season Trending Attendance', width = 12, status = 'success', solidHeader = TRUE, collapsible = FALSE,
                    plotOutput('plotSeason'))
              )),
      tabItem(tabName = 'data', 
              fluidRow(
                box(htmlOutput('table2'), width = 20, height = 400)
              ))
      )
  )
)
