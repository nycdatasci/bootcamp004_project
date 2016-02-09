library(dplyr)
library(ggplot2)

mlb_data = read.csv('https://raw.githubusercontent.com/mtpa/sads/master/SADS_Chapter_7/bobbleheads.csv', sep = ',', header = TRUE)
View(mlb_data)
###Adding Column For League###
AL = c('Baltimore Orioles', 'Boston Red Sox', 'Chicago White Sox', 'Cleveland Indians', 'Detroit Tigers',
       'Houston Astros', 'Kansas City Royals', 'Los Angeles Angels', 'Minnesota Twins', 'New York Yankees', 
       'Oakland Athletics', 'Seattle Mariners', 'Tampa Bay Rays', 'Texas Rangers', 'Toronto Blue Jays')

league_func = function(x){
  if(x %in% AL) return('American League')
  else
    return('National League')
}

mlb_data$home_league = sapply(mlb_data$home_team, league_func)
mlb_data$away_league = sapply(mlb_data$opponent, league_func)

###Adding Column for Division###
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

mlb_data$home_division = sapply(mlb_data$home_team, division_func)
mlb_data$away_division = sapply(mlb_data$opponent, division_func)

###Adding column to see if there are any promotions being used###
promotion_func = function(x){
  if(x %in% mlb_data[,11:14] == 'YES')
    return('YES')
  else
    return('NO')
}
mlb_data$promotion = ifelse(mlb_data$bobblehead == 'YES', 'YES', 
                            ifelse(mlb_data$fireworks == 'YES', 'YES',
                                   ifelse(mlb_data$shirt == 'YES', 'YES',
                                          ifelse(mlb_data$cap == 'YES', 'YES', 'NO'))))

###Adding Data for Win Percentage###
win_table = read.csv('leagues_MLB_2012-standings_expanded_standings_overall.csv', header = TRUE)
names(win_table)[2] = 'home_team'
merged_data = merge(mlb_data, win_table, by = 'home_team')
names(merged_data)[26] = 'home_W.L.'
View(win_table)
visitor_table = group_by(merged_data, home_team) %>%
  summarise(., mean(home_W.L.))

names(visitor_table) = c('opponent', 'visitor_W.L.')
mlb_data = merge(merged_data, visitor_table, by = 'opponent')

###Adding Lat and Long Table###
lat_long = read.csv('MLB Stadiums.csv')
View(lat_long)
names(lat_long)[1] = 'home_team'
mlb_data = merge(mlb_data, lat_long, by = 'home_team')

#reorder columns
mlb_data = mlb_data[,c(3,4,5,7,1,2,6,8:31)]


