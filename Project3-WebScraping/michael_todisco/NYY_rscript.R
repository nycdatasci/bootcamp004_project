library(ggplot2)
library(dplyr)
nyy_scraped = read.csv('nyy.csv')
yankees = read.csv('Yankees.csv')
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

cat('\n', 'Proportion of Test Set Vaiance Accounted for: ',
    round((with(yankees.test, cor(Attendance, predict_attend)^2)), digits =3), "\n", sep = " ")

yankees.plotting.frame = rbind(yankees.train, yankees.test)

group.labels = c("No Promo", "Yes Promo")
group.symbols = c(21, 24)
group.colors = c('black', 'black')
group.fill = c('black', 'red')
xyplot(predict_attend / 1000 ~ Attendance / 1000 | training_test,
       data = yankees.plotting.frame, groups = Promo, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2,1), xlim = c(30, 55), ylim = c(30,55),
       aspect = 1, type = c('p', 'g'),
       panel = function(x, y, ...)
       {panel.xyplot(x, y, ...)
         panel.segments(35, 35, 50, 50, col = "black", cex = 2)
         },
       strip = function(...) strip.default(..., style = 1),
       xlab = "Actual Attendance (thousands)",
       ylab = "Predicted Attendance (thousands)",
       key = list(space = 'top',
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols),
                                col = rev(group.colors),
                                fill = rev(group.fill))))

no_march = subset(yankees, Month != 'March')

my.model.fit = lm(train.model, data = no_march)
summary(my.model.fit)
anova(my.model.fit)
plot(my.model.fit)

library(car)
influencePlot(my.model.fit)

vif(my.model.fit)
avPlots(my.model.fit)
