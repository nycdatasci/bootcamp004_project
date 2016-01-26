#Load libraries
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)

#read in combine data to R
combine_data = tbl_df(read.csv('combine.csv', header=TRUE, stringsAsFactors = FALSE))

#add drafted vs not drafted column
combine_data = mutate(combine_data, drafted = (ifelse(picktotal > 0, 'Drafted', 'Not Drafted')))

#In 'round' column, change round = 0 to round = ND for players that have not been drafted
combine_data = mutate(combine_data, round = ifelse(round == 0, 'ND', round))

###CREATING SUMMARY TABLE####
num_player_data = group_by(combine_data, year) %>%
  summarise(., num_players = length(name))

fortyyd_data = filter(combine_data, fortyyd > 0)
broad_data = filter(combine_data, broad > 0)
vertical_data = filter(combine_data, vertical > 0)
bench_data = filter(combine_data, bench > 0)
three_cone_data = filter(combine_data, threecone > 0)

avg_num_players = mean(num_player_data$num_players)
avg_weight = round(mean(combine_data$weight),2)
avg_40 = format(round(mean(fortyyd_data$fortyyd),2), nsmall = 2)
avg_vertical = round(mean(vertical_data$vertical),2)
avg_broad = round(mean(broad_data$broad),2)
avg_bench = format(round(mean(bench_data$bench),2), nsmall = 2)
avg_three_cone = round(mean(three_cone_data$threecone),2)

summary_data = data.frame(avg_num_players, avg_weight, avg_40, avg_vertical, avg_broad, avg_bench, avg_three_cone)

#height and weight plot#
#smooth plot
qplot(weight, heightinchestotal, data = combine_data, geom = 'smooth') +
  theme_bw() +
  xlab('lbs') +
  ylab('inches') +
  ggtitle('Height and Weight')

###bench press vs vertical jump scatter plot###
#no clear correlation
p2 = filter(vertical_data, bench > 0, pos_class != 'FB', pos_class != 'ST')
qplot(vertical, bench, data = p2, color = pos_class, geom = 'jitter') +
  ggtitle('Bench Press vs Vertical Jump') +
  scale_color_brewer(name = 'Position', palette = 'RdBu') +
  xlab('vertical (in)') +
  ylab('bench (reps)') +
  coord_fixed(ratio = .35) +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15))
  
#merge university data to apply state column based on college name
university_data = tbl_df(read.csv('Accreditation_2015_12.csv', header=TRUE, stringsAsFactors = FALSE))
univ_data = unique(university_data[ ,c('college', 'Institution_State', 'Institution_Zip')])

merged_data = merge(combine_data1, univ_data, by = 'college', all.x = TRUE)

state_summary = filter(merged_data, Institution_State > 0) %>%
  group_by(., Institution_State) %>%
  summarise(., num_players = length(college))

state_summary$region = state_summary$Institution_State
state_summary = sapply(state_summary, function(x) tolower(x))

###Plot State Map###
states = map_data('state')

total = merge(states, state_summary, by = 'region') %>%
  arrange(., order)

p = ggplot()
p = p + geom_polygon(data = total, aes(x = long, y = lat, group = group, fill = num_players))
p = p + ggtitle('# Players by College State')
p = p + theme_bw() + coord_map()

#Categorize side of the ball
offense = c('C', 'OG', 'OT', 'OC', 'FB', 'RB', 'QB', 'WR', 'TE')
defense = c('FS', 'SS', 'CB', 'DE', 'DT', 'NT', 'ILB', 'OLB')

side_of_ball = function(x){
  if(x %in% offense) return('Offense')
  else if(x %in% defense) return('Defense')
  else return('Special Teams')
}
#adding new column to cateogrize side of ball
combine_data$side_of_ball = apply(combine_data[,'position'], 1, side_of_ball)

#choose only players that were selected in the 1st round
first_round_data = arrange(filter(combine_data, round == 1), year, picktotal)

###First Round Side of Ball Plot###
ggplot(first_round_data, aes(year, fill = side_of_ball)) + 
  geom_bar(position = 'fill') + 
  ggtitle('First Round - Offense vs Defense') +
  scale_fill_brewer(palette = 'Paired', name = 'Side of Ball') +
  ylab('Percentage') +
  coord_fixed(ratio = 12) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))

#group positions to make visualization easier
defensive_backs = defense[1:3]
defensive_line = defense[4:6]
linebacker = defense[7:8]
offensive_line = offense[1:4]

pos_class = function(x){
  if(x %in% defensive_backs) return('DB')
  else if(x %in% defensive_line) return('DL')
  else if(x %in% linebacker) return('LB')
  else if(x %in% offensive_line) return('OL')
  else if(x == 'FB') return('FB')
  else if(x == 'RB') return('RB')
  else if(x == 'QB') return('QB')
  else if(x == 'WR') return('WR')
  else if(x == 'TE') return('TE')
  else return ('ST')
}

#adding new column to categorize positions
first_round_data$pos_class = apply(first_round_data[,'position'], 1, pos_class)

###First Round Position Plot###
ggplot(first_round_data, aes(year, fill = pos_class)) + 
  geom_bar(position = 'fill') + 
  ggtitle('First Round - Positions') +
  scale_fill_brewer(palette = 'Paired', name = 'Position') +
  ylab('Percentage') +
  coord_fixed(ratio = 12) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))

###Best and Worst 40 Times###
best_forty = select(arrange(filter(fortyyd_data, fortyyd > 0), fortyyd), year, name, position, fortyyd)
best_forty[1:10,]

worst_forty = select(arrange(filter(fortyyd_data, fortyyd > 0), desc(fortyyd)), year, name, position, fortyyd)
worst_forty[1:10,]

###Distribution of 40 Yard Dash Times###

plot(hist(fortyyd_data$fortyyd), xlab = "seconds",
     main = "40 Yard Dash Time Distribution", col = "Red")

###40 YD Boxplot By Position####
qplot(reorder(pos_class, fortyyd), fortyyd, data = p1, geom = 'boxplot') + 
  ggtitle('40 Yard Dash Times') + 
  ylab('Seconds') +
  coord_fixed(ratio = 3) +
  xlab('Position') +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 13))

p1 = filter(fortyyd_data, pos_class != 'ST') %>%
  filter(., pos_class != 'FB')

qplot(fortyyd, data = p1, geom = 'freqpoly', color = pos_class) + 
  facet_wrap(~ pos_class) +
  xlab('Seconds') +
  ggtitle('40 Yard Dash Times by Position') +
  guides(color = 'none') +
  theme_bw()

filter(fortyyd_data, side_of_ball == 'Offense') %>%
  filter(., pos_class != 'FB') %>%
  ggplot(., aes(fortyyd)) +
  geom_density(aes(group=pos_class, fill = pos_class), alpha = 0.3) +
  theme_bw() +
  ggtitle('Offense 40 Yard Dash Time Distribution') +
  xlab('Seconds') +
  scale_fill_discrete(name = 'Position')

filter(fortyyd_data, side_of_ball == 'Defense') %>%
  ggplot(., aes(fortyyd)) +
  geom_density(aes(group=pos_class, fill = pos_class), alpha = 0.3) +
  theme_bw() +
  ggtitle('Defense 40 Yard Dash Time Distribution') +
  xlab('Seconds') +
  scale_fill_discrete(name = 'Position')

a = group_by(fortyyd_data, pos_class) %>%
  summarise(., avg_weight = mean(weight), avg_40 = mean(fortyyd)) %>%
  filter(., pos_class != 'ST')

b = group_by(vertical_data, pos_class) %>%
  summarise(., avg_vertical = mean(vertical)) %>%
  filter(., pos_class != 'ST')

c = group_by(broad_data, pos_class) %>%
  summarise(., avg_broad_jump = mean(broad)) %>%
  filter(., pos_class != 'ST')

d = group_by(bench_data, pos_class) %>%
  summarise(., avg_bench = mean(bench)) %>%
  filter(., pos_class != 'ST')

e = group_by(three_cone_data, pos_class) %>%
  summarise(., avg_threecone = mean(threecone)) %>%
  filter(., pos_class != 'ST')

summary_by_position = data.frame(a, b$avg_vertical, c$avg_broad_jump, d$avg_bench, e$avg_threecone)
summary_by_position$pos_class = factor(summary_by_position$pos_class, 
                                    levels = summary_by_position$pos_class[order(summary_by_position$avg_40)])

ggplot(summary_by_position, aes(pos_class, avg_40), avg_40) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Blues') +
  coord_cartesian(ylim = c(4.00, 5.50)) +
  theme_bw() +
  xlab('position') +
  ylab('seconds') +
  ggtitle('Average 40 Yard Dash by Position')

###40 Times by Year Plot####
qplot(year, fortyyd, data = fortyyd_data, geom = 'smooth') +
  ggtitle('40 Yard Dash Times by Year') +
  ylab('seconds') +
  theme_bw()

###RB Forty times by round bar plot###
rb_40 = filter(fortyyd_data, position == 'RB')

summary_by_round = group_by(rb_40, round) %>%
  summarise(., avg_40 = mean(fortyyd))

ggplot(summary_by_round, aes(round, avg_40)) +
  geom_bar(stat = 'identity') + 
  coord_cartesian(ylim = c(4.3, 4.7)) +
  theme_bw() +
  xlab('round') +
  ylab('seconds') +
  ggtitle('RB Average 40 Yard Dash by Round') +
  theme(plot.title = element_text(size = 22),
        axis.title = element_text(size = 18))

###RB 40 times by draft pick###
rb_data = filter(combine_data, fortyyd > 0 & position == 'RB' & picktotal > 0)
qplot(picktotal, fortyyd, data = rb_data, geom = 'smooth') +
  ggtitle('40 Yard Dash Times by Draft Pick') +
  xlab('Draft Pick') +
  ylab('Seconds') +
  theme_bw()

###Percent of players not drafted###
not_last_year = filter(combine_data, year != 2015)
total_players = nrow(not_last_year)
filter(combine_data, year != 2015) %>%
  group_by(., drafted) %>%
  summarise(., '# of players' = length(drafted), 
            '% of players' = (length(drafted) / total_players) * 100)

###RB Boxplot - Drafted vs Not Drafted####
zoom = ylim(4.1,5.1)
qplot(drafted, fortyyd, data = rb_40, geom = 'boxplot') + 
  zoom + ggtitle('Running Back 40 Yard Dash Times') + 
  xlab("") + ylab('Seconds') +
  coord_fixed(ratio = 1) +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15))


