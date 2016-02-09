if (!(exists('nba') & 
      exists('lineup_stats') & 
      exists('player_stats') & 
      exists('both_stats') & 
      exists('max_minutes'))) 
{
  print('loading data')
  print(getwd())
  nba = read.csv('data/nba.csv.bz2')
  print('done loading data')
  nba$X = NULL
  names = names(nba)[sapply(nba, is.numeric)]
  lineup_stats = gsub('.lineup', '', names[grep('.lineup', names)])[2:70]
  player_stats = gsub('.player', '', names[grep('.player', names)])[4:90]
  both_stats = intersect(lineup_stats, player_stats)
  max_minutes = max(nba$MIN_TOTAL.lineup)
}

lineup_stat_tip = paste0(
  'A Lineup Stat tells us the performance on a given metric of the 5 players in a ',
  'given lineup while they shared the floor. ',
  'Most stats are per 100 plays.'
)
player_stat_tip =  paste0(
  'A Player Stat tells us the average performance on a given metric of the 5 players ',
  'in a given lineup, regardless of whether they were playing together or not. ',
  'Most stats are per 100 plays.'
)
usage_tip = paste0(
  'Check this box to use Usage-Weighted Stats. ',
  'Each player\'s usage percentage will be used as a weight when calculating the average. ',
  'This is most applicable to offensive stats.'
)
minute_filter_tip = paste0(
  'Our sample is limited to lineups where the total minutes played together are within this range.'
)
dim_stat_tip = paste0(
  'Select a stat for investigation of diminishing returns.'
)
density_stat_tip = 'Select a stat.'
sum_tip = paste0(
  'Check this box to multiply the averages by 5.',
  'This is suggested for \'counting stats\'.'
)
