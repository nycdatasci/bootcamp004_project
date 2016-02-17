mlb_data = read.csv('mlb_data.csv')

mlb_data$day_of_week = factor(mlb_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                                               "Thursday", "Friday", "Saturday", "Sunday"))
mlb_data$month = factor(mlb_data$month, levels = c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT"))