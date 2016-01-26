# install the necessary packages:

library(dplyr)
library(ggplot2)

#______________________________________________________________________________
# read in the data from the appropriate text file:

tab_log = read.csv("/Users/adamcone/Desktop/Project1Visualization/tables/tab_log.tst",
                   header = FALSE,
                   sep = "\t")
#______________________________________________________________________________
#Data Cleaning and Reformatting

# As per Steven's email on Sunday, January 24, any NewBalance values above
# $500 is an Open Produce special case and should not be considered in 
# the general tab_log data. Therefore, the first thing for me to do
# is to find all the CustomerIDs that correspond with values over $500
# in tab log, and to rid the data of these entries. tab_log, the original
# data, has 26,373 rows.

# Next I will rename the columns of tab_log:

names(tab_log) = c("ID",
                   "CustomerID",
                   "OldBalance",
                   "NewBalance",
                   "WhenLogged"
                   )

# The next step is to identify all the special case customers:

spec_cust = unique(tab_log %>%
                     filter(., NewBalance > 500) %>%
                     select(., CustomerID)
                   )

# Next, I'll filter all the rows corresponding to the special_customers
# out of tab_log to create a new data frame with only analysis-appropriate
# data:

tab_reg = tab_log %>%
          filter(., !(CustomerID %in% spec_cust$CustomerID)
                 )

# tab_reg has 21,767 rows, a decrease of about 17% from the original data.

# Next, I'll create a dataframe with the ID Column (an SQL artifact) gone,
# and with properly formatted date and time, since R looks at WhenLogged
# in tab_reg as a factor:

# Since mutate doesn't work on "POSIXlt" and "POSIXct" classes, I'll create
# a new data frame with the proper formatting directly from tab_reg:

DateTime = strptime(tab_reg$WhenLogged,
                    format = "%Y-%m-%d %H:%M:%S",
                    tz = "CST6CDT"
                    )

tab_df = data.frame(DateTime = DateTime,
                    CustomerID = tab_reg$CustomerID,
                    OldBalance = tab_reg$OldBalance,
                    NewBalance = tab_reg$NewBalance
                    )
#______________________________________________________________________________
# Scatter and step plots of individual customers:

# So that I draw all of the plots on the same axes and make comparison easier,
# I should know the limits for each dimension.

# x: [2009-11-21 14:19:48, 2012-09-28 19:33:20]
# y: [-344.31, 498.50]

# The CustomerIDs I'm picking for this exercise are:
# 5 (low amplitude), 27 (credit), 51 (high amplitude), 89 (outstanding tab).
# I'll graph each independently:

# low amplitude
cust = 5
x_data_point = tab_df$DateTime[tab_df$CustomerID == cust]
y_data_point = tab_df$NewBalance[tab_df$CustomerID == cust]
x_data_step = c(min(tab_df$DateTime),
           tab_df$DateTime[tab_df$CustomerID == cust],
           max(tab_df$DateTime)
           )
y_data_step = c(tab_df$OldBalance[arrange(tab_df, DateTime)$CustomerID == cust][1],
           tab_df$NewBalance[tab_df$CustomerID == cust],
           tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust]
           [length(tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust])])
ggplot() +
geom_point(mapping = aes(x = x_data_point,
                         y = y_data_point
                         ),
           size = 1.25,
           alpha = 0.5) + 
geom_step(mapping = aes(x = x_data_step, y = y_data_step)) +
theme_bw() +
labs(x = "Date", y = "Customer Tab ($)") +
ggtitle("Low-Amplitude Customer Tab vs. Time") +
coord_cartesian(xlim = c(min(tab_df$DateTime), max(tab_df$DateTime)),
                ylim = c(min(tab_df$NewBalance), max(tab_df$NewBalance))
)

# credit
cust = 27
x_data_point = tab_df$DateTime[tab_df$CustomerID == cust]
y_data_point = tab_df$NewBalance[tab_df$CustomerID == cust]
x_data_step = c(min(tab_df$DateTime),
                tab_df$DateTime[tab_df$CustomerID == cust],
                max(tab_df$DateTime)
)
y_data_step = c(tab_df$OldBalance[arrange(tab_df, DateTime)$CustomerID == cust][1],
                tab_df$NewBalance[tab_df$CustomerID == cust],
                tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust]
                [length(tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust])])
ggplot() +
  geom_point(mapping = aes(x = x_data_point,
                           y = y_data_point
  ),
  size = 1.25,
  alpha = 0.5) + 
  geom_step(mapping = aes(x = x_data_step, y = y_data_step)) +
  theme_bw() +
  labs(x = "Date", y = "Customer Tab ($)") +
  ggtitle("Customer Tab with Credit vs. Time") +
  coord_cartesian(xlim = c(min(tab_df$DateTime), max(tab_df$DateTime)),
                  ylim = c(min(tab_df$NewBalance), max(tab_df$NewBalance))
  )

# high amplitude
cust = 51
x_data_point = tab_df$DateTime[tab_df$CustomerID == cust]
y_data_point = tab_df$NewBalance[tab_df$CustomerID == cust]
x_data_step = c(min(tab_df$DateTime),
                tab_df$DateTime[tab_df$CustomerID == cust],
                max(tab_df$DateTime)
)
y_data_step = c(tab_df$OldBalance[arrange(tab_df, DateTime)$CustomerID == cust][1],
                tab_df$NewBalance[tab_df$CustomerID == cust],
                tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust]
                [length(tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust])])
ggplot() +
  geom_point(mapping = aes(x = x_data_point,
                           y = y_data_point
  ),
  size = 1.25,
  alpha = 0.5) + 
  geom_step(mapping = aes(x = x_data_step, y = y_data_step)) +
  theme_bw() +
  labs(x = "Date", y = "Customer Tab ($)") +
  ggtitle("High-Amplitude Customer Tab vs. Time") +
  coord_cartesian(xlim = c(min(tab_df$DateTime), max(tab_df$DateTime)),
                  ylim = c(min(tab_df$NewBalance), max(tab_df$NewBalance))
  )

# oustanding debt
cust = 89
x_data_point = tab_df$DateTime[tab_df$CustomerID == cust]
y_data_point = tab_df$NewBalance[tab_df$CustomerID == cust]
x_data_step = c(min(tab_df$DateTime),
                tab_df$DateTime[tab_df$CustomerID == cust],
                max(tab_df$DateTime)
)
y_data_step = c(tab_df$OldBalance[arrange(tab_df, DateTime)$CustomerID == cust][1],
                tab_df$NewBalance[tab_df$CustomerID == cust],
                tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust]
                [length(tab_df$NewBalance[arrange(tab_df, DateTime)$CustomerID == cust])])
ggplot() +
  geom_point(mapping = aes(x = x_data_point,
                           y = y_data_point
  ),
  size = 1.25,
  alpha = 0.5) + 
  geom_step(mapping = aes(x = x_data_step, y = y_data_step)) +
  theme_bw() +
  labs(x = "Date", y = "Customer Tab ($)") +
  ggtitle("Outstanding Debt Customer Tab vs. Time") +
  coord_cartesian(xlim = c(min(tab_df$DateTime), max(tab_df$DateTime)),
                  ylim = c(min(tab_df$NewBalance), max(tab_df$NewBalance))
  )

#______________________________________________________________________________
# Open Produce Total Tab over time

# Next, what I'm interested in is generating the cumulative tab of all Open
# Produce customers over time. My plan will be to first get the original
# tab as a single value, and then update it with each additional row of 

# So, what's the tab when the curtain opens on our play at 2009-11-21 14:19:48?
# Well, in order to get that, I'll group tab_df by customer, and identify
# all the customers whose earliest OldBalance entries are non-zero: that means
# their first recorded tab transaction wasn't there first transaction.

previous_tabs = tab_df %>%
                group_by(., CustomerID) %>%
                # isolate the first tab transaction for each customer
                filter(., DateTime == min(DateTime)) %>%
                # which of these first transactions had a previous balance?
                filter(., OldBalance != 0)

# Great. Now, the initial net tab just before the first tab transaction at
# 2009-11-21 14:19:48 is the sum of the OldBalance in previous_tabs:

orig_tab = sum(previous_tabs$OldBalance)

# OK, now I'll construct a new vector, the length of tab_df. Each entry will be
# Open Produce's total tab after the transaction in the same index of tab_df.

run_tab = rep(NA, nrow(tab_df))

# I'll create a new data frame that orders all tab transactions
# chronologically:

tab_chrono = tab_df %>% arrange(., DateTime)

# Next, I'll mutate tab_chrono to include a TabChange column:

tab_chrono = tab_chrono %>%
             mutate(., TabChange = NewBalance - OldBalance)

# The first element of run_tab will be orig_tab plus
# TabChange from the first row of tab_chrono:

run_tab[1] = orig_tab + tab_chrono[1, "TabChange"]

# I will fill the rest of run_tab with a for-loop:
for (i in 2:length(run_tab)) {
  run_tab[i] = run_tab[i - 1] + tab_chrono[i, "TabChange"]
}

# Now, I'll mutate tab_chrono again to add run_tab as a new column TotalTab:

tab_chrono = tab_chrono %>% mutate(., TotalTab = run_tab)

#______________________________________________________________________________
# TotalTab vs. DateTime plot

ggplot() +
  geom_step(data = tab_chrono,
            mapping = aes(x = DateTime,
                          y = TotalTab)
            ) +
  theme_bw() +
  labs(x = "Date", y = "Total Tab ($)") +
  ggtitle("Total Open Produce Tab vs. Time") +
  coord_cartesian(ylim = c(0,5000))

#______________________________________________________________________________
# Next, I want to determine the period of time (in seconds) for which Open Produce had a
# particular tab.

# First, I'll initialize a vector:

TabTime = NULL

# Next, I'll loop over the tab transactions in tab_chrono to fill TabTime:

for (i in 1:(nrow(tab_chrono) - 1)) {
  TabTime[i] = difftime(tab_chrono$DateTime[i + 1],
                        tab_chrono$DateTime[i],
                        tz = "CST6CDT",
                        units = "secs"
                        )
}

# To fill in the last element:

TabTime[nrow(tab_chrono)] = 0

# Next, I'll add this data to tab_chrono with a mutate function call:

tab_chrono = tab_chrono %>% mutate(., TabTime_s = TabTime)

# Now, I want to find a single, representative value for the tab total over
# the period. I will do this by minimizing the error between a single value
# and the time-weighted tab for Open Produce. A function that determines this
# error is:

tab_error = function(tab_est) {
  stopifnot(class(tab_est) == "numeric")
  total_error = sum(abs(tab_est - tab_chrono$TotalTab) * tab_chrono$TabTime)
  return(total_error)
}

# Now, I will try successive estimates of the best estimate until I'm within $1:

guess = c(0, 5000, 2500)

while (abs(guess[3] - guess[2]) > 0.1) {
  guess_error = sapply(guess, tab_error)
  errors = sapply(guess, tab_error)
  if (abs(errors[3] - errors[2]) < abs(errors[3] - errors[1])) {
    new_guess = median(guess[2:3])
  } else if (abs(errors[3] - errors[2]) > abs(errors[3] - errors[1])) {
    new_guess = median(c(guess[1], guess[3]))
  } else if (abs(errors[3] - errors[2]) == abs(errors[3] - errors[1])) {
    new_guess = guess[3]
  }
  guess[1] = guess[2]
  guess[2] = guess[3]
  guess[3] = new_guess
}

#______________________________________________________________________________

# Next, I'll plot TotalTab vs. DateTime with the time-weighted median line:

ggplot() +
  geom_step(data = tab_chrono,
            mapping = aes(x = DateTime,
                          y = TotalTab)
  ) +
  geom_hline(yintercept = new_guess, color = "red") +
  theme_bw() +
  labs(x = "Date", y = "Total Tab ($)") +
  ggtitle("Total Open Produce Tab vs. Time") +
  coord_cartesian(ylim = c(0,5000))

#______________________________________________________________________________