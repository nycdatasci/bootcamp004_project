###############################################################################
# [ShinyApp: Project 3 - Indeed Unsupervised Data Preparation]
# by Sung Pil Moon
###############################################################################
# cat('\014')
# setwd("~/Documents/Dropbox/DataScience/Projects/Project 3 - WebScrape")
# Sys.setlocale('LC_ALL','C') 
# save.image(file="proj3_SungMoon_PrepareData.RData")

#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud")

load('proj3_SungMoon_PrepareData.RData')
library(dplyr)

indeed_companies <- read.csv('indeed_companies.csv')
indeed_companies_complete <- indeed_companies %>% filter(!is.na(overall_rating)) 
indeed_companies_with_ratings <- indeed_companies_complete[c('overall_rating', 'wl_bal_rating', 'benefit_rating', 'culture_rating', 'jsecurity_rating', 'mgmt_rating')]
indeed_salaries <- read.csv('indeed_salaries.csv')
#write.csv(indeed_companies_complete, file = "Indeed_comp_complete_for_review.csv",row.names=FALSE, na="")
indeed_review <- read.csv('indeed_reviews.csv')
  



##########################################################
# My Testing..
###########################################################
#colnames(indeed_companies_complete)
#View(indeed_companies)
#View(indeed_companies_complete)
#View(indeed_companies_with_ratings)
#View(indeed_salaries)
#View(indeed_review)

# mean(indeed_companies_with_ratings$overall_rating)
# mean(indeed_companies_with_ratings$wl_bal_rating)
# mean(indeed_companies_with_ratings$benefit_rating)
# mean(indeed_companies_with_ratings$culture_rating)
# mean(indeed_companies_with_ratings$mgmt_rating)
# mean(indeed_companies_with_ratings$jsecurity_rating) # 3
# mean(indeed_companies_complete$jsecurity_rating) # 3
# mean(indeed_companies$jsecurity_rating) # Na

# Check NAs
#library(mice)
#md.pattern(indeed_companies_with_ratings)

# first <- c("Fear", "Frontier", "Nanny", "Job", "Yard", "Airport", "Half Pint", "Commando", "Fast Food", "Basketball", "Bachelorette", "Diva", "Baggage", "College", "Octane", "Clean", "Sister", "Army", "Drama", "Backyard", "Pirate", "Shark", "Project", "Model", "Survival", "Justice", "Mom", "New York", "Jersey", "Ax", "Warrior", "Ancient", "Pawn", "Throttle", "The Great American", "Knight", "American", "Outback", "Celebrity", "Air", "Restaurant", "Bachelor", "Family", "Royal", "Surf", "Ulitmate", "Date", "Operation", "Fish Tank", "Logging", "Hollywood", "Amateur", "Craft", "Mystery", "Intervention", "Dog", "Human", "Rock", "Ice Road", "Shipping", "Modern", "Crocodile", "Farm", "Amish", "Single", "Tool", "Boot Camp", "Pioneer", "Kid", "Action", "Bounty", "Paradise", "Mega", "Love", "Style", "Teen", "Pop", "Wedding", "An American", "Treasure", "Myth", "Empire", "Motorway", "Room", "Casino", "Comedy", "Undercover", "Millionaire", "Chopper", "Space", "Cajun", "Hot Rod", "The", "Colonial", "Dance", "Flying", "Sorority", "Mountain", "Auction", "Extreme", "Whale", "Storage", "Cake", "Turf", "UFO", "The Real", "Wild", "Duck", "Queer", "Voice", "Fame", "Music", "Rock Star", "BBQ", "Spouse", "Wife", "Road", "Star", "Renovation", "Comic", "Chef", "Band", "House", "Sweet")
# second <- c("Hunters", "Hoarders", "Contest", "Party", "Stars", "Truckers", "Camp", "Dance Crew", "Casting Call", "Inventor", "Search", "Pitmasters", "Blitz", "Marvels", "Wedding", "Crew", "Men", "Project", "Intervention", "Celebrities", "Treasure", "Master", "Days", "Wishes", "Sweets", "Haul", "Hour", "Mania", "Warrior", "Wrangler", "Restoration", "Factor", "Hot Rod", "of Love", "Inventors", "Kitchen", "Casino", "Queens", "Academy", "Superhero", "Battles", "Behavior", "Rules", "Justice", "Date", "Discoveries", "Club", "Brother", "Showdown", "Disasters", "Attack", "Contender", "People", "Raiders", "Story", "Patrol", "House", "Gypsies", "Challenge", "School", "Aliens", "Towers", "Brawlers", "Garage", "Whisperer", "Supermodel", "Boss", "Secrets", "Apprentice", "Icon", "House Party", "Pickers", "Crashers", "Nation", "Files", "Office", "Wars", "Rescue", "VIP", "Fighter", "Job", "Experiment", "Girls", "Quest", "Eats", "Moms", "Idol", "Consignment", "Life", "Dynasty", "Diners", "Chef", "Makeover", "Ninja", "Show", "Ladies", "Dancing", "Greenlight", "Mates", "Wives", "Jail", "Model", "Ship", "Family", "Videos", "Repo", "Rivals", "Room", "Dad", "Star", "Exes", "Island", "Next Door", "Missions", "Kings", "Loser", "Shore", "Assistant", "Comedians", "Rooms", "Boys")
# first <- sample(first, 3, replace = F)
# second <- sample(second, 5, replace = FALSE)
# first
# second
# 
# sample(first, 2, replace = FALSE)
# sample(second, 4, replace = FALSE)
# sample(first, 3, replace = FALSE)
# sample(second, 5, replace = FALSE)

