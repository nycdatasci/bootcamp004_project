library(dplyr)

food.price = read.csv("./data/food/Food_Price_Transpose.csv", na.strings = "NA")
food.price = food.price[, c(length(food.price), 2:(length(food.price)-1))]

producer.price = read.csv("./data/producer/Producer_Price_Transpose.csv", na.strings = "NA")
producer.price = producer.price[, c(length(producer.price), 2:(length(producer.price)-1))]

all.price = read.csv("./data/consumer/cpi_all_1973_2013.csv", na.strings = "NA")

food_producer_join = inner_join(select(food.price, year, All.food), producer.price, by = "year")

food.and.all = select(inner_join(food.price, rename(all.price, All.item = percent.change), by = "year"), year, All.food, All.item)

food.category.cor = select(food.price, -year, -All.food, -Food.at.home)

category_percent = read.csv("./data/category_percent.csv")

category_percent = filter(category_percent, !(category %in% c('All.food', 'Food.at.home', 'Meats')))

cor_zoom_width = 600

cor_zoom_height = 400