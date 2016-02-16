setwd("/Users/binlin/Documents/DataScience/projects/Shiny Project/food_price_app/data/")

#******************* Transpose food price index data ******************
food.price = read.csv("./food/Food_Price_Clean.csv", na.strings = "N/A")

food.price.transpose = setNames(data.frame(t(food.price[,-1])), food.price[,1])

food.price.transpose$year  = as.integer(substr(rownames(food.price.transpose), 2, 5))

write.csv(food.price.transpose, "./food/Food_Price_Transpose.csv")


#******************* Transpose producer price index data ******************
producer.price = read.csv("producer/producer_price.csv", na.strings = "N/A")

producer.price.transpose = setNames(data.frame(t(producer.price[,c(-1, -2)])), producer.price[,2])

producer.price.transpose$year  = substr(rownames(producer.price.transpose), 2, 5)

write.csv(producer.price.transpose, "producer/Producer_Price_Transpose.csv")
