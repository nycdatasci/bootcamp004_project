library(dygraphs)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(dummies)
library(googleVis)
df = read.csv('/Users/adamowens/data/tripadvisors2.csv', header = T)

summary(df)
names(df)
head(df)

df= df[ ! duplicated(df[c("Hotel_Name")]),]

df_chic = filter(df, City == 'Chicago')
df_ny = filter(df, City == 'New York City')
df_slc = filter(df, City == 'Salt Lake City')
df_la = filter(df, City == 'Los Angeles')

replace(df$Star_Rating, "NA", 0)

df_ratings = group_by(df, City, Hotel_Features) %>%
  summarize(., count= n(), Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T))%>%
  filter(., Hotel_Features %in% c("Luxury","Mid-Range","Budget","Free Parking","Free Breakfast","Pets Allowed", "Pool","Green")) %>%
  arrange(., desc(City), desc(Mean_Ratings))

df_bubble = group_by(df, City, Hotel_Features) %>%
  summarize(., count= n(), Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T))%>%
  filter(., Hotel_Features %in% c("Luxury","Budget","Pets Allowed")) %>%
  arrange(., desc(City), desc(Mean_Ratings))

Bubble <- gvisBubbleChart(df_bubble, colorvar = "City",
                          xvar="mean_rank", yvar="mean_ratings",
                          idvar ="Hotel_Features", sizevar="Total_Reviews")
plot(Bubble)

df_line = group_by(df, City) %>%
  summarize(., Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T))

Line4 <-  gvisLineChart(df_line, "City", c("Mean_Ratings","Mean_CityRank"),
                        options=list(gvis.editor="Overview of Ratings by City"))
plot(Line4)



df_bar = group_by(df, Hotel_Features) %>%
  summarize(., Hotel_Count = n(), Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T)) %>%
  arrange(., desc(Mean_Ratings)) %>%
  filter(., Hotel_Count >=5) %>%
  select(., Hotel_Features, Mean_Ratings)

df_bar$Hotel_Features = gsub("Hell&#39;s Kitchen", "Hell's Kitchen", df_bar$Hotel_Features)


Bar1 <- gvisBarChart(df_bar, options = list(colors="['red','#004411']", width = 600, height=700))
plot(Bar1)

df_bar = select(df_line, City, Total_Reviews) %>%
  arrange(., desc(Total_Reviews))

Bar2 <- gvisBarChart(df_bar)
plot(Bar2)

names(df)
df_features = group_by(df, Hotel_Features) %>%
  summarize(., Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T)) %>%
  filter(., Hotel_Features %in% c("Luxury","Mid-Range","Budget","Free Parking","Free Breakfast","Pets Allowed", "Pool","Green"))

df_features1 = select(df_features, Hotel_Features, Mean_Ratings, Mean_CityRank)
Bar2 <- gvisBarChart(df_features1)
plot(Bar2)

ggplot(df, Star_Rating, City_Ranking)

p <- ggplot(df, aes(x = Star_Rating,y = City_Ranking))+ 
  geom_point(aes(size = Review_Count, colour=City)) +
  scale_size(range = c(1,15))
  
p + xlab("Hotel Star Ratings") + ylab("City Ranking")

p1 <- ggplot(df_ratings, aes(x = Hotel_Features,y = Mean_Ratings))+ 
  geom_point(aes(size = count, colour=City)) +
  scale_size(range = c(1,30)) +
  
  geom_text(size=3)

p1 + xlab("Hotel Features") + ylab("Ratings")




###########################Regression###############################

dygraph(df_ratings, main = "City Rank and Star Rating") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


