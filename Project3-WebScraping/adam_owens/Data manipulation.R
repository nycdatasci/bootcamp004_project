library(dygraphs)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(dummies)
library(googleVis)

df = read.csv('~/data/tripadvisors2.csv', header = T)
View(df)
summary(df)
names(df)
head(df)

df= df[ ! duplicated(df[c("Hotel_Name")]),] #remove duplicated hotel names if those are pulled

# a quick look at each data frame to see what we're dealing with
df_chic = filter(df, City == 'Chicago')
df_ny = filter(df, City == 'New York City')
df_slc = filter(df, City == 'Salt Lake City')
df_la = filter(df, City == 'Los Angeles')
  
replace(df$Star_Rating, "NA", 0) #replace NA with 0

########### Bar chart with feature ratings by NYC neigborhood
df_bar = group_by(df, Hotel_Features) %>%
  summarize(., Hotel_Count = n(), Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T)) %>%
  arrange(., desc(Mean_Ratings)) %>%
  filter(., Hotel_Count >=5) %>%
  select(., Hotel_Features, Mean_Ratings)

df_bar$Hotel_Features = gsub("Hell&#39;s Kitchen", "Hell's Kitchen", df_bar$Hotel_Features)

Bar1 <- gvisBarChart(df_bar, options = 
                       list(colors="['red','#004411']", width = 600, height=500))
plot(Bar1)

########## Bubble chart comparing cityrank and Hotel rating
p <- ggplot(df, aes(x = Star_Rating,y = City_Ranking))+ 
  geom_point(aes(size = Review_Count, colour=City)) +
  scale_size(range = c(1,15))
  
p + xlab("Hotel Star Ratings") + ylab("City Ranking")

df_ratings = group_by(df, City, Hotel_Features) %>%
  summarize(., count= n(), Mean_Ratings = mean(Star_Rating, na.rm=T), Mean_CityRank= mean(City_Ranking, na.rm=T), Total_Reviews = sum(Review_Count, na.rm = T))%>%
  filter(., Hotel_Features %in% c("Luxury","Mid-Range","Budget","Free Parking","Free Breakfast","Pets Allowed", "Pool","Green")) %>%
  arrange(., desc(City), desc(Mean_Ratings))

########## Bubble chart comparing average review ratings and hotel features
p1 <- ggplot(df_ratings, aes(x = Hotel_Features,y = Mean_Ratings))+ 
  geom_point(aes(size = count, colour=City)) +
  scale_size(range = c(1,30))

p1 + xlab("Hotel_Features") + ylab("Mean_Ratings")
