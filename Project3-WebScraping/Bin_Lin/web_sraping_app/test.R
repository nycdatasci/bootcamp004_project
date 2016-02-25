#install.packages("rjson")
#install.packages('jsonlite')
library("jsonlite")
#library("rjson")
library(dplyr)

setwd('/Users/binlin/Documents/DataScience/projects/web_scraping/web_sraping_app/data/')

json_file = "./marshable_article_detail.json"

articles = fromJSON(json_file, simplifyDataFrame = TRUE)

articles.reduced = select(articles, -content, -title, -topics, -link)

summary(articles.reduced)

write.csv(articles, 'articles.csv')

write.csv(articles.reduced, 'articles_reduced.csv')
