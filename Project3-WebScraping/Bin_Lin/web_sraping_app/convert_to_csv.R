library("jsonlite")
library(dplyr)

setwd('/Users/binlin/Documents/DataScience/projects/web_scraping/web_sraping_app/')

json_file = "data/marshable_article_detail_2016-02-25-023950318.json"

articles = fromJSON(json_file, simplifyDataFrame = TRUE )

row.names(articles)<-NULL

# move id to the front (using stupid way)
articles_with_just_id = select(articles, id)
articles_without_id = select(articles, -id)
articles_merged = cbind(articles_with_just_id, articles_without_id)

articles.reduced = select(articles, -content, -title, -topics, -link)

# save as csv files
write.csv(articles_merged, 'data/articles.csv', row.names=FALSE)
write.csv(articles.reduced, 'data/articles_reduced.csv', row.names=FALSE)


test.rows = sample(1:nrow(articles_merged), 200, replace=TRUE)
articles.test = articles_merged[test.rows,]
articles.training = articles_merged[-test.rows,]

# save test and train set to csv files
write.csv(articles.training, 'data/articles_training.csv', row.names=FALSE)
write.csv(articles.test, 'data/articles_test.csv', row.names=FALSE)
