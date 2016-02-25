#install.packages('tm')
#install.packages('wordcloud')
#install.packages('memoise')

library(dplyr)
library(tm)
library(wordcloud)
library(memoise)

# load data
articles = read.csv("./data/articles.csv", na.strings = "NA", stringsAsFactors = FALSE)

articles.reduced = select(articles, -content, -title, -topics, -link)

# concat all topics into one string
all.topics =paste(articles$topics, collapse = ', ')

# split topics into list and load into data frame
topic.df = as.data.frame(strsplit(all.topics, ', '), stringsAsFactors = FALSE)
colnames(topic.df) = c('topic')

# creaet a data frame for topics and corresponding counts
by_topic = group_by(topic.df,topic)
topic.counts = summarise(by_topic, total = n())
topic.counts = head(arrange(topic.counts, desc(total)), 20)

# creaet a data frame for authors and counts
by_author = group_by(articles.reduced, author)
top.authors = arrange(summarise(by_author, count = n()), desc(count))
top.authors = head(top.authors, 10)
articles.top.authors = filter(articles.reduced, author %in% top.authors$author)


#arrange(summarise(by_author, count = n()), desc(count))

# create a data frame based on the scaled variables (except 'shares')
articles.numeric = select(articles.reduced, -id, -author, -channel, -post_date,-type, -timedelta)
articles.scaled = as.data.frame(scale(select(articles.numeric, -shares))) 
articles.scaled$shares = articles.reduced$shares


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function() {
  text <- all.topics
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})




