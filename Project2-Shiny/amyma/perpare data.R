require(Hmisc)
require(zoo)
# data
setwd("~/Documents/R /Data Bootcamp/project2/Airbnb data")
review<-read.csv("reviews.csv")
review$date<-as.Date(review$date)
review$Date<-as.factor(as.yearmon(as.Date(review$date),"%m %y"))
#review$Month<-factor(months(as.Date(review$date)),levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
review$Weekday <- weekdays(as.Date(review$date))
review$Weekday <- factor(review$Weekday, levels=c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday"))
review$Year <- format(review$date, "%y")
review$Month<-format(review$date, "%m")
# text mining--word cloud for review
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# read file
text <- readLines("reviews 2.txt")
# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)

m <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)

m <- data.frame(as.matrix(m))
v<-m[order(-m$X1), , drop = FALSE]
d <- data.frame(word = rownames(v),X1=v)
d[2,1]<-"apartment"
review_txt<-d%>%
  filter(X1<=max(X1),
         X1>=1500)
review_txt<-as.data.frame(review_txt)
# head(d, 10)
# set.seed(123)
# jpeg('wordcloud.jpg')
# wordcloud(words = d$word, freq = d$X1, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))
# dev.off()



listing<-read.csv("listings 2.csv")
listing$review_scores_rating<-impute(listing$review_scores_rating,0)
map<-listing[,c(21,38,46:47,50,58,74,77)]
map$price<-as.numeric(map$price)
map$review_scores_rating<-as.numeric(map$review_scores_rating)

host<-listing[,c(17,19,20,21,31,72)]
host<-unique(host)
host<-host%>%
  arrange(desc(host_total_listings_count))
host$host_since<-as.Date(host$host_since)

setwd("~/Documents/R /Data Bootcamp/project2/New ui")
save(review, file = "data/review.RData")
save(review_txt, file = "data/review_txt.RData")
save(map, file = "data/map.RData")
save(host,file="data/host.RData")
