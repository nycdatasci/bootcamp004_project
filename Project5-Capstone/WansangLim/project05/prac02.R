library(RTextTools)
library(e1071)

# pos_tweets =  rbind(
#   c('I love this car', 'positive'),
#   c('This view is amazing', 'positive'),
#   c('I feel great this morning', 'positive'),
#   c('I am so excited about the concert', 'positive'),
#   c('He is my best friend', 'positive')
# )
# 
# neg_tweets = rbind(
#   c('I do not like this car', 'negative'),
#   c('This view is horrible', 'negative'),
#   c('I feel tired this morning', 'negative'),
#   c('I am not looking forward to the concert', 'negative'),
#   c('He is my enemy', 'negative')
# )
# 
test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

#####################################
#tweets = rbind(pos_tweets, neg_tweets, test_tweets)

tweetData500 <- read.csv("/media/ubun10/64GB1/NYC/project05/project05/Data/tweetData500a.csv", comment.char="#", stringsAsFactors=FALSE)
#tweetData500 <- read.csv("/media/ubun10/64GB/NYC/project05/project05/Data/tweetDataBrief.csv", comment.char="#", stringsAsFactors=FALSE)

tweets <- tweetData500
tweets <- tweets[1:21,]



# tweets <-c()
# ran.samples = sample(1:nrow(tweets), nrow(tweets))
# for (i in 1: nrow(tweetData500)) {
#   tweets <- rbind(
#     tweets,
#     c(tweetData500$x[ran.samples[i]], tweetData500$classifi[ran.samples[i]])
#   )
# }

tweets01 <-c()
ran.samples = sample(1:nrow(tweets), nrow(tweets))
for (i in 1: nrow(tweets)) {
  tweets01 <- rbind(
    tweets01,
    c(tweets$x[i], tweets$classifi[i])
  )
}

tweets <- tweets01
#Then build the document-term matrix:
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 

# train the model
mat = as.matrix(matrix)
aa <- c(1:10)
bb <- c(11:15)


#all.samples <- sample(1:nrow(tweets), nrow(tweets))
# num.all = nrow(tweets)
# train.index = all.samples[1:(num.all*0.8)]
# test.index = all.samples -train.index

#classifier = naiveBayes(mat[aa,], tweets[aa,2] )
classifier = naiveBayes(mat[aa,], as.factor(tweets[aa,2]) )
# test the validity
predicted = predict(classifier, mat[bb,],threshold = 0.05,); predicted
table(tweets[bb, 2], predicted)
recall_accuracy(tweets[bb, 2], predicted)

####

tweets01 <-c()

for (i in 1: length(tweets)) {
  tweets01 <- rbind(
    tweets01,
    c(tweets[i], "positive")
  )
}

