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
# test_tweets = rbind(
#   c('feel happy this morning', 'positive'),
#   c('larry friend', 'positive'),
#   c('not like that man', 'negative'),
#   c('house not great', 'negative'),
#   c('your song annoying', 'negative')
# )
#######################################
pos_tweets =  rbind(
  c("RT @mitchellvii: EXCLUSIVE: RNC Condemns Bill Kristol's Anti-Trump Third Party as 'Helping to Elect Hillary Clinton' - Breitbart https://t.…" , 'positive'),
  c( "RT @JeffDSachs: Hillary got millions for AIPAC speech, from billionaire Haim Saban. More hawkish, more funds. Simple &amp; dangerous. https://t…" , 'positive'),
  c("RT @Biloximeemaw: Al Sharpton calls 4 killing of Cops. One of Obama's advisors. Is this what the @GOP wants Hillary will follow suit. https…"   , 'positive'),
  c("\"Progressive\" Hillary supporters where you at? Hillary moves to Trump's right on Israel: https://t.co/VlMJp1J4hU #FeelTheBern #FreePalestine"  , 'positive'),
  c("I swear when you get done with one assignment there's 50 more due \xed\xa0\xbd\xed\xb8\xa9\xed\xa0\xbd\xed\xb8\xa9\xed\xa0\xbd\xed\xb8\xa9" , 'positive')
)

neg_tweets = rbind(
  c("RT @krikaworks: Two faced as ever Hillary just lies and seems privileged to do so.\nhttps://t.co/0iPSTilJnj", 'negative'),
  c("RT @chanelpuke: Who would you rather have as the President?\nRT for this Rock or like for Hillary https://t.co/wpEnyohvdN" , 'negative'),
  c("RT @yasminefarhan: Vote for Hillary if you think it's ok to shoot a 12 year old in the eyeball bc he threw a rock  https://t.co/AHcMPscefi"  , 'negative'),
  c("RT @EdgeofSports: And yes, Hillary Clinton today - by association - called the legendary Desmond Tutu anti-semitic. #BDS https://t.co/y6q08…"  , 'negative'),
  c("Trying to come up with best line of attack against Hillary. On one hand she represents a party with no new ideas - so she's \"stale.\""  , 'negative')
)

test_tweets = rbind(
  c("RT @wikileaks: Hillary Clinton's #Libya \"Tick Tock\" details how she 'led' the destruction  https://t.co/FuGMXhTLjp #imwithher https://t.co/…", 'positive'),
  c("RT @femalenomads: Who would you rather have as the President?\nRT for this Rock or like for Hillary https://t.co/ptbJqLW1KR", 'positive'),
  c("@AG_Conservative No way #Drumpf would prosecute his buddy Hillary.  His sister w/b nominated SCOTUS.  But he can't beat her.  Ain't happenin" , 'negative'),
  c("RT @SgBz: Hillary Clinton has been exonerated on every one of the charges Republicans have leveled at her. https://t.co/NPGZfGTZ98 #p2 #tcot" , 'negative'),
  c("RT @realDonaldTrump: \"@TheAme19: @realDonaldTrump @RedNationRising And the answer ISN'T Hillary or Pocahontas Warren for $300...\"" , 'negative')
)

#####################################
tweets = rbind(pos_tweets, neg_tweets, test_tweets)

#Then we can build the document-term matrix:
  
  # build dtm
  matrix= create_matrix(tweets[,1], language="english", 
                        removeStopwords=FALSE, removeNumbers=TRUE, 
                        stemWords=FALSE) 

  # train the model
  mat = as.matrix(matrix)
  classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )
  
  # test the validity
  predicted = predict(classifier, mat[11:15,]); predicted
  table(tweets[11:15, 2], predicted)
  recall_accuracy(tweets[11:15, 2], predicted)
  
  ####Classsi input start
  
  # write.csv(tweets.text, file = "/media/ubun10/64GB/NYC/project05/project05/Data/tweetData.csv")
  tweetData <- read.csv("/media/ubun10/64GB/NYC/project05/project05/Data/tweetData.csv", comment.char="#", stringsAsFactors=FALSE)
  tweetData$classifi <- ""
  