library(twitteR)
library(RCurl)
library(plyr)
library(ROAuth)
library(httr)
library(stringr)

consumer_key <- "O4sU0x3BtyfZoH1cjWU8HrM2G"
consumer_secret <- "oO6qmxSvKXSGqG0NtkHY9IDUKVfaxM45rI9U0z776HbtDBJiTw"
access_token <- '2942238205-sGK6enBX04AdEk135Lgz4AHeABZF1CzbsvxhVxu'
access_secret <- 'a2dlw8hda537G9PLTrHWdblDPHpsTFvPZnoc0Wnb2vWaK'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#tweets = searchTwitter("hillary", n=10, lang="en")
tweets = searchTwitter("hillary", n=30, lang="en",  geocode='40.78306,-73.971249,100mi')

tweets.text = laply(tweets,function(t)t$getText())

pos = scan('/media/ubun10/64GB1/NYC/project05/project05/twitter-sentiment-analysis-master/wordbanks/positive-words.txt', what='character', comment.char=';')
neg = scan('/media/ubun10/64GB1/NYC/project05/project05/twitter-sentiment-analysis-master/wordbanks/negative-words.txt', what='character', comment.char=';')

###############################################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    
    #sentence = gsub('\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    #word.list = str_split(sentence, '\s+')
    word.list = str_split(sentence, ' ')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    #  a TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}
###############################################
targets <- "I am a boy and a girl or an adult"

analysis = score.sentiment(tweets.text, pos, neg)
analysis
