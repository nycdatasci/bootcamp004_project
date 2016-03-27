library(shiny)
library(RCurl)
library(plyr)
library(ROAuth)
library(httr)
library(jsonlite)
library(RTextTools)
library(e1071)
library(stringr)

shinyServer(function(input, output) {
  twittSearch <- reactive(input$twitSearch)
  twittLocation <- reactive(input$twitLocation)
  locationRange <- reactive(input$locationRange)
  NumberTwitts <- reactive(input$NumberTwitts)
  
  tweetData <- read.csv("/media/ubun10/64GB1/NYC/project05/project05/Data/tweetData500a.csv", comment.char="#", stringsAsFactors=FALSE)
  
  output$twitSearch <- renderDataTable({
    input$goButton
    isolate({
      twittSearch <- twittSearch()
      twittLocation <- twittLocation()
      locationRange <- locationRange()
      NumberTwitts <- NumberTwitts()
      
      #GeoCode API
      geoWeb <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=", twittLocation, "&key=AIzaSyANifkybPlJWYynG_FSwzwSn-CunJTE4N0", sep = "")
      geoAPI <- fromJSON(geoWeb)
      latt <- geoAPI$results$geometry$location$lat
      lont <- geoAPI$results$geometry$location$lng
      
      if (twittSearch == "") {
        outPut01 <- "Please, input search"
      } else {
          #Twit setting
          consumer_key <- "O4sU0x3BtyfZoH1cjWU8HrM2G"
          consumer_secret <- "oO6qmxSvKXSGqG0NtkHY9IDUKVfaxM45rI9U0z776HbtDBJiTw"
          access_token <- '2942238205-sGK6enBX04AdEk135Lgz4AHeABZF1CzbsvxhVxu'
          access_secret <- 'a2dlw8hda537G9PLTrHWdblDPHpsTFvPZnoc0Wnb2vWaK'
          
          coordinate <- paste(latt,",", lont,",", locationRange,'mi',sep = "")
          print(coordinate)
          setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

          #tweets = searchTwitter(twittSearch, n=10, lang="en",  geocode='39.0997265,-94.5785667,100mi')
          tweets = searchTwitter(twittSearch, n= NumberTwitts, lang="en",  geocode=coordinate)
          # End of Twit setting
          
          Tweets.text <<- laply(tweets,function(t)t$getText())
          ############################################################## 
          tweetLen <- length(Tweets.text)
          X <- c(1: tweetLen)
          x <- c(1: tweetLen)
          classifi <- c(1: tweetLen)
          
          newTestSet <- data.frame(X, x, classifi)
          # 
          for (k in  1 : length(Tweets.text)) {
            newTestSet$X[k] <- k
            newTestSet$x[k] <- Tweets.text[k]
            newTestSet$classifi[k] <- "positive"
          }

           bindedData <- rbind(tweetData, newTestSet)
           tweets <- bindedData
           
           #####Convert to Matrix
           tweets01 <-c()
           #ran.samples = sample(1:nrow(tweets), nrow(tweets))
           for (i in 1: nrow(tweets)) {
             tweets01 <- rbind(
               tweets01,
               c(tweets$x[i], tweets$classifi[i])
             )
           }
           
           tweets <- tweets01
           # train the model
           #Then build the document-term matrix:
           matrix= create_matrix(tweets[,1], language="english", 
                                 removeStopwords=FALSE, removeNumbers=TRUE, 
                                 stemWords=FALSE) 
           
           mat = as.matrix(matrix)
           
           endTrain <- nrow(tweetData)
           print(endTrain)
           print(tweetLen)
           train.index <- c(1:endTrain)
           test.index <- c(499:510)

           classifier = naiveBayes(mat[train.index,], as.factor(tweets[train.index,2]) )
           print(train.index)
           print(test.index)
           # test the validity
           predicted = predict(classifier, mat[test.index,],threshold = 0.05,); predicted
          pridicTable <-  table(tweets[test.index, 2], predicted)
           accu <-  recall_accuracy(tweets[test.index, 2], predicted)
            print(pridicTable)
          ###################################################################
          outPut01 <- pridicTable
      }
          outPut01

    })#End of isolate
  })#End of twitSearch
  
 
  output$classify <- renderPrint({
    input$goButton
    number <- input$obs
    
    tweet <- tweetData[number,]
    tweetData$classifi[number] <- input$Select
    
    tweetData[number,]
  })
  
  output$senti <- renderPrint({
    tweets.text <- Tweets.text
    input$goButton
    
    pos = scan('/media/ubun10/64GB1/NYC/project05/project05/twitter-sentiment-analysis-master/wordbanks/positive-words.txt', what='character', comment.char=';')
    neg = scan('/media/ubun10/64GB1/NYC/project05/project05/twitter-sentiment-analysis-master/wordbanks/negative-words.txt', what='character', comment.char=';')
    
    ###############################################
    score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
      
    {
      
      require(plyr)
      require(stringr)

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
    
    analysis <- score.sentiment(tweets.text, pos, neg)
    analysis$score
  })# end of semtiment
})#End of Function, shinyServer
