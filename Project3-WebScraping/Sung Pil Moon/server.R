


server <- function(input, output) { 
  
  updateInputDataForIndeedDataTable <- reactive({  
    dataFiltered <- indeed_companies
    dataFiltered$job_link <- paste0("<a href='",dataFiltered$job_link,"' target='_blank'>link</a>")  
    dataFiltered$overall_link <- paste0("<a href='",dataFiltered$overall_link,"' target='_blank'>link</a>")  
    
    dataFiltered
  })
  
  #######################################
  # Component 1: Indeed Data Table
  ########################################
  output$myIndeedDataTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable <- updateInputDataForIndeedDataTable() 
    #dataForDTable <- dataForDTable[, c(2:13)]
    dataForDTable <- dataForDTable[, c(3,8,7,6,5,11,12,13,2,4,9,10)]
    colnames(dataForDTable) <- c("COMP_NAME", 
                                 "JOB_TITLE", "JOB_POSTED", "JOB_LOCATION", "JOB_LINK",
                                 "COMP_DESC_LINK",  "RATING_OVERALL",  "RATING_WORK_LIFE_BAL", 
                                 "RATING_CULTURE", "RATING_BENEFITS", "RATING_JOB_SECURITY?", "RATING_MANAGEMENT"
    )
    
    dataForDTable                   
    
  }, 
  rownames = FALSE,
  escape = FALSE,
  class = 'cell-border stripe',
  #extensions = list(FixedColumns = list(leftColumns = 2)), # <-- will cause layout issue
  extensions = c('ColVis','ColReorder','Scroller'),
  options = list(
    
    searching = F,
    pageLength = 10,
    lengthMenu = list(c(10, 5, 15, 20, 25, 50, 100), c('10', '5', '15', '20', '25','50','100')),
    dom = 'RC<"clear">lfrtip',
    scrollX = TRUE,
    #scrollCollapse = TRUE,
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )))
  
  
  #######################################
  # Component 2: Indeed JOB Map
  ########################################
  
  
  ##############################################################################
  # Component 3: Indeed Cluster Analysis
  ##############################################################################
  
  #//////////////////////////////////////////////////////
  # Component 3.1. Scatter Plot for Cluster analysis
  #//////////////////////////////////////////////////////
  updateInputDataForClusterAnalysis <- reactive({
    dataFilteredForClusterAnalysis <- indeed_companies_complete
    #View(indeed_companies_with_ratings) 
    #View(indeed_companies_complete)
    if(input$checkboxForNormalize == TRUE){
      stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
      indeed_ratings.scaled = stdize(indeed_companies_with_ratings, na.rm = T) 
    } else {
      indeed_ratings.scaled  = indeed_companies_with_ratings
    }
    
    dataFilteredForClusterAnalysis <- indeed_ratings.scaled
    dataFilteredForClusterAnalysis
    
  })
  
  output$myClusterAnalysisChart <- renderPlot({
    dataForClusterAnalysis <- updateInputDataForClusterAnalysis()  
    
    if(input$singleSelectizeForClusterAnalysis2 != "" & input$singleSelectizeForClusterAnalysis2 != ""){
      var1 = input$singleSelectizeForClusterAnalysis1
      var2 = input$singleSelectizeForClusterAnalysis2
      
      if(var1 == 'overall_rating'){ var1 = dataForClusterAnalysis$overall_rating } 
      else if(var1 == 'wl_bal_rating'){ var1 = dataForClusterAnalysis$wl_bal_rating } 
      else if(var1 == 'benefit_rating'){ var1 = dataForClusterAnalysis$benefit_rating } 
      else if(var1 == 'culture_rating'){ var1 = dataForClusterAnalysis$culture_rating } 
      else if(var1 == 'jsecurity_rating'){ var1 = dataForClusterAnalysis$jsecurity_rating } 
      else if(var1 == 'mgmt_rating'){ var1 = dataForClusterAnalysis$mgmt_rating }
      
      if(var2 == 'overall_rating'){ var2 = dataForClusterAnalysis$overall_rating } 
      else if(var2 == 'wl_bal_rating'){ var2 = dataForClusterAnalysis$wl_bal_rating } 
      else if(var2 == 'benefit_rating'){ var2 = dataForClusterAnalysis$benefit_rating } 
      else if(var2 == 'culture_rating'){ var2 = dataForClusterAnalysis$culture_rating } 
      else if(var2 == 'jsecurity_rating'){ var2 = dataForClusterAnalysis$jsecurity_rating } 
      else if(var2 == 'mgmt_rating'){ var2 = dataForClusterAnalysis$mgmt_rating }
      
      
      km.indeed1 = kmeans(indeed_ratings.scaled, centers = input$sliderForKmeanRange)
      
      df.cluster <- data.frame(km.indeed1[1]) #indeed_companies_with_ratings indeed_companies_complete
      indeed_companies_with_ratings2 <<- cbind(dataForClusterAnalysis, df.cluster)
      indeed_companies_with_ratings2 <- within(indeed_companies_with_ratings2, { 
        cluster <- as.character(cluster) 
      })
      dataForClusterAnalysis <- indeed_companies_with_ratings2
      # head(indeed_companies_with_ratings2)
      
      # 2nd part to get summary of cluster after choosing K
      indeed_companies_with_ratings_complete2 <<- cbind(indeed_companies_complete, df.cluster)
      indeed_companies_with_ratings_complete2 <- within(indeed_companies_with_ratings_complete2, { 
        cluster <- as.character(cluster) 
      })
      
      # After get the cluster aes(colour=cond2
      qplot(var1, var2, data=dataForClusterAnalysis, alpha=1.0, colour = cluster) + 
        theme(legend.position="none") #+ scale_color_gradient2( low = "darkblue", mid="orange", high = "darkred")
      
    }
    
  })
  

  
  #/////////////////////////////////////////
  # Component 3.2. Scree plot for K-mean
  #////////////////////////////////////////
  output$myScreePlotForKmean <- renderPlot({
    
    dataForScreePlot <- updateInputDataForClusterAnalysis()  
    k = input$sliderForKmeanRange
    
    wssplot = function(data, nc = 15, seed = 0) {
      wss = (nrow(data) - 1) * sum(apply(data, 2, var))
      for (i in 2:nc) {
        set.seed(seed)
        wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
      }
      wss.global <<- wss
      qplot(1:nc, wss, color="darkred", xlab = "Number of Clusters", ylab = "Within-Cluster Variance (WCV)",
            main = "Scree Plot for the K-Means Procedure") + 
        geom_point(aes(colour='orange', size=5)) + geom_line() + theme(legend.position="none") +
        scale_x_continuous(breaks = seq(1, 15, by = 1)) + 
        geom_hline(aes(yintercept=wss[k]), colour="#0000AA", linetype="dashed") + 
        geom_vline(aes(xintercept=k), colour="#BB0000", linetype="dashed")
      
    }
    wssplot(dataForScreePlot) 
    
  })
  
  #//////////////////////////////////
  # Component 3.3. DataTable for WCV value
  #//////////////////////////////////
  # Vertical WCV data table
  output$myWCVdataTable <- DT::renderDataTable(DT::datatable({ 
    #wss_temp <- updateWcvValue()
    wss_temp <- wss.global
    
    dataForWcvDataFrame <- data.frame(num_cluster = c(1:15), WCV = wss_temp)
    dataForWcvDataFrame <- within(dataForWcvDataFrame, { 
      num_cluster <- as.integer(num_cluster) 
    })
    dataForWcvDataFrame
    
  }, 
  rownames = FALSE,
  #caption = 'These values are the Within-Cluster variance (WCV) corresponding to the number of Clusters (K)',
  class = 'cell-border stripe',
  extensions = c('ColVis','ColReorder','Scroller'),
  options = list(
    searching = F,
    #dom = 'RC<"clear">lfrtip',
    dom = 't',
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}"), pageLength = 20))
  )
  
  
  #//////////////////////////////////
  # Component 3.4. Selectize input
  #//////////////////////////////////
  output$renderSelectizeInput <- renderUI({
    if( is.null(input$sliderForKmeanRange )){
      selectizeInput('singleSelectizeKforDisplayDTable', 'Choose K for Cluster Details:', 
                     choices = 1:input$sliderForKmeanRange, selected = 1, multiple = FALSE)
    } else{
      selectizeInput('singleSelectizeKforDisplayDTable', 'Choose K for Cluster Details:', 
                     choices = 1:input$sliderForKmeanRange, selected = input$sliderForKmeanRange,multiple = FALSE
      )  
    }
    
  })
  
  #//////////////////////////////////////////////////
  # Component 3.5. ValueBoxes for Cluster Analysis
  #//////////////////////////////////////////////////
  updateInputDataForIndeedDataTableAfterClusterAnalysis <- reactive({
    dataFilteredAfterClusterAnalysis = indeed_companies_with_ratings_complete2
    
    print("myK1: ", is.null(input$singleSelectizeKforDisplayDTable ))
    cat("myK2: ", input$singleSelectizeKforDisplayDTable == "")
    myK = input$singleSelectizeKforDisplayDTable 

    dataFilteredAfterClusterAnalysis = dataFilteredAfterClusterAnalysis[as.character(dataFilteredAfterClusterAnalysis$cluster) == myK,]
    #View(dataFilteredAfterClusterAnalysis)
    
    clusterAvgRatingOverall <- round(mean(dataFilteredAfterClusterAnalysis$overall_rating), 2)
    clusterAvgRatingWLBalance <- round(mean(dataFilteredAfterClusterAnalysis$wl_bal_rating), 2)
    clusterAvgRatingBenefits <- round(mean(dataFilteredAfterClusterAnalysis$benefit_rating), 2)
    clusterAvgRatingCulture <- round(mean(dataFilteredAfterClusterAnalysis$culture_rating), 2)
    clusterAvgRatingManagement <- round(mean(dataFilteredAfterClusterAnalysis$mgmt_rating), 2)
    clusterAvgRatingJobSecurity <- round(mean(dataFilteredAfterClusterAnalysis$jsecurity_rating), 2)
    
    output$boxForAggOverallRating <- renderValueBox({
      valueBox( value = clusterAvgRatingOverall, subtitle = "Avg.Rating.Overall", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingOverall) "red" else 
                  if(4.0 <= clusterAvgRatingOverall & clusterAvgRatingOverall < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingOverall & clusterAvgRatingOverall < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingOverall & clusterAvgRatingOverall < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingOverall & clusterAvgRatingOverall < 3.0) "blue" else "purple"
      )   
    })
    
    output$boxForAggWLBalanceRating <- renderValueBox({
      valueBox( value = clusterAvgRatingWLBalance, subtitle = "Avg.Rating.Work-Life Balance", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingWLBalance) "red" else 
                  if(4.0 <= clusterAvgRatingWLBalance & clusterAvgRatingWLBalance < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingWLBalance & clusterAvgRatingWLBalance < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingWLBalance & clusterAvgRatingWLBalance < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingWLBalance & clusterAvgRatingWLBalance < 3.0) "blue" else "purple"
      )   
    })
    
    output$boxForAggBenefitsRating <- renderValueBox({
      valueBox( value = clusterAvgRatingBenefits, subtitle = "Avg.Rating.Benefits-Compensation", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingBenefits) "red" else 
                  if(4.0 <= clusterAvgRatingBenefits & clusterAvgRatingBenefits < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingBenefits & clusterAvgRatingBenefits < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingBenefits & clusterAvgRatingBenefits < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingBenefits & clusterAvgRatingBenefits < 3.0) "blue" else "purple"
      )   
    })
    
    output$boxForAggCultureRating <- renderValueBox({
      valueBox( value = clusterAvgRatingCulture, subtitle = "Avg.Rating.Company.Culture", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingCulture) "red" else 
                  if(4.0 <= clusterAvgRatingCulture & clusterAvgRatingCulture < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingCulture & clusterAvgRatingCulture < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingCulture & clusterAvgRatingCulture < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingCulture & clusterAvgRatingCulture < 3.0) "blue" else "purple"
      )   
    })
    
    output$boxForAggJobSecurityRating <- renderValueBox({
      valueBox( value = clusterAvgRatingManagement, subtitle = "Avg.Rating.Management", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingManagement) "red" else 
                  if(4.0 <= clusterAvgRatingManagement & clusterAvgRatingManagement < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingManagement & clusterAvgRatingManagement < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingManagement & clusterAvgRatingManagement < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingManagement & clusterAvgRatingManagement < 3.0) "blue" else "purple"
      )   
    })
    
    output$boxForAggManagementRating <- renderValueBox({
      valueBox( value = clusterAvgRatingJobSecurity, subtitle = "Avg.Rating.Job.Security", icon = icon("star-o"),
                color = if(4.5 <= clusterAvgRatingJobSecurity) "red" else 
                  if(4.0 <= clusterAvgRatingJobSecurity & clusterAvgRatingJobSecurity < 4.5) "orange" else 
                    if(3.5 <= clusterAvgRatingJobSecurity & clusterAvgRatingJobSecurity < 4.0) "olive" else
                      if(3.0 <= clusterAvgRatingJobSecurity & clusterAvgRatingJobSecurity < 3.5) "teal" else
                        if(2.0 <= clusterAvgRatingJobSecurity & clusterAvgRatingJobSecurity < 3.0) "blue" else "purple"
      )   
    })
    
    dataFilteredAfterClusterAnalysis
  })
  
  #renderRadarChart
  output$renderRadarChart <- renderPlot({
    
    radarChartVal <- updateInputDataForIndeedDataTableAfterClusterAnalysis()
    
    valForRadarRatingOverall <- round(mean(radarChartVal$overall_rating), 2)
    valForRadarRatingWLBalance <- round(mean(radarChartVal$wl_bal_rating), 2)
    valForRadarRatingBenefits <- round(mean(radarChartVal$benefit_rating), 2)
    valForRadarRatingCulture <- round(mean(radarChartVal$culture_rating), 2)
    valForRadarRatingManagement <- round(mean(radarChartVal$mgmt_rating), 2)
    valForRadarRatingJobSecurity <- round(mean(radarChartVal$jsecurity_rating), 2)
    
    maxmin <- data.frame(
      Overall=c(5,1),
      WorkLife_Balance=c(5,1),
      Compensation_Benefit=c(5,1),
      Management=c(5,1),
      JobSecurity=c(5,1),
      Culture=c(5,1))
    
    RNGkind("Mersenne-Twister")
    #set.seed(123)
    
    dat <- data.frame(
      Overall= c(4.13, valForRadarRatingOverall),
      WorkLife_Balance=c(4.02, valForRadarRatingWLBalance),
      Compensation_Benefit=c(4.02, valForRadarRatingBenefits),
      Management=c(3.6, valForRadarRatingManagement),
      JobSecurity=c(3.00, valForRadarRatingJobSecurity),
      Culture=c(3.98, valForRadarRatingCulture))
    
    dat <- rbind(maxmin,dat)
    
    radarchart(dat, axistype=2, pty=32, pcol=c("darkblue", "orange"), plty=1, plwd=3, axislabcol="grey", na.itp=FALSE) #,title="Comparison: Overall dataset vs. Selected cluster") 
    
    legend("bottomright", legend = c("Overall Dataset", "Selected Cluster"), col=c("darkblue","orange"), lty=c(1,1))
    
  })
  #   
  #////////////////////////////////////////////////////////////////////////////////
  # Component 3.6. (Filtered) DataTable after choosing k value
  #////////////////////////////////////////////////////////////////////////////////
  output$myFilteredClusterDataTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForClusterDTable <- updateInputDataForIndeedDataTableAfterClusterAnalysis() 
    
    dataForClusterDTable$job_link <- paste0("<a href='",dataForClusterDTable$job_link,"' target='_blank'>link</a>")  
    dataForClusterDTable$overall_link <- paste0("<a href='",dataForClusterDTable$overall_link,"' target='_blank'>link</a>")  
    
    dataForClusterDTable <- dataForClusterDTable[, c(14,8,7,6,5,3,12,13,2,4,9,10,11)]
    colnames(dataForClusterDTable) <- c("CLUSTER", "JOB_TITLE", "JOB_POSTED", "JOB_LOC", "JOB_LINK", "COMP_NAME", 
                                        "OVERALL_RATING", "WORK_LIFE_BALANCE", "BENEFIT_RATING", "CULTURE_RATING", 
                                        "JOB_SCRTY_RATING", "MGMT_RATING", "COMP_DESC_LINK"
    )
    dataForClusterDTable                   
    
  }, 
  rownames = FALSE,
  escape = FALSE,
  class = 'cell-border stripe',
  extensions = c('ColVis','ColReorder','Scroller'),
  options = list(
    
    searching = F,
    #pageLength = 15,
    lengthMenu = list(c(15, 5, 10, 20, 25, 50, 100), c('15', '5', '10', '20', '25','50','100')),
    dom = 'RC<"clear">lfrtip',
    scrollX = TRUE,
    #scrollCollapse = TRUE,
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )))
  
  
  ##############################################################################
  # TabItem 4: Company Review Analysis
  ###############################################################################
  
  #////////////////////////////////////////////////////////////////////////////////
  # Component 4.1: myCompReviewAnalysisDataTable
  #////////////////////////////////////////////////////////////////////////////////
  updateInputDataForReviewTable <- reactive({
    dataFilteredForReview <- indeed_review
    
    dataFilteredForReview <- dataFilteredForReview[(input$sliderForReviewRange[1] <= dataFilteredForReview$comp_rating_overall &
                                                      dataFilteredForReview$comp_rating_overall <= input$sliderForReviewRange[2]),]
    
    dataFilteredForReview
  })
  
  output$myCompReviewAnalysisDataTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForCompReviewDTable <- updateInputDataForReviewTable() 
    dataForCompReviewDTable$comp_review_link <- paste0("<a href='",dataForCompReviewDTable$comp_review_link,"' target='_blank'>link</a>")  
    
    dataForCompReviewDTable <- dataForCompReviewDTable[, c(2,3,4,7,6,5)]
    colnames(dataForCompReviewDTable) <- c("COMPANY_NAME", "COMPANY_RATING", "REVIEW_LINK", "REVIEW_SUMMARY", "REVIEW_PROS", "REVIEW_CONS")
    dataForCompReviewDTable                   
    
  }, 
  rownames = FALSE,
  escape = FALSE,
  class = 'cell-border stripe',
  extensions = c('ColVis','ColReorder','Scroller'),
  options = list(
    
    searching = F,
    lengthMenu = list(c(15, 5, 10, 20, 25, 50, 100), c('15', '5', '10', '20', '25','50','100')),
    dom = 'RC<"clear">lfrtip',
    scrollX = TRUE,
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )))
  
  
  #////////////////////////////////////////////////////////////////////////////////
  # Component 4.2: wordCloud
  #////////////////////////////////////////////////////////////////////////////////
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  wordsListPositive <- reactive({
    
    dataForWordCloud <- updateInputDataForReviewTable()
    
    review_text_pros <- paste(dataForWordCloud$review_pros, collapse=" ")
    review_source_pros <- VectorSource(review_text_pros)
    corpus <- Corpus(review_source_pros)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    frequency
  })
  
  wordsListNegative <- reactive({
    
    dataForWordCloud <- updateInputDataForReviewTable() 
    
    review_text_pros <- paste(dataForWordCloud$review_cons, collapse=" ")
    review_source_pros <- VectorSource(review_text_pros)
    corpus <- Corpus(review_source_pros)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    frequency
    
  })
  
  output$wordCloudPlotPositive <- renderPlot({
    v <- wordsListPositive()
    wordcloud_rep(names(v), v, scale=c(5,0.3),
                  min.freq = input$sliderForReviewFreq, max.words=input$sliderForReviewMaxWord,
                  colors=brewer.pal(8, "Paired"))
  })
  
  output$wordCloudPlotNegative <- renderPlot({
    v2 <- wordsListNegative()
    wordcloud_rep(names(v2), v2, scale=c(3,0.5),
                  min.freq = input$sliderForReviewFreq, max.words=input$sliderForReviewMaxWord,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  #///////////////////////////////////////////////
  # Component 5.1: Resume keywords WordCloud 
  #///////////////////////////////////////////////
  wordcloud_keywords <- repeatable(wordcloud)
  
  wordsListCompKeywords <- reactive({
    
    #View(dice_comp_keyword)
    dataOfCompanyKeywordsForWordCloud <- dice_comp_keyword
    
    comp_keywords_text_pros <- paste(dataOfCompanyKeywordsForWordCloud$desc_keywords, collapse=",")
    keyword_source_pros <- VectorSource(comp_keywords_text_pros)
    corpus <- Corpus(keyword_source_pros)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, c("manager", "data"))  
    
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    frequency
    
  })
  
  #///////////////////////////////////////////////
  # Component 5.2: Keyword Frequency DataTable
  #///////////////////////////////////////////////
  output$wordCloudPlotTopWordsResume <- renderPlot({
    kwords <- wordsListCompKeywords()
    
    wordcloud_keywords(names(kwords), kwords, scale=c(5,0.3),
                       min.freq = input$sliderForCompKeywordsFreq, max.words=input$sliderForCompKeywordsMaxWord,
                       colors=brewer.pal(12, "Paired"))
  })
  
  updateInputDataForKeywordAnalysisTable <- reactive({
    wfreqency <- wordsListCompKeywords()
    dataFilteredForReview <- data.frame(word=names(wfreqency), freq=wfreqency)
    
    dataFilteredForReview
  })
  
  output$myCompKeywordsDataTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForCompKeywordsDTable <- updateInputDataForKeywordAnalysisTable() 
    dataForCompKeywordsDTable <- dataForCompKeywordsDTable[(input$sliderForCompKeywordsFreq <= dataForCompKeywordsDTable$freq),]
    dataForCompKeywordsDTable                   
    
  }, 
  rownames = FALSE,
  class = 'cell-border stripe',
  extensions = c('ColVis','ColReorder','Scroller'),
  options = list(
    searching = F,
    lengthMenu = list(c(20, 5, 10, 15, 25, 50, 100), c('20', '5', '10', '15', '25','50','100')),
    dom = 'RC<"clear">lfrtip',
    scrollX = TRUE,
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )))
  
  
  
} # end of Server


app = shinyApp(
  ui <- ui,
  server <- server
)

# shiny::runApp(host="192.168.2.179",port=3168)
runApp(app, host = '0.0.0.0', port = 3168)

# 192.168.2.178:3168

