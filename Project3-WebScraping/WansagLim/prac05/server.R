library(shiny)
library(DT)
library(jsonlite)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  articleSearch <- reactive(input$text)
    # eventReactive(input$goButton,{ input$text}) 
  
  year10 <- reactive(input$year)
    # eventReactive(input$yearButton,{input$year} )
  
  yearInterest <- reactive(input$yearInterest)
  yearMonth <- reactive(input$yearMonth)
  monthInterest <- reactive(input$monthInterest)

  #renderPrint, renderPlot
  output$graphHit <-renderPlot({
    input$goButton
    isolate({
      articleSearch <- articleSearch()
      year10 <- year10()
      year09 <- year10() - 1
      year08 <- year10() - 2
      year07 <- year10() - 3
      year06 <- year10() - 4
      year05 <- year10() - 5
      year04 <- year10() - 6
      year03 <- year10() - 7
      year02 <- year10() - 8
      year01 <- year10() - 9
      year00 <- year10() - 10
      
      year10 <- paste(year10, "0217", sep = "") 
      year09 <- paste(year09, "0217", sep = "") 
      year08 <- paste(year08, "0217", sep = "") 
      year07 <- paste(year07, "0217", sep = "") 
      year06 <- paste(year06, "0217", sep = "") 
      year05 <- paste(year05, "0217", sep = "") 
      year04 <- paste(year04, "0217", sep = "") 
      year03 <- paste(year03, "0217", sep = "") 
      year02 <- paste(year02, "0217", sep = "") 
      year01 <- paste(year01, "0217", sep = "")
      year00 <- paste(year00, "0217", sep = "")

      address <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=korea&begin_date=20150217&end_date=20160216&sort=newest&api-key=48c66fa2c448eda40826487d4f19a018%3A0%3A71658152"
      articleAddress <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?q="
      
      
      articleKey <- "&api-key=48c66fa2c448eda40826487d4f19a018:0:71658152"
      articleAPI10 <- paste(articleAddress, articleSearch,"&begin_date=",year09,"&end_date=",year10,"&sort=newest",articleKey, sep = "")
      articleAPI09 <- paste(articleAddress, articleSearch,"&begin_date=",year08,"&end_date=",year09,"&sort=newest",articleKey, sep = "")
      articleAPI08 <- paste(articleAddress, articleSearch,"&begin_date=",year07,"&end_date=",year08,"&sort=newest",articleKey, sep = "")
      articleAPI07 <- paste(articleAddress, articleSearch,"&begin_date=",year06,"&end_date=",year07,"&sort=newest",articleKey, sep = "")
      articleAPI06 <- paste(articleAddress, articleSearch,"&begin_date=",year05,"&end_date=",year06,"&sort=newest",articleKey, sep = "")
      articleAPI05 <- paste(articleAddress, articleSearch,"&begin_date=",year04,"&end_date=",year05,"&sort=newest",articleKey, sep = "")
      articleAPI04 <- paste(articleAddress, articleSearch,"&begin_date=",year03,"&end_date=",year04,"&sort=newest",articleKey, sep = "")
      articleAPI03 <- paste(articleAddress, articleSearch,"&begin_date=",year02,"&end_date=",year03,"&sort=newest",articleKey, sep = "")
      articleAPI02 <- paste(articleAddress, articleSearch,"&begin_date=",year01,"&end_date=",year02,"&sort=newest",articleKey, sep = "")
      articleAPI01 <- paste(articleAddress, articleSearch,"&begin_date=",year00,"&end_date=",year01,"&sort=newest",articleKey, sep = "")
      
      #normalize "about
      aboutAPI10 <- paste(articleAddress, "about","&begin_date=",year09,"&end_date=",year10,"&sort=newest",articleKey, sep = "")
      aboutAPI09 <- paste(articleAddress, "about","&begin_date=",year08,"&end_date=",year09,"&sort=newest",articleKey, sep = "")
      aboutAPI08 <- paste(articleAddress, "about","&begin_date=",year07,"&end_date=",year08,"&sort=newest",articleKey, sep = "")
      aboutAPI07 <- paste(articleAddress, "about","&begin_date=",year06,"&end_date=",year07,"&sort=newest",articleKey, sep = "")
      aboutAPI06 <- paste(articleAddress, "about","&begin_date=",year05,"&end_date=",year06,"&sort=newest",articleKey, sep = "")
      aboutAPI05 <- paste(articleAddress, "about","&begin_date=",year04,"&end_date=",year05,"&sort=newest",articleKey, sep = "")
      aboutAPI04 <- paste(articleAddress, "about","&begin_date=",year03,"&end_date=",year04,"&sort=newest",articleKey, sep = "")
      aboutAPI03 <- paste(articleAddress, "about","&begin_date=",year02,"&end_date=",year03,"&sort=newest",articleKey, sep = "")
      aboutAPI02 <- paste(articleAddress, "about","&begin_date=",year01,"&end_date=",year02,"&sort=newest",articleKey, sep = "")
      aboutAPI01 <- paste(articleAddress, "about","&begin_date=",year00,"&end_date=",year01,"&sort=newest",articleKey, sep = "")
      
      #artiConD
      if (articleSearch == 'Enter search term' || articleSearch == "") {
        message01 <- "Please, Type your search term"
        message01
      } else {
        articleContent10 <- fromJSON(articleAPI10)
        articleContent09 <- fromJSON(articleAPI09)
        articleContent08 <- fromJSON(articleAPI08)
        articleContent07 <- fromJSON(articleAPI07)
        articleContent06 <- fromJSON(articleAPI06)
        articleContent05 <- fromJSON(articleAPI05)
        articleContent04 <- fromJSON(articleAPI04)
        articleContent03 <- fromJSON(articleAPI03)
        articleContent02 <- fromJSON(articleAPI02)
        articleContent01 <- fromJSON(articleAPI01)
        
        Sys.sleep(1)
        #normalize "About"
        normalContent10 <- fromJSON(aboutAPI10)
        normalContent09 <- fromJSON(aboutAPI09)
        normalContent08 <- fromJSON(aboutAPI08)
        normalContent07 <- fromJSON(aboutAPI07)
        normalContent06 <- fromJSON(aboutAPI06)
        normalContent05 <- fromJSON(aboutAPI05)
        normalContent04 <- fromJSON(aboutAPI04)
        normalContent03 <- fromJSON(aboutAPI03)
        normalContent02 <- fromJSON(aboutAPI02)
        normalContent01 <- fromJSON(aboutAPI01)
        
        artiConHit10 <- articleContent10$response$meta$hits
        artiConHit09 <- articleContent09$response$meta$hits
        artiConHit08 <- articleContent08$response$meta$hits
        artiConHit07 <- articleContent07$response$meta$hits
        artiConHit06 <- articleContent06$response$meta$hits
        artiConHit05 <- articleContent05$response$meta$hits
        artiConHit04 <- articleContent04$response$meta$hits
        artiConHit03 <- articleContent03$response$meta$hits
        artiConHit02 <- articleContent02$response$meta$hits
        artiConHit01 <- articleContent01$response$meta$hits
        
        #normalize hit
        normalConHit10 <- normalContent10$response$meta$hits
        normalConHit09 <- normalContent09$response$meta$hits
        normalConHit08 <- normalContent08$response$meta$hits
        normalConHit07 <- normalContent07$response$meta$hits
        normalConHit06 <- normalContent06$response$meta$hits
        normalConHit05 <- normalContent05$response$meta$hits
        normalConHit04 <- normalContent04$response$meta$hits
        normalConHit03 <- normalContent03$response$meta$hits
        normalConHit02 <- normalContent02$response$meta$hits
        normalConHit01 <- normalContent01$response$meta$hits
        
        
        hitVec <- c(artiConHit10, artiConHit09, artiConHit08, artiConHit07, artiConHit06, artiConHit05, artiConHit04, artiConHit03, artiConHit02, artiConHit01)
        hitNormalVec <- c(normalConHit10,normalConHit09, normalConHit08, normalConHit07, normalConHit06, normalConHit05, normalConHit04, normalConHit03, normalConHit02, normalConHit01)
        hitNormlizedVec <- hitVec / hitNormalVec
        years <- c(year10, year09, year08, year07, year06, year05, year04, year03, year02, year01)
        #Graph.Table
        graphTable <- data.frame(years, hitVec, hitNormalVec, hitNormlizedVec)
        

        ### Graph
        barHitGraph <- ggplot(graphTable, aes(x=years, y=hitVec)) + geom_bar(stat="identity")
        barNormLizedGraph <- ggplot(graphTable, aes(x=years, y=hitNormlizedVec)) + geom_bar(stat="identity")
        barJustAbout <- ggplot(graphTable, aes(x=years, y=hitNormalVec)) + geom_bar(stat="identity")
        
        if (input$radio == 1) {
          barHitGraph + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                           axis.text.y = element_text(size = 12),
                           panel.background = element_blank())
        } else if (input$radio == 2) {
          barNormLizedGraph + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                                   axis.text.y = element_text(size = 12),
                                   panel.background = element_blank())
        } else if (input$radio == 3) {
          barJustAbout + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                                 axis.text.y = element_text(size = 12),
                                 panel.background = element_blank())
        } 
        
      }
    }) # End of Isolate

    
  }) #Graph Hit
  
  output$graphMonth <- renderPlot({
    input$yearMonthButton
    isolate({
      articleSearch <- articleSearch()
      yearMonth <- yearMonth()
      
      month12End <- paste(yearMonth, "1231", sep = "")
      month12 <- paste(yearMonth, "1201", sep = "")
      month11 <- paste(yearMonth, "1101", sep = "")
      month10 <- paste(yearMonth, "1001", sep = "")
      month09 <- paste(yearMonth, "0901", sep = "")
      month08 <- paste(yearMonth, "0801", sep = "")
      month07 <- paste(yearMonth, "0701", sep = "")
      month06 <- paste(yearMonth, "0601", sep = "")
      month05 <- paste(yearMonth, "0501", sep = "")
      month04 <- paste(yearMonth, "0401", sep = "")
      month03 <- paste(yearMonth, "0301", sep = "")
      month02 <- paste(yearMonth, "0201", sep = "")
      month01 <- paste(yearMonth, "0101", sep = "")
      
      articleAddress <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?q="
      
      articleKey <- "&api-key=48c66fa2c448eda40826487d4f19a018:0:71658152"
      hitMon12 <- paste(articleAddress, articleSearch,"&begin_date=",month12,"&end_date=",month12End,articleKey, sep = "")
      hitMon11 <- paste(articleAddress, articleSearch,"&begin_date=",month11,"&end_date=",month12,articleKey, sep = "")
      hitMon10 <- paste(articleAddress, articleSearch,"&begin_date=",month10,"&end_date=",month11,articleKey, sep = "")
      hitMon09 <- paste(articleAddress, articleSearch,"&begin_date=",month09,"&end_date=",month10,articleKey, sep = "")
      hitMon08 <- paste(articleAddress, articleSearch,"&begin_date=",month08,"&end_date=",month09,articleKey, sep = "")
      hitMon07 <- paste(articleAddress, articleSearch,"&begin_date=",month07,"&end_date=",month08,articleKey, sep = "")
      hitMon06 <- paste(articleAddress, articleSearch,"&begin_date=",month06,"&end_date=",month07,articleKey, sep = "")
      hitMon05 <- paste(articleAddress, articleSearch,"&begin_date=",month05,"&end_date=",month06,articleKey, sep = "")
      hitMon04 <- paste(articleAddress, articleSearch,"&begin_date=",month04,"&end_date=",month05,articleKey, sep = "")
      hitMon03 <- paste(articleAddress, articleSearch,"&begin_date=",month03,"&end_date=",month04,articleKey, sep = "")
      hitMon02 <- paste(articleAddress, articleSearch,"&begin_date=",month02,"&end_date=",month03,articleKey, sep = "")
      hitMon01 <- paste(articleAddress, articleSearch,"&begin_date=",month01,"&end_date=",month02,articleKey, sep = "")
      
      articleMonth12 <- fromJSON(hitMon12)
      articleMonth11 <- fromJSON(hitMon11)
      articleMonth10 <- fromJSON(hitMon10)
      articleMonth09 <- fromJSON(hitMon09)
      articleMonth08 <- fromJSON(hitMon08)
      articleMonth07 <- fromJSON(hitMon07)
      Sys.sleep(1.1)
      articleMonth06 <- fromJSON(hitMon06)
      articleMonth05 <- fromJSON(hitMon05)
      articleMonth04 <- fromJSON(hitMon04)
      articleMonth03 <- fromJSON(hitMon03)
      articleMonth02 <- fromJSON(hitMon02)
      articleMonth01 <- fromJSON(hitMon01)
      
      hitMonVec <- c( articleMonth12$response$meta$hits, articleMonth11$response$meta$hits, articleMonth10$response$meta$hits,
            articleMonth09$response$meta$hits, articleMonth08$response$meta$hits, articleMonth07$response$meta$hits, articleMonth06$response$meta$hits,
            articleMonth05$response$meta$hits, articleMonth04$response$meta$hits, articleMonth03$response$meta$hits, articleMonth02$response$meta$hits,
            articleMonth01$response$meta$hits)
      
      monthVec <- c(month12, month11, month10, month09, month08, month07, month06, month05, month04, month03,
                    month02, month01)
      graphMonthTable <- data.frame(monthVec, hitMonVec)
      
      barMonthGraph <- ggplot(graphMonthTable, aes(x=monthVec, y=hitMonVec)) + geom_bar(stat="identity")
      barMonthGraph + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                            axis.text.y = element_text(size = 12),
                            panel.background = element_blank())
      
    })#end of isolate
  })#End of graphMonth
  
  output$articleExam <- DT::renderDataTable({
    input$goArticle
    isolate({
      articleSearch <- articleSearch()
      yearInterest <- yearInterest()
      yearInterest01 <- yearInterest - 1
      monthInterest <- monthInterest()
      
      yearForMonth <- yearInterest
      yearInterest01 <- paste(yearInterest01, "0217", sep = "") 
      yearInterest <- paste(yearInterest, "0217", sep = "")

      
      articleAddress <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?q="
      
      articleKey <- "&api-key=48c66fa2c448eda40826487d4f19a018:0:71658152"
      articleAPI10 <- paste(articleAddress, articleSearch,"&begin_date=",yearInterest01,"&end_date=",yearInterest,articleKey, sep = "")
      
      articleContent10 <- fromJSON(articleAPI10)
      
      artiConDoc10 <- articleContent10$response$docs
      
      artiURL10 <- artiConDoc10$web_url
      artiConSnippet10 <- artiConDoc10$snippet
      
      table10 <- data.frame(artiConSnippet10, artiURL10)
      
      #Articlae Table
      if (input$radioMonth == 1) {
        table10
      } else if (input$radioMonth == 2) {
          if (monthInterest <= 9) {
            monthInterest <- paste("0", monthInterest, sep = "")
          } else if (monthInterest >10) {
            monthInterest <- as.character(monthInterest)
          } 
          beginMon <- paste(yearForMonth, monthInterest, "01", sep = "" )
          endMon <- paste(yearForMonth, monthInterest, "30", sep = "")
          articleMonthAPI <- paste(articleAddress, articleSearch,"&begin_date=",beginMon,"&end_date=",endMon,articleKey, sep = "")
          monthContent <- fromJSON(articleMonthAPI)
          monConDoc <- monthContent$response$docs
          monURL <- monConDoc$web_url
          monSnippet <- monConDoc$snippet
          tableMonth <- data.frame(monSnippet, monURL)
          tableMonth
      }
      
    }) # End of isolate
  })
  
  output$rawAPI <- renderPrint({
    articleSearch <- articleSearch()
    yearInterest <- yearInterest()
    yearInterest01 <- yearInterest - 1
    
    yearInterest <- paste(yearInterest, "0217", sep = "") 
    yearInterest01 <- paste(yearInterest01, "0217", sep = "") 
    
    articleAddress <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?q="
    
    articleKey <- "&api-key=48c66fa2c448eda40826487d4f19a018:0:71658152"
    articleAPI10 <- paste(articleAddress, articleSearch,"&begin_date=",yearInterest01,"&end_date=",yearInterest,"&sort=newest",articleKey, sep = "")
    
    articleContent10 <- fromJSON(articleAPI10)
    infoRawAPI <- c(yearInterest, articleSearch, articleContent10)
    infoRawAPI
  })
  
  output$test03 <- renderPrint({
    apiData <- fromJSON('/Volumes/64GB/NYC/project03/api2012.txt')
    apiResu <- apiData$results
    apiResu
  })  
  
  output$codeText <- renderText({
    aa <- scan(file ="ui.R", what = "character", sep=":")
    bb <- paste(aa, collapse = "\n ")
    aaa <- "http://shiny.rstudio.com/articles/html-tags.html"
    aa 
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
})
