library(shiny)
library(htmltools)
library(leaflet)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
  #Reading Variable
  collectCoordinate <- read.csv("./data/collectCoordinate.csv", stringsAsFactors=FALSE)

  output$mymap <-renderLeaflet({
    
        leaflet() %>%
          addProviderTiles("Stamen.TonerLite",
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          #addMarkers(data = collectCoordinate, popup = ~as.character(collectCoordinate$NumberSampels))
          addCircleMarkers(
            data = collectCoordinate,
            radius = as.numeric(collectCoordinate$NumberSampels),
            stroke = T, opacity = 50,
            fill = T, fillColor = "#920000",
            fillOpacity = 50,
            #popup = cordinates$AverageWage,
            color = "white"
          )
          # 
  }) #End of map
  
  output$sampleHIs <- renderPlot({
      histoGo <- qplot(Location, NumberSampels, data = collectCoordinate)
      histoGo + geom_bar(stat="identity",fill="#00b8e6", colour="black") + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                                               panel.background = element_blank())
  })
  
  output$ginImage01 <- renderImage({

    list(
      src = './img/ginseng01.jpg',
      fileType = "image/jpg",
      width = 280,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  
  output$title <- renderImage({
    
    list(
      src = './img/title.jpg',
      fileType = "image/jpg",
      # width = 280,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)

  output$anova <- renderImage({
    
    list(
      src = './img/anova01.jpg',
      fileType = "image/jpg",
      width = 700,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  
  output$soil <- renderImage({
    
    list(
      src = './img/soil01.jpg',
      fileType = "image/jpg",
       width = 700,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  
  output$mediTitle <- renderImage({
    
    list(
      src = './img/mediTitle01.jpg',
      fileType = "image/jpg",
      width = 700,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  
  output$gelPhoto <- renderImage({

    list(
      src = './img/gelPhoto.jpg',
      fileType = "image/jpg",
      width = 700,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  
  output$gelPCA <- renderImage({
    print ('pca')
    list(
      src = './img/NYPCAgel01.jpg',
      fileType = "image/jpg",
      width = 700,
      # height = 500,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)
  

################################################
  showRawData<- read.csv("./data/molecular.csv", stringsAsFactors=FALSE)
  output$rawMole <- renderDataTable(
    showRawData
  )
######################=================================================================================================================
  output$vecFirstTable <- renderDataTable({   #DT::renderDataTable({
    #Vector Machine begin for Molecular Marker
    #View(linearly.separable)
    
    #Creating training and test sets.
  set.seed(input$randomSet)
  
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    molData <- read.csv(inFile$datapath, header = TRUE,
             sep = ',', quote = '"')
    linearly.separable <- molData[, 5:ncol(molData)]
    linearly.separable$PopuNY <- as.factor(linearly.separable$PopuNY)

    
  all.samples = sample(1:111, 100)
  num.all = length(all.samples)
  train.index = all.samples[1:(num.all*0.8)]
  test.index = -train.index
    
    #get polymorphic column
    firstColName <- as.character(names(linearly.separable)[1])
    polycolName <- c(firstColName)
    for (i in 2:125) {
      if(sum(linearly.separable[train.index, i]) != length(train.index) & 
         sum(linearly.separable[test.index, i]) != length(test.index)) {
        colName <- as.character(names(linearly.separable)[i])
        polycolName <- c(polycolName, c(colName) )
      }
    }
    
      #Chose best column
        nyData <- linearly.separable[linearly.separable$PopuNY== "NY", ]
        nonNYData <- linearly.separable[linearly.separable$PopuNY== "noNY",]
        numNY <- nrow(nyData)
        numNonNY <- nrow(nonNYData)
        diffColumn <- c()
        PrimerName <- c()
        for (j in 2:125) {
          nyBand <- sum(nyData[, j])
          nonNYBand <- sum(nonNYData[, j])
          NYFrequen <- (nyBand/(nyBand + nonNYBand))*(numNonNY/nrow(linearly.separable))
          nonNYFrequen <- (nonNYBand/(nyBand + nonNYBand))*(numNY/nrow(linearly.separable))
          diffBetweenNYnon <- abs(NYFrequen - nonNYFrequen)
          diffColumn <- c(diffColumn, c(diffBetweenNYnon))
          colName01 <- as.character(names(linearly.separable)[j])
          PrimerName <- append(PrimerName, colName01) 
        }

        ##############End of Chose best Column
    # 
    if (input$bestWholeVec == "Best10") {
      dataDiffFrameS <- data.frame(PrimerName, diffColumn)
      dataDiffFrame <- dataDiffFrameS[order(-diffColumn),][1:10,]
      top10Primer <- append(dataDiffFrame$PrimerName, 1)
      print(dataDiffFrame$PrimerName)
      #print(linearly.separable[, top10Primer])
      linearly.separable <- linearly.separable[, top10Primer]
    } else if (input$bestWholeVec == "Whole") {
      linearly.separable <- linearly.separable[, polycolName]
    }

    library(e1071)
    svm.mmc.linear = svm(PopuNY ~ ., #Familiar model fitting notation.
                         data = linearly.separable, #Using the linearly separable data.
                         subset = train.index, #Using the training data.
                         kernel = "linear", #Using a linear kernel.
                         cost = input$svmCost) #1e6 A very large cost; default is 1.
    
    #Visualizing the results of the maximal margin classifier.
    #plot(svm.mmc.linear, linearly.separable[train.index, ])
    
    #Additional information for the fit.
    summary01 <<- summary(svm.mmc.linear)
    print(summary01)
    
    #Finding the indices of the support vectors.
    svm.mmc.linear$index
    
    #Predicting on the test data.
    ypred = predict(svm.mmc.linear, linearly.separable[test.index, ])
    predicTable01 <<- table("Predicted Values" = ypred, "True Values" = linearly.separable[test.index, "PopuNY"])
    print(predicTable01)
    ####################################################################
    predicTable01 <- as.data.frame(predicTable01)
    predictedValues01 <- levels(predicTable01[, 1])
    noNY <- predicTable01[1:2,3]
    NY <- predicTable01[3:4, 3]

    printTable <- data.frame(predictedValues01, noNY, NY)

    #correctGuess <- newDataTable$TrueOrNot
    tableNum <- predicTable01[, 3]
    hitRate <<- (tableNum[1]+tableNum[4])/sum(tableNum)*100
    
    DT::datatable(printTable, options = list(pageLength = 50, searching = FALSE)) #, colnames = c('Prediction', 'Real Value \n-1 ', 'Real Value \n+1', 'Correct?'))

  })# End of vecFirstTable
#################End of vecFirstTable
  output$bestVector <- renderDataTable({
      
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    molData <- read.csv(inFile$datapath, header = TRUE,
                        sep = ',', quote = '"')
    linearly.separable <- molData[, 5:ncol(molData)]
    linearly.separable$PopuNY <- as.factor(linearly.separable$PopuNY)

    firstColName <- as.character(names(linearly.separable)[1])

    if (input$bestWholeVec == "Best10") {
      #Chose best column
      nyData <- linearly.separable[linearly.separable$PopuNY== "NY", ]
      nonNYData <- linearly.separable[linearly.separable$PopuNY== "noNY",]
      numNY <- nrow(nyData)
      numNonNY <- nrow(nonNYData)
      diffColumn <- c()
      PrimerName <- c()
      for (j in 2:125) {
        nyBand <- sum(nyData[, j])
        nonNYBand <- sum(nonNYData[, j])
        NYFrequen <- (nyBand/(nyBand + nonNYBand))*(numNonNY/nrow(linearly.separable))
        nonNYFrequen <- (nonNYBand/(nyBand + nonNYBand))*(numNY/nrow(linearly.separable))
        diffBetweenNYnon <- abs(NYFrequen - nonNYFrequen)
        diffColumn <- c(diffColumn, c(diffBetweenNYnon))
        colName01 <- as.character(names(linearly.separable)[j])
        PrimerName <- append(PrimerName, colName01) 
      }
      dataDiffFrameS <- data.frame(PrimerName, diffColumn)
      dataDiffFrame <- dataDiffFrameS[order(-diffColumn),][1:10,]

      ##############End of Chose best Column
    } else if (input$bestWholeVec == "Whole") {
      aa <- c(1)
      bb <- c(1)
      dataDiffFrame <- data.frame(aa, bb)
    }
    dataDiffFrame
  })
  
  output$hitRate <- renderText({
    start <- input$randomSet
    cost <- input$svmCost
    wakeup <- input$bestWholeVec
    wakeup02 <- input$file1
    if (exists("hitRate")) {
      hitRate
    } else {
      hitRate <- "Input the data file."
      hitRate
    }
  })
  
  output$summary01 <- renderPrint({
    start <- input$randomSet
    cost <- input$svmCost
    wakeup <- input$bestWholeVec
    wakeup02 <- input$file1
    
    if (exists("summary01")) {
      summary01
    } else {
      summary01 <- "Input the data file for summary."
      summary01
    }
    
  })
  
  aa <<- function(x) {
    x <- "Test function"
    return(x)
  }
  
  output$test01 <- renderPrint({
      bb <- function(x) {
        x <- 1000
        return(x)
      }
      aa(100)
  })

})# end of shinyServer
