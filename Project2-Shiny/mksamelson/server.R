library(shiny)
library(datasets)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)

setwd("C:/Users/Matt/Dropbox/DataScienceBootcamp/Projects/ShinyProject/Brokers/data")

surveydata = read.csv("masterdata.csv", header = TRUE)

comments <- c("executing.high.touch.orders" = "This chart shows average firm satisfaction with broker high-touch execution quality",
              "locating.natural.liquidity" = "comment2",
              "impact.of.market.conditions" = "comment3",
              "understand.of.your.firm" = "comment4",
              "availability.of.broker.capital" = "comment5")

# #DensityText <- reactive({
#   
#   
#   #question = input$Chart
#   question = "executing.high.touch.orders"
#   
#   #select the relevent data columns pertinent to the question
#   
#   question_df = select(surveydata,contains(question))
#   
#   #company names and strip off un=neccessary parts of the name
#   
#   names_vec = names(question_df)
#   
#   for (i in 1:length(names_vec)){
#     names_vec[i] = sub('.Quality.*','', names_vec[i])
#   }
#   
#   #reassign condensed names to data columns
#   
#   names(question_df)= names_vec
#   
#   #Determine how many votes for each company.  
#   #Drop companies with fewer than 5 votes
#   
#   number_votes = sapply(question_df,function(x) length(which(!is.na(x))))
#   number_votes = number_votes[number_votes >5]
#   
#   names(number_votes)
#   
#   question_df = question_df[names(number_votes)]
#   
#   #melt the wide form data and strip out the NA values
#   
#   question_df = melt(question_df)
#   question_df = question_df[!is.na(question_df$value),]  
#   question_df_2 = question_df
#   print(question_df_2)
#   str(question_df_2)
#   
#   #return (question_df_2)
# #})


# Define server logic required to plot Broker Quality
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  
  formulaText <- reactive({
  
       question = input$Chart
       question_df = select(surveydata,contains(question))
       names_vec = names(question_df)
       
       for (i in 1:length(names_vec)){
            names_vec[i] = sub('.Quality.*','', names_vec[i])
       }
  
       names(question_df)= names_vec
  
       number_votes = sapply(question_df,function(x) length(which(!is.na(x))))
       number_votes = number_votes[number_votes >5]
  
       names(number_votes)
  
       question_df = question_df[names(number_votes)]
  
       question_df_reduced = round(sapply(question_df,function(x) mean(x, na.rm=TRUE)),3)
  
       question_df_reduced = question_df_reduced[order(question_df_reduced,decreasing=TRUE)]

       quartile <- ntile(question_df_reduced, 4) 
       combined = cbind(question_df_reduced,quartile) 
       combined <- as.data.frame(combined)
       names(combined) <- c("x", "y")
       #print(combined)
       return (combined)
       })
  
  DensityText <- reactive({
    
    
    question = input$Chart
    #question = "executing.high.touch.orders"
    
    #select the relevent data columns pertinent to the question
    
    question_df = select(surveydata,contains(question))
    
    #company names and strip off un=neccessary parts of the name
    
    names_vec = names(question_df)
    
    for (i in 1:length(names_vec)){
      names_vec[i] = sub('.Quality.*','', names_vec[i])
    }
    
    #reassign condensed names to data columns
    
    names(question_df)= names_vec
    
    #Determine how many votes for each company.  
    #Drop companies with fewer than 5 votes
    
    number_votes = sapply(question_df,function(x) length(which(!is.na(x))))
    number_votes = number_votes[number_votes >5]
    
    names(number_votes)
    
    question_df = question_df[names(number_votes)]
    
    #melt the wide form data and strip out the NA values
    
    question_df = melt(question_df)
    question_df = question_df[!is.na(question_df$value),]  
    question_df_2 = question_df
    print(question_df_2)
    return (question_df_2)
  })
 
  output$BrokerSelector <- renderUI({checkboxGroupInput("BrName", "Choose 2 Brokers:", as.list(row.names(formulaText())))})
   
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested

  output$BarPlot <- renderPlot({

      df <- formulaText()

      p <- ggplot(df, aes(seq_along(x), x)) +
        geom_bar(stat="identity",aes(fill=as.factor(y)))+
        theme_bw()+
        scale_x_discrete(labels=row.names(df),waiver()) +
        theme(axis.text.x = element_text(angle = 90, hjust = .95,vjust=2.5))+
        geom_text(aes(label=sprintf("%.3f",x)), angle=90, hjust=-.35,size = 3)+
        xlab("")+ylab("Rating")+
        scale_y_continuous(limits = c(0, 5))+
        ggtitle(input$Chart)
        
      p

      })
    
  output$DensityPlot <- renderPlot({  
      
        broker = input$BrName
        df_1 = DensityText()  
        df_2 = filter(df_1, variable %in% broker[[1]][1]) #first broker data
        df_3 = filter(df_1, variable %in% broker[[2]][1]) #second broker data
        j=as.data.frame((table(df_2$value))/length(df_2$value)) #Tablize firstbroker data and convert to df
        k=as.data.frame((table(df_3$value))/length(df_3$value)) #Tablize secondbroker data and convert to df
        for (r in 1:nrow(j)){
          j$broker[r] = broker[[1]][1]}  #add first broker name back to tabilized data
        for (s in 1:nrow(k)){
          k$broker[s] = broker[[2]][1]}  #add first broker name back to tabilized data
        l = rbind(j,k)
        
        v = ggplot(l,aes(Var1,Freq,fill=l$broker))+
            geom_bar(stat="identity",position="dodge")+
            theme_bw()+
            xlab("Rating")+
            ylab("Density")+
            scale_fill_discrete(name = "Broker")
        
        
        v     

      })
    
  output$comment <- renderUI({
      
      if(is.null(input$Chart)) {
        NULL
      } else {
        h4(comments[input$Chart])
      }
    })

})