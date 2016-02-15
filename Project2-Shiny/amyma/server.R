server <- function(input, output) {
## review
    reviewdata <- reactive({
    df <- review %>%
      filter(date>=input$tr[1],
             date<=input$tr[2])%>%
      group_by_(input$tm) %>%
      summarise(.,count=n())
  })
    reviewdate<-reactive({
      df <- review %>%
        group_by(date) %>%
        summarise(.,count=n())
      xt <- xts(x = df$count, order.by = df$date)
       })
    
    rwd<-reactive({review_txt})

    output$dygraph <- renderDygraph({
      dygraph(reviewdate(),ylab = "Num. of Reviews / Day") %>%
        dyOptions(drawGrid = input$showgrid)%>%
        dySeries("V1", label = "Num. of Reviews")%>%
        dyAnnotation("2015-1-2", text = "A") %>%
        dyAnnotation("2014-1-2", text = "A")%>%
        dyAnnotation("2015-5-19", text = "B") %>% # TUESDAY
        dyAnnotation("2015-8-6", text = "B.") %>% # THURSDAY
        dyAnnotation("2015-8-7", text = "B.")%>% # FRIDAY
        dyRangeSelector()%>%
        dyRoller(rollPeriod = 1)

    })
    
    
  output$geoChart <- renderGvis({
    gvisLineChart(reviewdata(),options=list(legend="none",hAxis="{title:'Time'}",
                                  vAxis="{title:'Number of Reviews'}",
                                  series="[{color:'green', targetAxisIndex: 0}]",
                                  gvis.editor="Make a change?"))
  })
wordcloud_rep <- repeatable(wordcloud)
output$wordcloud<- renderPlot({
  wordcloud_rep(words = rwd()$word, freq = rwd()$X1, scale=c(5,1),
                  min.freq = input$rfreq, 
                  max.words=input$rmax,
                  rot.per=0.2,
                  random.order=F, 
                  colors=brewer.pal(8, "Dark2"))
  })
## map
  mapdata <- reactive({
    df <- map %>%
      filter(neighbourhood_group_cleansed %in% input$neighbour,
             room_type %in% input$room,
             price >=input$price[1],
             price<=input$price[2],
             number_of_reviews >=input$review[1],
             number_of_reviews <=input$review[2],
             review_scores_rating>=input$rating[1],
             review_scores_rating<=input$rating[2])
  })

  pricedata<-reactive({
    df<-mapdata()%>%
      group_by(room_type)%>%
      summarise(med_price=median(price))
  })
  ratingdata<-reactive({
    df<-mapdata()%>%
      group_by(room_type)%>%
      summarise(avg_rating=mean(review_scores_rating))
  })
  
  output$map <- renderLeaflet({
    map<-leaflet() %>%
      setView(lng = -73.94197, lat = 40.73638, zoom = 12) %>%#change view by selection
      addProviderTiles("CartoDB.Positron") %>%
       addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')
    map%>%
      addLegend("topright", pal = pal, values =  c("Entire home/apt", "Private room", "Shared room"),
                    title = "Room Type",
                    opacity = 1)  
  })
  
  
  observe({
    leafletProxy("map", data = mapdata()) %>%
      clearShapes() %>%
       addCircles(radius=45,stroke = FALSE, fillOpacity =0.5,color = ~pal(room_type), 
                  popup = ~paste(sep = "<br/>","Room Type:",room_type,
                          "Neighbourhood:",neighbourhood_group_cleansed,
                          "Price:",price,
                         "Minimum Nights:", minimum_nights,
                          "Num. of Reviews:",number_of_reviews,
                          "Review Rating:",review_scores_rating))
    
 })
  output$histroom <- renderPlot({
    
    ggplot(mapdata(), aes(room_type, ..count..)) + 
      geom_bar(aes(fill = room_type))+
      theme_hc()+
      scale_fill_manual(values = c("Entire home/apt"= "#EE3B3B",
                                   "Private room" ="#0000EE",
                                   "Shared room" ="#66CD00"),
                        guide=FALSE)+
      labs(y="Count",x="Room Type")
    
  })
  
  output$medprice <- renderPlot({
    
    ggplot(pricedata(), aes(x=room_type,y=med_price)) + 
      geom_bar(aes(fill = room_type),stat="identity",position='dodge')+
      theme_hc()+
      scale_fill_manual(values = c("Entire home/apt"= "#EE3B3B",
                                   "Private room" ="#0000EE",
                                   "Shared room" ="#66CD00"),
                        guide=FALSE)+
      labs(y="Median Price",x="Room Type")
  })  
  
  output$avgrating <- renderPlot({
    
    ggplot(ratingdata(), aes(x=room_type,y=avg_rating)) + 
      geom_bar(aes(fill = room_type),stat="identity",position='dodge')+
      theme_hc()+
      scale_fill_manual(values = c("Entire home/apt"= "#EE3B3B",
                                   "Private room" ="#0000EE",
                                   "Shared room" ="#66CD00"),
                        guide=FALSE)+
      labs(y="Avg. Rating Score",x="Room Type")
  })  
## listing
  
  listdata <- reactive({
    df <- map %>%
      filter(review_scores_rating>=input$listrating[1],
             review_scores_rating<=input$listrating[2])%>%
      select(neighbourhood_group_cleansed,room_type)
  })
  
   listdata2<-reactive({
     df<- map%>%
    filter(review_scores_rating>=input$listrating[1],
          review_scores_rating<=input$listrating[2])%>%
    group_by(neighbourhood_group_cleansed,room_type) %>%
    tally  %>%
    group_by(neighbourhood_group_cleansed) %>%
    mutate(pct=(100*n)/sum(n))%>%
    select(neighbour=neighbourhood_group_cleansed,room_type,pct)
   })
  output$activelist<-renderGvis({
    if(input$listformat=="Count"){
    df<-table(listdata()$neighbourhood_group_cleansed,listdata()$room_type)
    df<-as.data.frame.matrix(df)
    df$neighbour<-rownames(df)}
    if(input$listformat=="Percentage") {
      df <- as.data.frame(with(listdata2(), tapply(pct, list(neighbour, room_type) , I)))
      df$neighbour<-rownames(df)
    }
   gvisBarChart(df,"neighbour",c("Entire home/apt","Private room","Shared room"),
                options=list(colors= "['#EE3B3B', '#0000EE','#66CD00']",
                             legend="bottom",
                             bar="{groupWidth:'90%'}",gvis.editor="Make a change?",
                            width=700,height=400))
   
  })

 
  output$ziptable <- DT::renderDataTable({
    df <- host %>% 
      filter(host_since>=input$hostt[1],
             host_since<=input$hostt[2])%>%
      arrange(.,desc(host_total_listings_count))
    DT::datatable(head(df,input$hostn), class = 'cell-border stripe', escape = FALSE)
  })

}
