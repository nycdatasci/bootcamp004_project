library(shiny)
library(shinydashboard)
library(googleVis)
require(datasets)
library(dplyr)#for reshaping
library(reshape2)#for even more reshaping
#library(tidyr)#for more reshaping
library(ggplot2)#for plotting
library(leaflet)#for maps
library(maptools)#also for maps
library(rgdal)#for reading in shape data for maps


#Goal is to explore various geographical factors (number of food providers in various categories) that may effect two big indicators of health (obesity rate and diabetes rate) 
#Health data is from CHS survey 2013.Food data is from NYC health inspections set from NYC open data, 
#health_zip.csv is the survey data but with the appropriate columns already selected out to make the size more managable for loading.
health_zip=read.csv("health_zip.csv")
health_zip=select(health_zip,bmi,diabetes,sex,race,age,uhf42)#get rid of columns I wont need
health_zip=health_zip[which(complete.cases(health_zip)),]
health_zip$obese=0 #intialize new column
health_zip$obese[which(health_zip$bmi>30)]=1


#Need to convert uhf42 to uhf, and then to zip code, uhf and uhf42 are two different ways the united health fund designates neighborhoods, 
uhf_convert=as.data.frame(c(1:42))#make a data frame with one column for the 42 UHF zones
colnames(uhf_convert)="uhf42"
uhf_convert$uhf=c(101:107,201:211,301:310,401:410,501:504)#I'm guessing the 42 UHF neighbor hoods are in the same order in both numbering schemes, I can use the borough as a partial check, but to a degree this is taken on faith.

obese_zip=select(read.csv("obesity_2013.csv"),uhf=UHF.code,zip=NYC.Zip.Code,pop,area,popdense)#load only the columns  I need from another file I found online that does the zip conversion
#obesity_2013.csv also has someinfo about the zip codes added to them by me personally (via excel :P) I added area and population information when I could find it. The largest consistent table i found only had data as recent as 2000
#areas should be fine and consistent, as I account for zip codes that have since split, but the population and thus its density are unreliable, 
#some zips have had the population quadruple since 2000, so I won't be able to normalize on population. Pop not reliable anyways, since its only offically updated on census years (which the rest of our data isn't on)

uhf_convert=left_join(uhf_convert,obese_zip,by="uhf")#get zips for all of uhf42
uhf_convert$zip=as.character(uhf_convert$zip)

#Now prepare the food data, this is from the leath inspections data set from NYC open data.
food=read.csv("food.csv",stringsAsFactors=FALSE)
food$date=as.Date(as.character(food$date), "%m/%d/%Y")
food=filter(food,date<as.Date("2014-01-01"))
food=filter(food,date>as.Date("2013-01-01"))#take only those restaurants that had an inspection in 2013 (the year of the health survey)
food=select(food,name,zip,cuisine,address,street)#drop date info now that were done using it
food=unique(food)#remove duplicate restaurants (that had more then one inspection at the same location in 2013)
#Can a take sub strings from names that indicate cuisine better than the inspector, i.e donut or pizza in the name
food$cuisine[which(grepl("BURGER",food$name))]="Hamburgers"
food$cuisine[which(grepl("PIZZA",food$name))]="Pizza"
food$cuisine[which(grepl("Donut",food$name))]="Donuts"
#cuisine labels are a little too generic (most are just labeled "american"), i can do a little better by labeling major fast food chains as "fast food")
food$cuisine[food$name=="MCDONALD'S"]="Fastfood"
food$cuisine[food$name=="WENDY'S"]="Fastfood"
food$cuisine[food$name=="BURGER KING, BURGER KING, POPEYES LOUISIANA KITCHEN"]="Fastfood"
food$cuisine[food$name=="KFC"]="Fastfood"
food$cuisine[food$name=="KFC,TACO BELL"]="Fastfood"
food$cuisine[food$name=="TACO BELL"]="Fastfood"
food$cuisine[food$name=="POPEYES LOUISIANA KITCHEN"]="Fastfood"
food$cuisine[food$name=="ARBY'S"]="Fastfood"
#Can also consolidate simliar cuisine listings, make data less sparse, more meaningful
food$cuisine[food$cuisine=="Fruits/Vegetables"]="Vegetarian" 
food$cuisine[food$cuisine=="Salads"]="Vegetarian"#not strictly vegetarian, but I'm going to assume they have more healthy options then alot of others
food$cuisine[food$cuisine=="Hotdogs/Pretzels"]="Hotdogs" 
food$cuisine[food$cuisine=="CafÃ©/Coffee/Tea"]="Café/Coffee/Tea"
food$cuisine[food$cuisine=="Bottled beverages, including water, sodas, juices, etc."]="Juice, Smoothies, Fruit Salads"
food$cuisine[food$cuisine=="Pizza/Italian"]="Pizza"
food$cuisine[food$cuisine=="Sandwiches/Salads/Mixed Buffet"]="Sandwiches"
food$cuisine[food$cuisine=="Ice Cream, Gelato, Yogurt, Ices"]="IceCream"


food=select(food,name,zip,cuisine)
#before joining with the uhf conversion table, have to fix something
food$zip[food$zip=="11249"]="11211" #newer zip code split from old
food$zip[food$zip=="10065"]="10021" #newer zip code split from old
food$zip[food$zip=="10075"]="10021" #newer zip code split from old
food$zip[food$zip=="11109"]="11101" #newer zip code split from old, this one I'm not as sure about, can't find specific info about when it split, but looking at the maps thats seems to be what happend.
food$zip[food$zip=="10112"]="10001" #Rockefeller center has no population,  negligible area, and lots of food vendors, just stick it in any zip with the same UHF
food$zip[food$zip=="10121"]="10001" #madison square garden is in a simliar situation to rockefeller center
food$zip[food$zip=="10119"]="10001" #ditto penn station, these examples raise odd questions about accesiblity, they get lots of business, but its probably mostly commuters right?
food$zip[food$zip=="10281"]="10004" #ditto world financial center
#want to rebin this the areas by uhf instead of by zip code, since some zip codes are so small, they dont seem meaningful, at least 1 had a population of zero (rockefellar center), but had lots of businesses. 
food=left_join(food,uhf_convert,by="zip")
food=filter(food,zip!=11371)#this isn't in a UHF neighborhood, and for good reason, its la guardia airport, so I exclude it
food=filter(food,zip!=11040)#outside the boundaries of UHF zones

#View(food[which(!complete.cases(food)),]) #for debug, after filters we have 48 records that still dont have UHF encoded properly, but none of these belong to a large group like "penn station" above
#View(food[which(complete.cases(food)),]) #for debug, after filters we have 17,178 restaurants
#Since the # of inccorectly coded restaurants is small compared to the total, and seems to be distributed in both area and cuisine type, i'm just going to throw them out, should only affect indivual counts in each UHF by 0 to 2

food=food[which(complete.cases(food)),]

food_count=summarise(group_by(food,cuisine,uhf42),num_count=n())
#food_count=spread(food_count, cuisine, num_count)#reshape
food_count=dcast(food_count, uhf42~cuisine,value.var="num_count")
food_count[is.na(food_count)]=0# replace NA with zero (no counts for that cuisine in that UHF)

#need to calculate areas for each UHF so I can scale my counts to be more comprable, can check totals by borough
#extent of bronx, brooklyn and queens make it hard to compare, since UHFs don't neccesarily go all the way to boarders, and may exclude purely commerical zips
#manhattan is pretty accurate, even with the ommited zip codes that were purely commerical. Staten island is harder, because its boundardies include area over water.


uhf_area=summarise(group_by(uhf_convert,uhf42),area=sum(area))



#Now the data is mostly ready to be put together for analysis, which will be done in the app



ui <- dashboardPage(
    dashboardHeader(title = "Health Indicators and Geographic Factors", titleWidth = 450),
    dashboardSidebar( tags$style(HTML("
      .main-sidebar{
        width: 200px;
      }
    ")),
        sidebarUserPanel("Christopher Redino"),
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Distribution Full", tabName = "distro_full", icon = icon("bar-chart")),
            menuItem("Distributions", tabName = "distro", icon = icon("bar-chart")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("Reference", tabName = "ref", icon = icon("book")),
            menuSubItem(icon = NULL,
                        checkboxGroupInput("sample", 
                                           label = h1("Sample"), 
                                           choices = list("Males" = 1, "Females" = 2, 
                                                          "Ages 18-24"=3,"Ages 25-44"=4,"Ages 45-65"=5,"Ages 65+"=6,
                                                          "White" = 7,"Black"=8,"Hispanic"=9,"Asian"=10,"Other Races"=11),
                                           selected = list(1,2,3,4,5,6,7,8,9,10,11))
                         ),
            menuSubItem(icon = NULL,
                        checkboxGroupInput("pred", 
                                           label = h1("Predictors"), 
                                           choices = list("Fast Food" = 1, "Pizza" = 2, "Hamburgers" = 3,
                                                          "Vegetarian"=4,"Chinese"=5,"Mexican"=6,
                                                          "Donuts"=7,"Ice Cream"=8,"Italian"=9,
                                                          "Indian"=10,"Hotdogs"=11,"Bakery"=12),
                                           selected = list(1,2,3,4,5,6,7,8,9,10,11,12))
                       ),
            menuSubItem(icon = NULL,
                        radioButtons("scaling", label = h3("Predictor Scaling"),
                                     choices = list("None" = 1, "Scale by Area" = 2), 
                                     selected = 1)
            )
        )
        ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(
                        
                      leafletOutput("measured_map"),
                      leafletOutput("predicted_map"),
                      p(),
                      fluidRow(column(3, textOutput("value1")))

                    )
            ),
            tabItem(tabName = "distro_full",
                    fluidRow(
                      plotOutput('plot1')
                      
                    )
            ),
            tabItem(tabName = "distro",
                    fluidRow(
                      plotOutput('plot2')
                      
                    )
            ),
            
            tabItem(tabName = "data",
                    fluidRow(
                        htmlOutput("table"), width = 50
                    )
            ),
            tabItem(tabName = "ref",
                    fluidRow(
                      imageOutput("image")
                      
                    )
            )
        )
    )
)

server <- function(input, output) {
  #first run operations which must  be done everytime the input is changed
  health_data=health_zip #make a copy of the  health data, so that we can start with the original each time the buttons are applied

  
#have one very bigv reactive function. Maybe its just a bad consequence of how I designed my code,
  #but it seems as though I neccesarily have to a bunch of filtering/reshaping AFTER getting input. Use of an action button might be a big improvement here. . .
  big_reactive= reactive({
        
     
  
    #do filters based on input check boxes
  sample=input$sample
  
  #Gender Selection 1male, 2female
  if("1" %in% sample==FALSE){
  health_data=health_data[!(health_data$sex==1),]
  }
  if("2" %in% sample==FALSE){
    health_data=health_data[!(health_data$sex==2),]
  }
  #age 1(18-24),2(25-44),3(45-65),4(65+)
  if("3" %in% sample==FALSE){
    health_data=health_data[!(health_data$age==1),]
  }
  if("4" %in% sample==FALSE){
    health_data=health_data[!(health_data$age==2),]
  }
  if("5" %in% sample==FALSE){
    health_data=health_data[!(health_data$age==3),]
  }
  if("6" %in% sample==FALSE){
    health_data=health_data[!(health_data$age==4),]
  }

  #Race Selection,1white,2black,3hispanic,4asian,5other
  if("7" %in% sample==FALSE){
    health_data=health_data[!(health_data$race==1),]
  }
  
  if("8" %in% sample==FALSE){
    health_data=health_data[!(health_data$race==2),]
  }
  
  if("9" %in% sample==FALSE){
    health_data=health_data[!(health_data$race==3),]
  }
  
  if("10" %in% sample==FALSE){
    health_data=health_data[!(health_data$race==4),]
  }
  
  if("11" %in% sample==FALSE){
    health_data=health_data[!(health_data$race==5),]
  }
 study_size=length(health_data[,1])#want to report this later
  
  #calculate  % obesity in each UHF
  obese_rate1=summarise(group_by(health_data,uhf42),total_obese=sum(obese))
  obese_rate2=summarise(group_by(health_data,uhf42),total=n())
  obese_rate=left_join(obese_rate1,obese_rate2,by="uhf42")
  obese_rate$rate=obese_rate$total_obese/obese_rate$total
  rate_data=obese_rate#make a copy now, because the data in this form will be needed elsewhere
  #now need to map these onto zip codes for plotting, each zip in the same UHF will get an identical rate
  obese_rate=left_join(uhf_convert,obese_rate,by="uhf42")
  obese_rate$GEOID10=obese_rate$zip
  obese_rate=select(obese_rate,GEOID10,rate)
  
  #color right map, do this by zip codes, (will need to rejoin frame)but since all zips in a uhf will be colored the same it should work anyways
  subdat=readOGR(dsn=path.expand("nyc_zip_mapping"),layer="nyc_zip_data")#read in shape data for mapping, already reduced to NYC from whole NYS in prepare_data.R
  
  
  #now combine mapping data frame with the obesity info
  obese_rate=left_join(as.data.frame(subdat),obese_rate,by="GEOID10")
  row.names(obese_rate)= 0:(nrow(obese_rate)-1)
  #at this point can fill in those zips that were missed due to splitting(done manually be inspection, don't know of a better way to automate this)
  obese_rate$rate[70]=obese_rate$rate[50]
  obese_rate$rate[124]=obese_rate$rate[163]
  obese_rate$rate[103]=obese_rate$rate[50]
  obese_rate$rate[23]=obese_rate$rate[49]
  obese_rate$rate[121]=obese_rate$rate[111]
  #I can do the rest later, I got the big obvious ones that can be seen zoomed out. . .
  
  subdat2=spCbind(subdat,obese_rate)
  
  ####check if we should rescale by area of UHF
  scaling=input$scaling
  
  if (scaling =="2"){
    food_count=left_join(food_count,uhf_area,by="uhf42")
    for(i in 2:80){
      food_count[,i]=food_count[,i]/food_count[,81]
    }
  }
  
  #make a copy so we can start with the original each time the buttons are applied
  food_data=select(food_count, uhf42,Fastfood,Pizza,Hamburgers,Vegetarian,Chinese,Mexican,Donuts,IceCream,Italian,Indian,Hotdogs,Bakery)#This is enough categories to start with . . . 
  #also filter the food types based on input
  
  pred=input$pred
  
  if("1" %in% pred==FALSE){
    food_data=select(food_data,-Fastfood)
  }
  if("2" %in% pred==FALSE){
    food_data=select(food_data,-Pizza)
  }
  if("3" %in% pred==FALSE){
    food_data=select(food_data,-Hamburgers)
  }
  if("4" %in% pred==FALSE){
    food_data=select(food_data,-Vegetarian)
  }
  if("5" %in% pred==FALSE){
    food_data=select(food_data,-Chinese)
  }
  if("6" %in%  pred==FALSE){
    food_data=select(food_data,-Mexican)
  }
  if("7" %in% pred==FALSE){
    food_data=select(food_data,-Donuts)
  }
  if("8" %in% pred==FALSE){
    food_data=select(food_data,-IceCream)
  }
  if("9" %in%  pred ==FALSE){
    food_data=select(food_data,-Italian)
  }
  if("10" %in% pred ==FALSE){
    food_data=select(food_data,-Hotdogs)
  }
  if("11" %in%  pred==FALSE){
    food_data=select(food_data,-Bakery)
  }
  if("12" %in%  pred==FALSE){
    food_data=select(food_data,-Indian)
  }
  #do the regression
  model_data=left_join(select(rate_data,rate,uhf42),food_data,by="uhf42")
  model_data=select(model_data,-uhf42)
  food_model = lm(rate ~ ., data = model_data)#build the model with all the predictors that haven't been toggled out
  food_fit=summary(food_model)$adj.r.squared#save the R^2 for later
  
  #color the left map based on predictions from the model
  #first get predictions for the data set (this are the just the points on the regression surface)
  food_data_out=food_data#make a copy
  food_data_out$rate=predict(food_model, food_data)
  food_data_out=select(food_data_out,uhf42,rate)
  
  ##now the predicted data is in the same format as the measured bmi, so i can use the same method of plotting as before
  food_data_out=left_join(uhf_convert,food_data_out,by="uhf42")
  food_data_out$GEOID10=food_data_out$zip
  food_data_out=select(food_data_out,GEOID10,rate)
  food_data_out=left_join(as.data.frame(subdat),food_data_out,by="GEOID10")
  row.names(food_data_out)= 0:(nrow(food_data_out)-1)
  
  subdat3=spCbind(subdat,food_data_out)
  
  #display combined data table in own pane
  obese_rate$measured_obesity_rate=obese_rate$rate
  food_data_out$predicted_obesity_rate=food_data_out$rate
  
  data_table=left_join(obese_rate,food_data_out,by="GEOID10")
  data_table$zip=data_table$GEOID10
  data_table=left_join(data_table,uhf_convert,by="zip")
  data_table=select(data_table,uhf,zip,measured_obesity_rate,predicted_obesity_rate,uhf42)
  data_table=left_join(data_table,food_data,by="uhf42")
  data_table=select(data_table,-uhf42)
  #remove NAs
  data_table=data_table[which(complete.cases(data_table)),]
  #order by UHF
  data_table=arrange(data_table,uhf)
 
  
  health_data2=left_join(health_data,uhf_convert,by="uhf42")#make yet another copy before i mess with types
  health_data2$obese=as.factor(health_data2$obese)#want obese as a factor temporarily
  health_data2$uhf=as.factor(health_data2$uhf)#this is for my histograms
  
  
  binpal =colorBin("Reds", seq(0.05,0.5,0.001), 42, pretty = FALSE)#42 colors for 42 UHF regions
  legend_pal=colorBin("Reds", seq(0.05,0.5,0.001), 10, pretty = FALSE)#10 colors is enough for the legend (otherwise its too big)
  
  # now that all the reshaping is done I need to collect all the objets I that I will pass forward
  #this will just be a list with all the info I need going forward:
  #two sets of map data, both palletes, distribution data, and a table summarising the final data used, also sample size and R^2
  list(subdat2,subdat3,binpal,legend_pal,health_data2,data_table,study_size,food_fit)
  
  
  ######################
  })#end of the big reactive statement, all this joins will be done everytime a button is pressed, so i expect it to be slow . . .
    #should use an action button instead . . . 
  
  #####output begins#########
    
  output$measured_map=  renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("MapQuestOpen.Aerial")%>%
      addPolygons(data=big_reactive()[[1]],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,color = ~big_reactive()[[3]](rate))%>%
      addLegend("topleft", pal = big_reactive()[[4]],values = c(0.07,0.4),  title = "Obesity Rate Measured",opacity = 1)
  })
  output$predicted_map=  renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("MapQuestOpen.Aerial")%>%
      addPolygons(data=big_reactive()[[2]],stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,color = ~big_reactive()[[3]](rate))%>%
      addLegend("topleft", pal = big_reactive()[[4]],values = c(0.07,0.4),  title = "Obesity Rate Predicted",opacity = 1)
  })

  output$value1 <- renderText({studysize=paste("Size of study:", big_reactive()[[7]])
  noquote(studysize)} )#Return the size of the sample of the Study
  output$plot1 <- renderPlot({
    my_distro_data=big_reactive()[[5]]
    qplot(bmi, data = my_distro_data,fill=obese)
  },height=800)
  output$plot2 <- renderPlot({
    my_distro_data=big_reactive()[[5]]
    qplot(bmi, data = my_distro_data,fill=obese)+facet_wrap(~uhf)
  },height=800)
    output$table <- renderGvis({
      my_data_table=big_reactive()[[6]]
        gvisTable(my_data_table,
                  options=list(page='disable'))
    })
    output$image <- renderImage({
        return(list(
          src = "images/uhf42.png",
          contentType = "image/png",
          alt = "UHF42ref"
        ))
      
      
    }, deleteFile = FALSE)
}

shinyApp(ui, server)

