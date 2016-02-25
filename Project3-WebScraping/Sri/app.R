library(shinydashboard)
library(googleVis)
library(lubridate)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)


load("data.RData")


# candidates_final <- read.csv("candidates_final.csv")
# cand_full <- read.csv("cand_full.csv")
# candidates_full <- read.csv("candidates_full.csv")
# avg_sent_final <- read.csv("avg_sent_final.csv")
# cand_full_sent <- read.csv("cand_full_sent.csv")
# trump_analysis <- read.csv("trump_analysis.csv", stringsAsFactors = F)
# cruz_analysis <- read.csv("cruz_analysis.csv", stringsAsFactors = F)
# rubio_analysis <- read.csv("rubio_analysis.csv", stringsAsFactors = F)
# jeb_analysis <- read.csv("jeb_analysis.csv", stringsAsFactors = F)
# hilary_analysis <- read.csv("hilary_analysis.csv", stringsAsFactors = F)
# bernie_analysis <- read.csv("bernie_analysis.csv", stringsAsFactors = F)
# trump_most <- read.csv("trump_most.csv", stringsAsFactors = F)
# cruz_most <- read.csv("cruz_most.csv", stringsAsFactors = F)
# rubio_most <- read.csv("rubio_most.csv", stringsAsFactors = F)
# jeb_most <- read.csv("jeb_most.csv", stringsAsFactors = F)
# hilary_most <- read.csv("hilary_most.csv", stringsAsFactors = F)
# bernie_most <- read.csv("bernie_most.csv", stringsAsFactors = F)
# cand_analysis <- read.csv("cand_analysis.csv", stringsAsFactors = F)


ui <- dashboardPage(
    dashboardHeader(title = "Sricharan Maddineni"),
    dashboardSidebar(
        sidebarUserPanel(name = "", image = "http://www.univ-orleans.fr/sites/default/files/Universit%C3%A9/images/twitter_circle_color-512.png"),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Exploratory", icon = icon("magic"), 
                     menuSubItem("Explore", tabName = "explore"),
                     menuSubItem("Data", tabName = "table")),
            menuItem("Time-Series", tabName = "best", icon = icon("area-chart")),
            menuItem("Sentiment", tabName="sent", icon = icon("smile-o")),
            menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
            menuItem("Notes", tabName = "notes", icon = icon("book"))
        ),
        br(),
        selectizeInput("selected", "Select Candidate(s)",
                       choices = list('Donald Trump'='trump', 'Ted Cruz'='cruz', 'Marco Rubio'='rubio', 
                                      'Jeb Bush'='jeb', 'Hillary Clinton'='hilary', 'Bernie Sanders'='bernie'), selected = "trump", multiple=TRUE)
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "wordcloud",
                    uiOutput("all")
            ),
            tabItem(tabName = "home",
                    div(style = 'overflow-y: scroll',
                    h2("2016 Presidential Candidates' Twitter Engagement Analysis"),
                    p("Twitter is a window into the world; anything happening across the world, is happening first on Twitter.
                      Using Twitter has become a must for presidential candidates to get their message out and 
                        their tweets reflect their personality, interests, and strategy. Analyzing their social 
                        presence leads to some interesting insights."),
                    p("Using Twitter's API, I've scrapped the last 3,200 tweets from each of the top 6
                      presidential candidates, and explored differences in engagement and sentiment. 
                      If you're unfamiliar with Twitter, you can use the widgets below to view their Twitter accounts (left),
                        and see the live firehose of tweets from all candidates (right)."),
                    br(),
                    column(6, includeHTML("twitter.html")),
                    column(6, includeHTML("twitter_l.html"))
                    )
            ),
            tabItem(tabName = "explore",
                    fluidRow(box(htmlOutput("bar"), width = 12, status = "info", solidHeader = F, title="Average Retweets vs Favorites vs Sentiment Score")),
                    fluidRow(box(htmlOutput("bar2"), width = 12, status = "info", solidHeader = F, title="Average Number of tweets per Day vs Sentiment Score")),
                    fluidRow(box(htmlOutput("cands"), width = 12, status = "info", solidHeader = F, title = "Total Retweets by Time of Day"))
            ),
            tabItem(tabName = "table",
                    fluidRow(box(DT::dataTableOutput("tablea"), width = 12, margin =2, status = "info", solidHeader = T))
            ),
            tabItem(tabName = "best",
                    box(h2("Engagement Analysis"),
                    h4("Whats being plotted: the most retweeted tweet per day."),
                    h5(""),
                    h4("Click on a date to view each candidates tweet and sentiment score on that day."),
                    fluidRow(htmlOutput("chart"), width = 12, margin = 2, solidHeader = T), width = 2000, status = "info"),
                    fluidRow(valueBoxOutput("date"), width = 4, margin = 2, 
                             valueBoxOutput("tot_rts"), width = 4, margin = 2,
                             valueBoxOutput("avg_sent"), width = 4, margin = 2),
                    h3("What They Tweeted"),                  
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Donald Trump", htmlOutput("trump"))),
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Ted Cruz", htmlOutput("cruz"))),
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Marco Rubio", htmlOutput("rubio"))),
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Jeb Bush", htmlOutput("jeb"))),
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Hillary Clinton", htmlOutput("hilary"))),
                    fluidRow(box(width = 12, status = "info", solidHeader = F,
                                 title = "Bernie Sanders", htmlOutput("bernie")))
            ),
            tabItem(tabName = "sent",
                    h2("How does Sentiment Score affect Retweets?"),
                    fluidRow(box(width = 12, plotOutput('box', width=1000))),
                    br(),
                    fluidRow(box(width = 12, plotOutput('box2', width=1000)))
            ),
            tabItem(tabName= "notes",
                    h2("Scraping"),
                    p("I used the Tweepy library to build a scraper that takes any Twitter handle (ex. @sriyoda), 
                      and extracts the last 3200 tweets (Twitter API call limit). It gathers the date and time, tweet content, 
                      tweet image (if used), rewteet count, and favorite count."),
                    pre(includeText("calls.txt")),
                    h2("Sentiment Analysis"),
                    p("I utilized the Opinion-Lexicon-English dictionary which contains positive and negative words lists. 
                      I assigned a (+1) value to positive words and a (-1) value to negative words in a tweet. 
                      Then I subtracted the negative word count from the positive word count to obtain a basic sentiment
                      analysis for each tweet (maybe more accurately, a vocabulary analysis). I plan on using a more robust sentiment API 
                      in the future to perform a better sentiment analysis."),
                    column(6, pre(includeText("pos.txt"))),
                    column(6, pre(includeText("neg.txt"))),
                    h2(""),
                    h4("Citations:"),
                    includeHTML("bib.html")
            )
        )
), skin = "black")


server <- function(input, output) {

    new_exp <- reactive({
        expl <- filter(avg_sent_final[,2:5],candidate %in% input$selected)
        expl
    })
    
    output$bar <- renderGvis({
        expla <- new_exp()
        gvisBarChart(expla,options=list(titleTextStyle="{color:'Black',fontSize:16}", height=300, width=1000))
    })
    
    new_exp_b <- reactive({
        explb <- filter(cand_full_sent[,2:4],candidate %in% input$selected)
        explb
    })
    
    output$bar2 <- renderGvis({
        explbb <- new_exp_b()
        gvisBarChart(explbb,options=list(titleTextStyle="{color:'Black',fontSize:16}", height=300, width=1000))
    })
    
    new_data <- reactive({
        df <- filter(candidates_final,variable %in% input$selected)
        df
    })
    
    output$chart <- renderGvis({
        dataaa <- new_data()
        gvisAnnotationChart(dataaa, datevar = "date", numvar = "value", idvar="variable",
                            options=list(colors="['#ff0000', '#0041C2', '#4CC417', '#2C3539', '#0041C2', '#842DCE', '#00e600']",
                                         width='auto', height=500, width=5000, lineWidth=0, fill=50,
                                         gvis.listener.jscode = "var selected_date = data.getValue(chart.getSelection()[0].row,0);var parsed_date = selected_date.getFullYear()+'-'+(selected_date.getMonth()+1)+'-'+selected_date.getDate();
                                         Shiny.onInputChange('selected_date',parsed_date)"))
    })

    new_databbb <- reactive({
        dfb <- filter(cand_full, candidate %in% input$selected) %>% group_by(time_t) %>% summarise(sum(retweets))
        dfb
    })
    
    output$cands <- renderGvis({
        datbbb <- new_databbb()
        gvisLineChart(datbbb, xvar = "time_t", yvar="sum(retweets)", options=list(width=1000,
                                                                                  height=300))
    })
    
    new_dataccc <- reactive({
        dfc <- filter(cand_full, candidate %in% input$selected)
    })
    
    output$tablea <- DT::renderDataTable(DT::datatable(
        cand_full[,c(3,4,5,6,7,9)])
    )

    observe(
        if (!is.null(input$selected_date)) {
            output$date <- renderValueBox({
                valueBox(
                    value = input$selected_date,
                    subtitle = "Selected Date",
                    icon = icon("eye"))
            })
            output$avg_sent <- renderValueBox({
                datex <- as.character(as.Date(input$selected_date))
                valueBox(
                    value = candidates_full %>% filter(date == datex) %>% summarise(n()),
                    subtitle = "Number of Tweets (all candidates)",
                    icon = icon("calculator"),
                    color = "maroon")
            })
            output$tot_rts <- renderValueBox({
                date_rts <- as.character(as.Date(input$selected_date))
                valueBox(
                    value = candidates_final %>% filter(date == date_rts) %>% summarise(sum(value)),
                    subtitle = "Total Retweets (all candidates)",
                    icon = icon("calculator"),
                    color = "purple")
            })
            output$trump <- renderUI({
                date1 <- as.Date(input$selected_date)
                data <- trump_most %>% filter(date == date1)
                sent <- trump_analysis %>% filter(text == data$text)
                HTML(paste(data$text, paste("Time of Day:", data$time, sep=" "), paste("Sentiment Score:", sent$score, sep=" "), sep = '<br/>'))
            })
            output$cruz <- renderUI({
                date1 <- as.Date(input$selected_date)
                data2 <- cruz_most %>% filter(date == date1)
                sent2 <- cruz_analysis %>% filter(text == data2$text)
                HTML(paste(data2$text, paste("Time of Day:", data2$time, sep=" "), paste("Sentiment Score:", sent2$score, sep=" "), sep = '<br/>'))
            })
            output$rubio <- renderText({
                date1 <- as.Date(input$selected_date)
                data3 <- rubio_most %>% filter(date == date1)
                sent3 <- rubio_analysis %>% filter(text == data3$text)
                HTML(paste(data3$text, paste("Time of Day:", data3$time, sep=" "), paste("Sentiment Score:", sent3$score, sep=" "), sep = '<br/>'))
            })
            output$jeb <- renderText({
                date1 <- as.Date(input$selected_date)
                data4 <- jeb_most %>% filter(date == date1)
                sent4 <- jeb_analysis %>% filter(text == data4$text)
                HTML(paste(data4$text, paste("Time of Day:", data4$time, sep=" "), paste("Sentiment Score:", sent4$score, sep=" "), sep = '<br/>'))
            })
            output$hilary <- renderText({
                date1 <- as.Date(input$selected_date)
                data5 <- hilary_most %>% filter(date == date1)
                sent5 <- hilary_analysis %>% filter(text == data5$text)
                HTML(paste(data5$text, paste("Time of Day:", data5$time, sep=" "), paste("Sentiment Score:", sent5$score, sep=" "), sep = '<br/>'))
            })
            output$bernie <- renderText({
                date1 <- as.Date(input$selected_date)
                data6 <- bernie_most %>% filter(date == date1) 
                sent6 <- bernie_analysis %>% filter(text == data6$text)
                HTML(paste(data6$text, paste("Time of Day:", data6$time, sep=" "), paste("Sentiment Score:", sent6$score, sep=" "), sep = '<br/>'))
            })
    })
    
    new_word <- reactive({
        dfw <- input$selected
        dfw
    })

    output$all <- renderUI({
        new_word <- new_word()
        plot <- list(trump = plotOutput("trump_w"),
                     cruz = plotOutput("cruz_w"),
                     rubio = plotOutput("rubio_w"),
                     jeb = plotOutput("jeb_w"),
                     hilary = plotOutput("hilary_w"),
                     bernie = plotOutput("bernie_w"))
        plot[new_word]
    })
    
    output$trump_w <- renderPlot({
        set.seed(1)
        wordcloud(names(v), v, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    output$cruz_w <- renderPlot({
        set.seed(3)
        wordcloud(names(v1), v1, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    output$rubio_w <- renderPlot({
        set.seed(5)
        wordcloud(names(v2), v2, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    output$jeb_w <- renderPlot({
        set.seed(5)
        wordcloud(names(v3), v3, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    output$hilary_w <- renderPlot({
        set.seed(3)
        wordcloud(names(v4), v4, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    output$bernie_w <- renderPlot({
        set.seed(5)
        wordcloud(names(v5), v5, colors=c(1,2,3,4,5,6), random.color=FALSE, min.freq = 60) 
    })
    
    new_dat_box <- reactive({
        dfg <- filter(cand_analysis, X %in% input$selected)
        dfg
    })
    
    output$box <- renderPlot({
        datggg <- new_dat_box()
        ggplot(datggg, aes(x=as.factor(score), y=retweets, fill=X)) + 
            geom_boxplot(outlier.shape = NA, colour='blue') + 
            scale_y_log10() + theme_bw()
    })
    
    new_dat_box_b <- reactive({
        dff <- filter(cand_analysis, X %in% input$selected)
        dff
    })
    
    output$box2 <- renderPlot({
        datfff <- new_dat_box_b()
        ggplot(data = datfff, aes(x=score, y=retweets)) +
            geom_point(stat = "identity", position = "identity", na.rm = FALSE, color="blue") + 
            scale_y_log10() + geom_smooth(color="red") + theme_bw()
    })
}

shinyApp(ui, server)
