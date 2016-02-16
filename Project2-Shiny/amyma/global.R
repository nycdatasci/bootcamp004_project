library(shiny)
library(shinydashboard)
library(googleVis)
library(datasets)
library(leaflet)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(dygraphs)
library(xts)

# load data
load("data/review.RData")
load("data/map.RData")
load("data/review_txt.RData")
load("data/host.RData")
#source("helpers.R")

insideairbnb <- tags$html(
  tags$body(
    a(href="http://insideairbnb.com/get-the-data.html"))
)
# variable list
neighbour<-c(
  "Bronx" = "Bronx",
  "Queens" = "Queens",
  "Brooklyn" = "Brooklyn",
  "Staten Island" = "Staten Island",
  "Manhattan"="Manhattan"
)
room<-c(
  "Entire home/apt"= "Entire home/apt",
  "Private room" ="Private room",
  "Shared room" ="Shared room"
)


choice=names(review)[-c(1:3)]

pal <- colorFactor(c("#EE3B3B", "#0000EE","#66CD00"), domain = c("Entire home/apt", "Private room","Shared room"))


