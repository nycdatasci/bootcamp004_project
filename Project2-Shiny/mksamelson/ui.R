library(shiny)
library(dplyr)
library(reshape2)
library(gdata)
library(ggplot2)
source("support.R")

#Question Choices

choices <- c("High Touch Execution Quality" = "executing.high.touch.orders",
             "Ability to Locate Natural Liquidity" = "locating.natural.liquidity",
             "Understanding of Market Conditions on Specific orders" = "impact.of.market.conditions",
             "Understanding of Client Investment Strategies and Trading Needs" = "understand.of.your.firm",
             "Availability of Broker Capital" = "availability.of.broker.capital")

#Broker Choices for Particular Question Comparative Density Bar Charts


# Define UI for Broker Performance application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("US Equities - Broker Performance 2015"),
  
  # Sidebar with controls to select the Question to Select
  
  sidebarPanel(
    radioButtons("Chart", "Chart:", choices)
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("BarPlot"),uiOutput("comment")),
      tabPanel("Density", plotOutput("DensityPlot"), uiOutput("BrokerSelector")),
      tabPanel("Table")
    )
    )
))