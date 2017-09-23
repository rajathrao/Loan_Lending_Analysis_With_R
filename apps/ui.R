
library(shiny)
library(dplyr)
library(Hmisc)
library(sqldf)

library(plotly)
library(ggplot2)




marketsharenew<-read.csv('ms.csv',header = TRUE,stringsAsFactors=FALSE)
fluidPage(
 
  titlePanel("Market share of the competition"),
  sidebarLayout(
    sidebarPanel(
     
      selectInput("Resinput", "Respondent Name",
                  choices = marketsharenew$ResName),
     
      selectInput("Stateinput", "State Name",
                  choices = marketsharenew$State)
    ),
    mainPanel(
      
      plotOutput("coolplot"),br(),
      tableOutput("results")
    )
  )
)
