library(shiny)
library(dplyr)
library(Hmisc)
library(sqldf)

library(plotly)
library(ggplot2)





shinyServer(function(input, output) {
  
  marketsharenew<-read.csv('ms.csv',header = TRUE,stringsAsFactors=FALSE)
  output$coolplot <- renderPlot({
    
    filtered <-
      marketsharenew%>%
      filter(ResName==input$Resinput,
             
             State==input$Stateinput
             
             
             
             
             
             
             
      )
    
    ggplot(data =filtered, aes(x = As_of_Year, y = total_loan,fill = State,shape = factor(As_of_Year))) + 
      geom_point(stat="identity", colour = "Blue", size = 6.5)+
      scale_fill_hue(c=45, l=80)+ labs(x="Year",y="Total Loan in $000's") 
    
    #plot_ly(filtered, x = ~As_of_Year, y = ~total_loan)
    
    
    
  })
  
  output$results<-renderTable({
    
    filtered <-
      marketsharenew %>%
      filter(ResName==input$Resinput,
             
             State==input$Stateinput
             
             
             
             
             
             
      )
    
    
  })
  
  
})

