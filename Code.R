
#-----Question 1 and Qeustion 2.. Data Cleaning and Merging---------------------------------------------------------

#loading the data files into R

library(sqldf)
library(plotly)
library(ggplot2)

Loans<- read.csv("2012_to_2014_loans_data.csv",header = TRUE,stringsAsFactors=FALSE)
Institutions<-read.csv("2012_to_2014_institutions_data.csv",header = TRUE,stringsAsFactors=FALSE)


#Removing NA from Respondent_ID in Institutions and Loans
Institutions<-Institutions[complete.cases(Institutions[,2]),]
Loans<-Loans[complete.cases(Loans[,10]),]

#Removing loan amounts that are Jumbo
View(Loans[which(Loans$Conforming_Status=='Jumbo'),])
Loans<-subset(Loans, Loans$Conforming_Status!='Jumbo')


#Removing - from Respondent_ID in Institutions
Loans$Respondent_ID<-gsub("-", "", Loans$Respondent_ID)
Institutions$Respondent_ID<-gsub("- ", "", Institutions$Respondent_ID)

# Adding New Column for Respondent Nnme in Laons Data Frame
Loans$ResName<-Institutions[match(Loans$Respondent_ID,Institutions$Respondent_ID),4]
Loans<-Loans[complete.cases(Loans[,25]),]

# Adding New Column for Loan type as High Medium or low

Loans$LoanCategory<- ifelse(Loans$Loan_Amount_000>=500, "High",
                            ifelse(Loans$Loan_Amount_000>=300 & Loans$Loan_Amount_000<499 , "Medium",
                                   ifelse(Loans$Loan_Amount_000<=299, "Low",
                                          
                                                 NA  )))

Loans<-Loans[complete.cases(Loans[,26]),]
  


#Some general commands
View(Loans[which(Loans$Loan_Amount_000 >500),])
View(Loans[which(Loans$Conforming_Status=='Jumbo'),])
View(Loans[which(Loans$Loan_Type_Description!='Conventional'),])

View(Institutions)
View(Loans)
str(Loans)
str(Institutions)
any(is.na(Institutions$Respondent_ID))



# -------------------------------------VISUALIZATION for QUESTION 3#------------------------------------
#--------------------------------------------------------------------------------------
#PIE CHART FOR MARKET SHARE BY STATE
library(plotly )
set.seed(100)
library(sqldf)


# use SQL function to query Loan amount disbursed by state
df<-sqldf("select Sum(Loan_Amount_000) as total_loan,State  from Loans group by State" )
View(df)


# Plotting the pie chart
 plot_ly(df, labels = ~State, values = ~total_loan, type = 'pie') %>%
  layout(title = 'Loan disrbursed by state',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



 #-----------------------------------------------------------------------------------------
 
 #BAR CHART FOR MARKET SHARE BY YEAR
 
 df1<- sqldf("select Sum(Loan_Amount_000) as total_loan,As_of_year from Loans group by As_of_Year" )
 View(df1)
 
# plotting bar chart 
 plot_ly( df1,
   x = ~As_of_Year,
   y = ~total_loan,
   name = "Market Share By Year",
   type = "bar"
   
 )
 #----------------------------------------------------------------------------------------------
 
 #STACKED BAR CHART FOR MARKET SHARE BY YEAR
 df2<- sqldf("select Sum(Loan_Amount_000) as total_loan,As_of_year,State from Loans  group by As_of_Year,State" )
 View(df2)
 
 #plotting the stacked bar chart
 ggplot(data = df2, aes(x = As_of_Year, y = total_loan/1000, fill = State)) + 
   geom_bar(stat = "identity")

 
 
 #---------------------------------------------------------------Question4 -----------------------
 
 #creating the market share data frame. This will be saved as a CSV to be later used for creating the  Shiny dashboard
 #
 
 
 marketshare<- sqldf("select Sum(Loan_Amount_000) as total_loan,State,ResName,As_of_Year 
                     from Loans  
                     group by ResName,State,As_of_Year 
                     order by total_loan desc" )

 View(marketshare)
 
 #save as csv
 write.csv(marketshare, file = "C:\\Users\\rajath\\Desktop\\capitalone data challenge\\Final\\apps\ms.csv", row.names = FALSE)
 

 
 # Create a new column with risk status-------------------------------------------------- 
 
 
 Loans$LoanRisk<- ifelse(Loans$Lien_Status_Description=="First Lien", "Low Risk",
                         ifelse(Loans$Lien_Status_Description=="Subordinate Lien", "High Risk",
                                
                                
                                NA  ))
 
 
 #----creating 2 new darta frames for low risk and high risk
 
 lowrisk<- sqldf('select State, count(LoanRisk) as LowRisk_Count from Loans where LoanRisk = "Low Risk"
              group by State')
 View(lowrisk)
               
 highrisk<-sqldf('select State, count(LoanRisk)  as HighRisk_Count from Loans where LoanRisk = "High Risk"
              group by State')
                         
 View(highrisk)
 
 #---------- merging the low risk and highrisk data frame
totalrisk<-sqldf('select lr.State, LowRisk_Count, HighRisk_Count 
                 from lowrisk lr join highrisk hr on lr.State=hr.State
                 
                 
                 ')


# adding a new column to the dataframe to calcualte the percentage of high risk
totalrisk$Percentage_Highrisk <-((totalrisk$HighRisk_Count/(totalrisk$HighRisk_Count+totalrisk$LowRisk_Count))*100)
View(totalrisk)

# plottign the data
plot_ly(totalrisk,x=~State,y=~Percentage_Highrisk,
        name = "Risk for the State",
        type = "bar")

#---------------------------------Loan category by state---------------------------------------------------------------


#---Loan category by state for Low category

category_of_loan_low<- sqldf('select State, count(LoanCategory) as Low_Count from Loans where LoanCategory = "Low"
              group by State')

View(category_of_loan_low)
#---Loan category by state for Medium category

category_of_loan_medium<- sqldf('select State, count(LoanCategory) as Medium_Count from Loans where LoanCategory = "Medium"
              group by State')

View(category_of_loan_medium)

#---Loan category by state for High category


category_of_loan_high<- sqldf('select State, count(LoanCategory) as High_Count from Loans where LoanCategory = "High"
              group by State')

View(category_of_loan_high)


#---Loan category for all categories by state
totalcategory<-sqldf('select l.State, Low_Count, Medium_Count,High_Count
                 from category_of_loan_low l left join category_of_loan_medium m on l.State=m.State
                left join category_of_loan_high h on l.State=h.State
                 
                 
                 ')
View(totalcategory)

#replacing null with 0 for calcualting purposes
require(tidyr)
totalcategory$High_Count [is.na(totalcategory$High_Count)]<-0

# addign new column to calculate percentage of low loans by state
totalcategory$Percentage_Low <-((totalcategory$Low_Count/(totalcategory$Low_Count+totalcategory$Medium_Count+totalcategory$High_Count))*100)
View(totalcategory)


# plottig the data
plot_ly(totalcategory,x=~State,y=~Percentage_Low,
        name = "Low category percentage for the State",
        type = 'scatter', mode = 'lines')



# creating new data frame by combinging the totalcategory and total risk data frame
totalrisk_and_category<- sqldf('select * from totalcategory tc join totalrisk tr on tc.State=tr.State ')
View(totalrisk_and_category)

#plotting the data

ggplot(data = totalrisk_and_category, aes(x = State, y = Percentage_Low, fill = Percentage_Highrisk)) + 
  geom_bar(stat = "identity")
