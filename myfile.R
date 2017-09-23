
#function to fetch the initial data

hmda_init<<-function(read){
  
if (read==1){
 
  library(sqldf)
  library(plotly)
  library(ggplot2)
  
   Loans<- read.csv("2012_to_2014_loans_data.csv",header = TRUE,stringsAsFactors=FALSE)
  Institutions<-read.csv("2012_to_2014_institutions_data.csv",header = TRUE,stringsAsFactors=FALSE)
  
  
  #Removing NA from Respondent_ID in Institutions
  Institutions<-Institutions[complete.cases(Institutions[,2]),]
  Loans<-Loans[complete.cases(Loans[,10]),]
  
  #Removing loan amounts that are Jumbo
  View(Loans[which(Loans$Conforming_Status=='Jumbo'),])
  Loans<-subset(Loans, Loans$Conforming_Status!='Jumbo')
  
  Loans<-Loans[complete.cases(Loans[,26]),]
  
  
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
  
  
  


}
else{
  print("Please enter 1 to extract the appropriate data")
}
}

#function to convert a file to json

hdma_to_json<<-function(x){
  library(jsonlite)
  y<<-toJSON(x)
  cat(y)
  write(y,file='x.json')
  return(y)
}









