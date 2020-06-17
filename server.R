
# Import Libraries-------------------------
library(shiny)

library(data.table)
library(tidyr)
library('dplyr')
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


# Data Load--------------------------------
#Remove this business csv from app directory
#business_df <- read.csv("./business.csv") 

#Load from rds file
app_data <- readRDS("./shinyAppDF.rds")

#Make business df 
business_df <- app_data[,names(app_data) %in% c( "business_id","State","City","Address","Latitude","Longitude","Stars","Sub_category")]
business_df <- distinct(business_df)

#Get unique city
uniqueCity=sort(unique(business_df$City))
#Get unique state names
uniqueState=sort(unique(business_df$State))
#Get unique categories
UniqueType=sort(unique(business_df$Sub_category))

#busin1 = business_df[1:20,]
busin1 = business_df




# Server logic-----------------------------

server <- function(input,output,session) {
  
  #Populate state names
  output$selectState <- renderUI(
    selectInput(inputId = "State",
                label = "Choose a state:", choices= 
                  as.character(uniqueState),selected = 'Arizona') #,selected = 'OH'
  )
  
  #Populate city names based on state selected
  output$selectCity <- renderUI(
   if(is.null(input$State)) #TODO: Need to remove .. will create junk input
     selectInput(inputId ="city",label = "Choose a city:", choices= 
               as.character(uniqueCity),selected = 'Ahwatukee')
   else{
      # Get the data set with the appropriate name
      cityForState <- sort(unique(business_df[business_df$State == input$State, "City"]))
      selectInput(inputId ="city",label = "Choose a city:", choices= as.character(cityForState),selected = 'Ahwatukee')
  }
      
  )
  
  #Populate different restaurant types
  output$selectType <- renderUI(
    selectInput(inputId ="Type",label = "Choose a restuarant type:", choices= 
                  as.character(UniqueType),selected = 'Bakery') 
  )
  
  #Action for search
 observeEvent(input$searchButton, {
   
   
   #tempDF <- business_df %>% filter(city == input$city & Sub.category == input$Type)
   #For RDs file
   tempDF <- app_data %>% filter(City == input$city & Sub_category == input$Type)
   
   resultRowID <- c("Total number of resturants" ,"Average restaurant ratings","Total number of reviews ", "Number of positive reviews","Number of negative reviews")
   
   resultsDF <- data.frame(Summary=resultRowID)
   resultsDF["Count"] <- NA 
   
   if(is.data.frame(tempDF) && nrow(tempDF)==0){
     tempDF <- business_df
    
   }
   else{
    totalResturant = length(unique(tempDF$business_id))#nrow(unique(tempDF$business_id))
    avgRestaurantRatings =  unique(tempDF$Stars)#Recheck
    totalReviewCount =nrow(tempDF)
   
    #Call MOdel pending
    
    numberofPositiveReviews = as.integer(234)
    numberofNegativeReviews = as.integer(123)
    
    
    #resultContent = c(as.numeric(totalResturant) , avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
    resultContent = list(as.numeric(totalResturant) , avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
  
     
     resultsDF$Count=resultContent
   }
   
   output$resultTable <- renderTable(resultsDF)#renderDataTable(resultsDF)
   
   #output$summary <- renderUI({
     # summaryText <- paste("<b>Summary of current restaurants of type ",input$Type ,"</b>")
     
     # HTML(paste(summaryText, sep = '<br/>'))
     
     # })
   #output$summary <- renderUI({
   #  title <- paste("Summary of current restaurant of type ",input$Type)
    # str1 <- paste("Total ", NROW(tempDF), "restuarants are in this are of type ",input$Type )
    # str2 <- paste("Average rating of restuarants is <Pending from reviews csv>")
    # str3 <- paste("Total <Pending from reviews csv> reviews are available for the selected category.")
    # title <- paste("Prediction ")
    # HTML(paste(str1, str2,str3, sep = '<br/>'))
     
  # })
   output$mymap <- renderLeaflet({
   
     leaflet(tempDF) %>% addTiles() %>%
       addMarkers(~Longitude, ~Latitude,popup = paste(tempDF$name,"<br>",
                                                      tempDF$address))
     
   })
   
  # output$summary <- renderText("Summary")
   
   
  })
 
 
 #Action for reset button
 observeEvent(input$resetButton,{
   updateSelectInput(session,inputId = "State",selected = 'Arizona') #,selected = 'OH'
   updateSelectInput(session,inputId = "Type",selected = 'Bakery') #,selected = 'Bakery'
   #leaflet(busin1) %>% addTiles() %>% addMarkers(~longitude, ~latitude,popup = paste(busin1$name,"<br>", busin1$address))
   leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
 })
  
  #Display map
  output$mymap <- renderLeaflet({
   
      #leaflet(busin1) %>% addTiles()%>% addMarkers(~longitude, ~latitude,popup = paste(busin1$name,"<br>",busin1$address))
    leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
    
    
  })
 
}