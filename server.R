
# Import Libraries-------------------------
library(shiny)

library(data.table)
library(tidyr)
library('dplyr')
library(e1071)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


# Data Load--------------------------------

print(Sys.time())
app_data <- read.csv("./Final_dataset.csv") # with this 33 sec

#Load from rds file
#app_data <- readRDS("./shinyAppDF.rds") #Need to create with new test data

#Make business df 
#business_df <- app_data[,names(app_data) %in% c( "business_id","State","City","Address","Latitude","Longitude","Stars","Sub_category")]
business_df <-app_data[,1:8]
business_df <- distinct(business_df)

#Get unique city
uniqueCity=sort(unique(business_df$City))
#Get unique state names
uniqueState=sort(unique(business_df$State))
#Get unique categories
UniqueType=sort(unique(business_df$Sub_category))
print(UniqueType)
#busin1 = business_df[1:20,]
busin1 = business_df


print(Sys.time())

# Server logic-----------------------------

server <- function(input,output,session) {
  
  #------------------Initial UI load : Start
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  observe({
    
  #Populate different restaurant types
    output$selectType <- renderUI(
      selectInput(inputId ="Type",label = "Choose a restuarant type:", choices= as.character(UniqueType)
                    ,selected = 'Bakery') 
    )
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
                    as.character(uniqueCity),selected = 'Tempe')
    else{
      # Get the data set with the appropriate name
      cityForState <- sort(unique(business_df[business_df$State == input$State, "City"]))
      selectInput(inputId ="city",label = "Choose a city:", choices= as.character(cityForState),selected = 'Tempe')
    }
    
  )
  
  
  
  #Display map
  output$mymap <- renderLeaflet({
    
    leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$name,"<br>",busin1$address))
    #leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
    
  
   
  })
  
  
  
  ## set my condition to TRUE
  rv$setupComplete <- TRUE
  ## the conditional panel reads this output
  output$setupComplete <- reactive({
   return(rv$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
  

  
  })
 
  
 
 
  #---------------------Initial UI load : End
  
  
  
  #---------------------Action for search : Start
 observeEvent(input$searchButton, {
   #-----Progress Bar
   withProgress(message = 'Fetching details', value = 0, {
     # Number of times we'll go through the loop
     n <- 10
     
     for (i in 1:n) {
       #For RDs file
       tempDF <- app_data %>% filter(City == input$city & Sub_category == input$Type)
       
       #Split DF in business 
       tempBusiness_df <-tempDF[,1:8]
       tempBusiness_df <- distinct(tempBusiness_df)
       
       
       resultRowID <- c("Total number of resturants" ,"Average restaurant ratings","Total number of reviews ", "Number of positive reviews","Number of negative reviews")
       
       resultsDF <- data.frame(Summary=resultRowID)
       resultsDF["Count"] <- NA 
       
       if(is.data.frame(tempDF) && nrow(tempDF)==0){
         tempDF <- business_df
         
       }
       else{
         totalResturant <- as.numeric(nrow(tempBusiness_df))#length(unique(tempDF$business_id))#nrow(unique(tempDF$business_id))
         #avgRestaurantRatings =  unique(tempDF$Stars)#Recheck
         avgRestaurantRatings = mean(tempDF$Stars) # Need to check
         totalReviewCount <- as.integer(nrow(tempDF))
         
         #Call MOdel 
         modelObject <- readRDS("./final_model_rds.rds")
         #Split DF to get reviews for prediction
         testDf <-tempDF[,9:42]
         result <- predict(modelObject,tempDF)
         #table(result)
         
         numberofPositiveReviews = as.integer(getElement(table(result), "positive"))#as.integer(234)
         numberofNegativeReviews = getElement(table(result), "negative")#as.integer(123)
         
         
         #resultContent = c(as.numeric(totalResturant) , avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
         resultContent = list(totalResturant, avgRestaurantRatings , totalReviewCount , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
         
         
         resultsDF$Count=resultContent
       }
       
       
       
       output$resultTable <- renderTable(resultsDF)#renderDataTable(resultsDF)
       
       output$predictionDetails <- renderUI({
         predictionText <- paste("<b>Prediction : </b>")
       
          HTML(paste(predictionText, sep = '<br/>'))
       
       })
       #output$summary <- renderUI({
       #  title <- paste("Summary of current restaurant of type ",input$Type)
       # str1 <- paste("Total ", NROW(tempDF), "restuarants are in this are of type ",input$Type )
       # str2 <- paste("Average rating of restuarants is <Pending from reviews csv>")
       # str3 <- paste("Total <Pending from reviews csv> reviews are available for the selected category.")
       # title <- paste("Prediction ")
       # HTML(paste(str1, str2,str3, sep = '<br/>'))
       
       # })
    
       output$mymap <- renderLeaflet({
         #ToDo : get name of the business
         leaflet(tempBusiness_df) %>% addTiles() %>%
           addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$name,"<br>",tempBusiness_df$address))
         
         #leaflet(tempBusiness_df) %>% addTiles() %>%
        #   addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$address))
         
         #leafletProxy("mymap", data = tempBusiness_df) %>%
         #  clearShapes() %>% addTiles() %>%
          # addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$name,"<br>",
                                                        #  tempBusiness_df$address))
         
       })
       
       # output$summary <- renderText("Summary")
       
       
       
       # Increment the progress bar, and update the detail text.
       incProgress(1/n, detail = paste("Analyzing"))
       
       # Pause for 0.1 seconds to simulate a long computation.
       Sys.sleep(0.1)
     }
   })
  #------------------------------------
   #tempDF <- business_df %>% filter(city == input$city & Sub.category == input$Type)
  
   
   
   
  })
 #---------------------Action for search : End
 
 
 #---------------------Action for reset button : Start
 observeEvent(input$resetButton,{
   updateSelectInput(session,inputId = "State",selected = 'Arizona') #,selected = 'OH'
   updateSelectInput(session,inputId = "city",selected = 'Tempe')
   updateSelectInput(session,inputId = "Type",selected = 'Bakery') #,selected = 'Bakery'
   leaflet(busin1) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$name,"<br>", busin1$address))
   #leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
 })
  
 #---------------------Action for reset button : End
 
 
  
 
}