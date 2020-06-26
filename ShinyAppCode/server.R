
# Import Libraries-------------------------
library(shiny)

library(data.table)
library(tidyr)
library('dplyr')
library(e1071)
library(randomForest)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


# Data Load--------------------------------

print(Sys.time())
#app_data <- read.csv("./Final_dataset.csv") # with this 33 sec

#Load from rds file
app_data <- readRDS("./shinyAppDFv6.rds") # with 1 sec

#------Make business df 

#business_df <-app_data[,1:9]
business_df <-app_data[,1:13]
business_df <- distinct(business_df)

#Get unique city
uniqueCity <- sort(unique(business_df$City))
#Get unique state names
uniqueState <- sort(unique(business_df$State))
#Get unique categories
UniqueType <- sort(unique(business_df$Sub_category))
#print(UniqueType)

#------DF for inital load and reset

busin1 <- business_df%>% filter(State == 'Arizona'& City == 'Phoenix' & Sub_category == 'Bakery')
print(Sys.time())

#---- Load model


#Load both models 
randomForestObject <- readRDS("./rf_dtm_500_V1.2.rds")
naiveBayesObject <- readRDS("./review_model_bayes_V1.2.rds")

#----get colors for marker
getColor <- function(dfVar) {
  sapply(dfVar$Price_range, function(Price_range) {
    
    switch (as.character(Price_range),
            "Not Available" = "lightgray","1" = "lightblue","2" = "cadetblue","3" = "cadetblue","4" = "red"
    )
    
  })
}


# Server logic-----------------------------

server <- function(input,output,session) {
  
  #------------------Initial UI load : Start
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  rv$resultDisplayed <- FALSE
  observe({
  
  #Render prediction details
    output$HeadText <- renderUI({
      headTextTemp <- paste("<b> Looking for new restaurant location? </b><br/> <br/>")
      HTML(paste(headTextTemp,  sep = '<br/>'))
      
    })
  #Populate different restaurant types
    output$selectType <- renderUI(
      selectInput(inputId ="Type",label = "Choose a restaurant type:", choices= as.character(UniqueType)
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
                    as.character(uniqueCity),selected = 'Phoenix')
    else{
      # Get the data set with the appropriate name
      cityForState <- sort(unique(business_df[business_df$State == input$State, "City"]))
      selectInput(inputId ="city",label = "Choose a city:", choices= as.character(cityForState),selected = 'Phoenix')
    }
    
  )
  
  #Populate decision model dropdown
  output$selectModel <- renderUI(
    
      selectInput(inputId ="modelType",label = "Choose a decision model:", choices= 
                    c("Naive Bayes" = "nbModel",
                      "Random Forest" = "rmModel"),selected = 'rmModel')
    
  )
  
 output$mapHeadText <- renderUI({
  
   mapTxt <- paste("<p><b> Restaurants currently present in the city.</b> </p>") #paste("<p style=\"color:#3333FF\">  </p>")
   HTML(paste(mapTxt,  sep = '<br/>'))
    
  })
  
  
  #Display map
  output$mymap <- renderLeaflet({
    
    
    iconsVar <- awesomeIcons(
      icon = 'information',
      library = 'ion',
      markerColor = getColor(busin1)
    )
  
    
    map <- leaflet(busin1,options = leafletOptions(worldCopyJump = F)) %>% addTiles()%>% addAwesomeMarkers(~Longitude, ~Latitude,popup = paste("Name:",busin1$Name,"<br> Address:",busin1$Address,"<br> Rating :",busin1$Business_Stars,
                                                                                     "<br> Outdoor Seating :",busin1$Outdoor_seating,"<br> Take out option :",busin1$Restaurant_takeout,
                                                                                     "<br> Restaurant delivery :",busin1$Restaurant_delivery), icon =iconsVar) 
                                                               
                                                                
    map %>%
      addLegend("topright",colors =c("lightgray",  "lightblue", "cadetblue", "red"),
                labels= c("NA", "under $10","$11-60","over $61"),
                title = "Price per person:",
                opacity = 1
      )
   
   
   
  })
  
  
  
  ## set condition to TRUE
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
    
     n <- 10
     thresholdForRestaurant <- 6
     thresholdForRating <- 3
     predictionText <- ""
     
     for (i in 1:n) {
       #For RDs file
       tempDF <- app_data %>% filter(State == input$State & City == input$city & Sub_category == input$Type)
       
       #Split DF in business 
       tempBusiness_df <-tempDF[,1:13]
      
       tempBusiness_df <- distinct(tempBusiness_df)
       
       
       # result data frame
       resultRowID <- c("Total number of restaurants" ,"Average restaurant ratings","Total number of reviews", "Number of positive reviews","Number of negative reviews")
       resultsDF <- data.frame(Summary=resultRowID)
       resultsDF["Count"] <- NA 
       
       
       if(is.data.frame(tempDF) && nrow(tempDF)==0){ # No restuarants matching the input criteria
         tempDF <- business_df
         predictionText <- paste("<b>Prediction : </b><br/> Not sufficient information available for selected input criteria." )
         
       }
       else{
         totalRestaurant <- nrow(tempBusiness_df)#as.integer(nrow(unique(tempDF$business_id)))#length(unique(tempDF$business_id))#nrow(unique(tempDF$business_id))
         
         # Need to check
         avgRestaurantRatings <- mean(tempDF$Business_Stars)
         #avgRestaurantRatings <- mean(tempDF$Review_rating)
         totalReviewCount <- as.integer(nrow(tempDF))
         
         
         #Split DF to get prediction of reviews
         testDf <- tempDF[,15:39]
        
         if (input$modelType == "nbModel") {
           result <- predict(naiveBayesObject,newdata=testDf,type = "class" )
         }
         else{
           result <- predict(randomForestObject,newdata=testDf,type = "class" )
         }
         
         
         #table(result)
         
         #From old model.. dont delete
         #numberofPositiveReviews <- as.integer(getElement(table(result), "positive"))#as.integer(234)
         #numberofNegativeReviews <- getElement(table(result), "negative")#as.integer(123)
         
         #for rf_dtm_100_new model
         numberofPositiveReviews <- as.integer(getElement(table(result), "2"))
         numberofNegativeReviews <- getElement(table(result), "1") #Confirmed on 25th June
         
         
         resultContent <- list(as.integer(totalRestaurant), avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
         
        
         #Populate the summary for prediction
         resultsDF$Count<- resultContent 
         
         #Rule based for prediction result
         if(totalRestaurant <= thresholdForRestaurant ){
           if(avgRestaurantRatings <= thresholdForRating){
             predictionText <- paste("<b>Prediction: Appears to be some potential.</b><br/><br/> Number of restaurants are very low and restaurant rating is also below average.<br/>
                                     There is an opportunity in this particular area.")
           }
           else {
             predictionText <- paste("<b>Prediction: Appears to be some potential.</b><br/><br/> Number of restaurants are very low but restaurant rating is above average.<br/>
                                     Additional parameters like city size should be considered for further scope." )
           }
           
         }else{
           if(avgRestaurantRatings <= thresholdForRating){
             predictionText <- paste("<b>Prediction: Potential seems to be high.</b><br/><br/> Number of restaurants are very high but restaurant rating is below average.<br/>
                                     Hence highly recommended." )
           }
           else {
             predictionText <- paste("<b>Prediction: Potential seems to be low.</b><br/><br/> Number of restaurants are very high and restaurant rating is above average.<br/>
                                     Less potential to open restaurant of the same type." )
           }
         }
         
       }
       
       
       #Render result summary table
       output$resultTable <- renderTable(resultsDF)#renderDataTable(resultsDF)
       
       #Render prediction details
       output$predictionDetails <- renderUI({
         #predictionTitle <- paste("<b>Prediction : </b> <br/>")
         HTML(paste( predictionText,  sep = '<br/>'))
       
       })
   
    
       output$mymap <- renderLeaflet({
         
         iconsVar <- awesomeIcons(
           
           icon = 'information', 
           library = 'ion',
           markerColor = getColor(tempBusiness_df)
         )
        
             map <-  leaflet(tempBusiness_df,options = leafletOptions(worldCopyJump = F)) %>% addTiles()%>% 
           addAwesomeMarkers(~Longitude, ~Latitude,popup = paste("Name:",tempBusiness_df$Name,"<br> Address:",tempBusiness_df$Address,
                                                          "<br> Rating :",tempBusiness_df$Business_Stars,
                                                          "<br> Outdoor Seating :",tempBusiness_df$Outdoor_seating,"<br> Take out option :",tempBusiness_df$Restaurant_takeout,
                                                           "<br> Restaurant delivery :",tempBusiness_df$Restaurant_delivery), icon =iconsVar) #, label=~as.character(busin1$Price_range)
         
             map %>%
               addLegend("topright",colors =c("lightgray",  "lightblue", "cadetblue", "red"),
                         labels= c("NA", "under $10","$11-60","over $61"),
                         title = "Price per person:",
                         opacity = 1
               )
       
         
       })
       
       # Increment the progress bar
       incProgress(1/n, detail = paste("Analyzing"))
       
       # Pause for 0.1 seconds
       Sys.sleep(0.1)
     }
   })
  #------------------------------------

  
   ## set condition to TRUE to display result components
   rv$resultDisplayed <- TRUE
 
   output$resultDisplayed <- reactive({
     return(rv$resultDisplayed)
   })
   outputOptions(output, 'resultDisplayed', suspendWhenHidden=FALSE)
   
   
   
   
  })
 #---------------------Action for search : End
 
 
 #---------------------Action for reset button : Start
 observeEvent(input$resetButton,{
   updateSelectInput(session,inputId = "State",selected = 'Arizona')
   updateSelectInput(session,inputId = "city",selected = 'Phoenix')
   updateSelectInput(session,inputId = "Type",selected = 'Bakery')
   updateSelectInput(session,inputId = "modelType",selected = 'rmModel') 
   #Display map
   output$mymap <- renderLeaflet({
     
     iconsVar <- awesomeIcons(
       icon = 'information',
       library = 'ion',
       markerColor = getColor(busin1)
     )                                                           
     
 map<- leaflet(busin1,options = leafletOptions(worldCopyJump = F)) %>% addTiles()%>% 
       addAwesomeMarkers(~Longitude, ~Latitude,popup = paste("Name:",busin1$Name,"<br> Address:",busin1$Address,
                                                      "<br> Rating :",busin1$Business_Stars,
                                                      "<br> Outdoor Seating :",busin1$Outdoor_seating,"<br> Take out option :",busin1$Restaurant_takeout,
                                                      "<br> Restaurant delivery :",busin1$Restaurant_delivery), icon =iconsVar)
     
 map %>%
   addLegend("topright",colors =c("lightgray",  "lightblue", "cadetblue", "red"),
             labels= c("NA", "under $10","$11-60","over $61"),
             title = "Price per person:",
            
             opacity = 1
   )
     
     
   })
   
   ## Hide results on reset
   rv$resultDisplayed <- FALSE
   
   output$resultDisplayed <- reactive({
     return(rv$resultDisplayed)
   })
   outputOptions(output, 'resultDisplayed', suspendWhenHidden=FALSE)
   
   
   
 })
  
 #---------------------Action for reset button : End
 
 
}