
# Import Libraries-------------------------
library(shiny)

library(data.table)
library(tidyr)
library('dplyr')
#library(e1071)
library(randomForest)
library(data.table)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


# Data Load--------------------------------

print(Sys.time())
#app_data <- read.csv("./Final_dataset.csv") # with this 33 sec
#app_data <- read.csv("./final_train_app.csv")

#Load from rds file
app_data <- readRDS("./shinyAppDF.rds") #Need to create with new test data

#------Make business df 
#business_df <- app_data[,names(app_data) %in% c( "business_id","State","City","Address","Latitude","Longitude","Stars","Sub_category")]
#business_df <-app_data[,1:8]
business_df <-app_data[,1:9]
business_df <- distinct(business_df)

#Get unique city
uniqueCity <- sort(unique(business_df$City))
#Get unique state names
uniqueState <- sort(unique(business_df$State))
#Get unique categories
UniqueType <- sort(unique(business_df$Sub_category))
#print(UniqueType)

#------DF for inital load and reset
busin1 <- business_df%>% filter(State == 'Arizona'& City == 'Tempe' & Sub_category == 'Bakery')

print(Sys.time())

#---- Load model
#Call MOdel e1071
#modelObject <- readRDS("./final_model_rds.rds")

#Call MOdel random Forest
modelObject <- readRDS("./rf_dtm_100_new.rds")

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
                    as.character(uniqueCity),selected = 'Tempe')
    else{
      # Get the data set with the appropriate name
      cityForState <- sort(unique(business_df[business_df$State == input$State, "City"]))
      selectInput(inputId ="city",label = "Choose a city:", choices= as.character(cityForState),selected = 'Tempe')
    }
    
  )
  
  
  
  #Display map
  output$mymap <- renderLeaflet({
    
    leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$Name,"<br>",busin1$Address))
    #leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
    
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
       #tempBusiness_df <-tempDF[,1:8]
       tempBusiness_df <-tempDF[,1:9]
       tempBusiness_df <- distinct(tempBusiness_df)#distinct(tempBusiness_df$business_id)
       
       
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
         #avgRestaurantRatings =  unique(tempDF$Stars)#Recheck
         #avgRestaurantRatings <- mean(tempDF$Stars) 
         
         # Need to check
         avgRestaurantRatings <- mean(tempDF$Business_Stars)
         #avgRestaurantRatings <- mean(tempDF$Review_rating)
         totalReviewCount <- as.integer(nrow(tempDF))
         
         
         #Split DF to get prediction of reviews
         testDf <- tempDF[,11:35]
         result <- predict(modelObject,testDf)
         
         #table(result)
         
         #From old model.. dont delete
         #numberofPositiveReviews <- as.integer(getElement(table(result), "positive"))#as.integer(234)
         #numberofNegativeReviews <- getElement(table(result), "negative")#as.integer(123)
         
         #for rf_dtm_100_new model
         numberofPositiveReviews <- as.integer(getElement(table(result), "1"))# check  positive and negative
         numberofNegativeReviews <- getElement(table(result), "2")
         
         #resultContent = c(as.numeric(totalRestaurant) , avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
         resultContent <- list(as.integer(totalRestaurant), avgRestaurantRatings , as.integer(totalReviewCount) , as.integer(numberofPositiveReviews), as.integer(numberofNegativeReviews))
         
         
        
         #Populate the summary for prediction
         resultsDF$Count<- resultContent 
         
         #Rule based for prediction result
         if(totalRestaurant <= thresholdForRestaurant ){
           if(avgRestaurantRatings <= thresholdForRating){
             predictionText <- paste("<b>Prediction: Appears to be some potential.</b><br/><br/> Number of restaurant are very low and restaurant rating is also below average.<br/>
                                     There is an opportunity in this particular area.")
           }
           else {
             predictionText <- paste("<b>Prediction: Appears to be some potential.</b><br/><br/> Number of restaurant are very low but restaurant rating is above average.<br/>
                                     Additional parameters like city size should be considered for further scope." )
           }
           
         }else{
           if(avgRestaurantRatings <= thresholdForRating){
             predictionText <- paste("<b>Prediction: Potential seems to be high.</b><br/><br/> Number of restaurant are very high but restaurant rating is below average.<br/>
                                     Hence highly recommended." )
           }
           else {
             predictionText <- paste("<b>Prediction: Potential seems to be low.</b><br/><br/> Number of restaurant are very high and restaurant rating is above average.<br/>
                                     Less potential to open restaurant of the same type." )
           }
         }
         
       }
       #Transpose
       
       #Render result summary table
       output$resultTable <- renderTable(resultsDF)#renderDataTable(resultsDF)
       
       #Render prediction details
       output$predictionDetails <- renderUI({
         #predictionTitle <- paste("<b>Prediction : </b> <br/>")
         HTML(paste( predictionText,  sep = '<br/>'))
       
       })
   
    
       output$mymap <- renderLeaflet({
         #ToDo : get name of the business
         leaflet(tempBusiness_df) %>% addTiles() %>%
           addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$Name,"<br>",tempBusiness_df$Address))
         
         #leaflet(tempBusiness_df) %>% addTiles() %>%
        #   addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$address))
         
         #leafletProxy("mymap", data = tempBusiness_df) %>%
         #  clearShapes() %>% addTiles() %>%
          # addMarkers(~Longitude, ~Latitude,popup = paste(tempBusiness_df$name,"<br>",
                                                        #  tempBusiness_df$address))
         
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
   updateSelectInput(session,inputId = "State",selected = 'Arizona') #,selected = 'OH'
   updateSelectInput(session,inputId = "city",selected = 'Tempe')
   updateSelectInput(session,inputId = "Type",selected = 'Bakery') #,selected = 'Bakery'
   #Display map
   output$mymap <- renderLeaflet({
     
     leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$Name,"<br>",busin1$Address))
     #leaflet(busin1) %>% addTiles()%>% addMarkers(~Longitude, ~Latitude,popup = paste(busin1$address))
     
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