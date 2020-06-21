
# Import Libraries-------------------------
library(shiny)
library(shinythemes)
library(rio)
library(dplyr)
library(leaflet)
library("tidyr")
library(shinycssloaders)



r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# UI Logic-------------------------


ui <-  navbarPage("Desicion support system",
                  tabPanel("Interactive Map",# Sidebar panel for inputs ----
                           div(class="outer",
                               
                               tags$head(
                                 # Include our custom CSS
                                 includeCSS("./styles.css")
                                 
                               ),
                               
                               conditionalPanel(condition = "output.setupComplete",fluidRow(
                                 sidebarPanel(
                                   uiOutput("selectState"),
                                   br(),
                                   uiOutput("selectCity"),
                                   br(),
                                   uiOutput("selectType"),
                                   br(),
                                   actionButton("searchButton", "Search"),
                                   actionButton("resetButton", "Reset"),
                                   hr(),
                                  
                                   #tableOutput('resultTable'),
                                   conditionalPanel(
                                     condition = "output.resultDisplayed",
                                     tableOutput('resultTable'),
                                     htmlOutput("predictionDetails"),
                                   ),
                                   
                                  
                                   
                                   width=4,fluid = FALSE
                                   
                                 ),
                                 column(8,
                                        leafletOutput("mymap",height= 750) #550 # 800 for without conditional panel
                                 )
                               )),
                               conditionalPanel(condition = "!output.setupComplete",withSpinner(uiOutput("spinnerDummyID1"), type = 5,color = "#0dc5c1"))
                           )#Div End
                           )
                           ,#tabPanel end
                  tabPanel("About",
                           verbatimTextOutput("about")
                  )
)




