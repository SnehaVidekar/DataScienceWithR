
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


ui <-  navbarPage("Decision support system",
                  tabPanel("Interactive Map",# Sidebar panel for inputs ----
                           div(class="outer",
                               
                               tags$head(
                                 # Include our custom CSS
                                 includeCSS("./styles.css")
                                 
                               ),
                               
                               conditionalPanel(condition = "output.setupComplete",fluidRow(
                                 sidebarPanel(
                                   htmlOutput("HeadText"),
                                   uiOutput("selectState"),
                                   #br(),
                                   uiOutput("selectCity"),
                                   #br(),
                                   uiOutput("selectType"),
                                   #br(),
                                   uiOutput("selectModel"),
                                   actionButton("searchButton", "Search"),
                                   actionButton("resetButton", "Reset"),
                                   hr(),
                                  
                                   #tableOutput('resultTable'),
                                   conditionalPanel(
                                     condition = "output.resultDisplayed",
                                     tableOutput('resultTable'),
                                     htmlOutput("predictionDetails")
                                   ),
                                   
                                   width=4,fluid = FALSE
                                   
                                 ),
                                 sidebarPanel(#column(8,
                                        
                                       htmlOutput("mapHeadText"),
                                        leafletOutput("mymap",height= 725) #750 # 800 for without conditional panel
                                       ,width=8,fluid = FALSE)
                               )),
                               conditionalPanel(condition = "!output.setupComplete",withSpinner(uiOutput("spinnerDummyID1"), type = 5,color = "#0dc5c1"))
                           )#Div End
                           )
                           ,#tabPanel end
                  tabPanel("About",
                           verbatimTextOutput("about")
                  )
)




