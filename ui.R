
# Import Libraries-------------------------
library(shiny)
library(shinythemes)
library(rio)
library(dplyr)
library(leaflet)
library("tidyr")
#library(pacman)

#list_of_packages <- c("rio", "dplyr", "leaflet", "tidyr","shiny")

#lapply(list_of_packages, 
       #function(x) if(!require(x,character.only = TRUE)) install.packages(x))


#pacman::p_load(pacman, rio, dplyr, leaflet, tidyr,shiny)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# UI Logic-------------------------


ui <- navbarPage("Desicion support system",
                 tabPanel("Interactive Map",# Sidebar panel for inputs ----
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("./styles.css")
                                
                              ),
                              
                              fluidRow(
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
                                         
                                         htmlOutput("summary"),
                                         #dataTableOutput('resultTable'),
                                         tableOutput('resultTable'),
                                        # verbatimTextOutput("summary"),
                                         width=4,fluid = FALSE
                                ),
                                column(8,
                                       leafletOutput("mymap",height= 800)
                                )
                              ))) ,
                 tabPanel("About",
                          verbatimTextOutput("about")
                 )
)


