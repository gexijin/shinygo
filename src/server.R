####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: server.R
# Purpose of file: server file for shiny
# Start data: 01-03-2021 (mm-dd-yyyy)
# Data last modified: 01-03-2021 (mm-dd-yyyy)
#######################################################

pacman::p_load(shiny, reactable, stringr)
setwd("C:/Users/ericw/Documents/ge_lab/idep/database_shiny_app/shinygo//")
source("src/databaseView.R")

server <- function(input, output, session) {
  observeEvent(input$submit, {
    if (input$geneList != "No genes") { #turn input into vector 
      geneListVec <- as.vector(str_split(input$geneList, "\n", simplify = TRUE))
    }
    
    #Decide on what to pass to function
    if (input$geneList != "No genes" && input$userIDtype == "None") {
      result <- getUserDf(userSpecie = input$userSpecie, geneList = geneListVec)
    } else if (input$geneList == "No genes" && input$userIDtype != "None") {
      result <- getUserDf(userSpecie = input$userSpecie, userIDtype = input$userIDtype)
    } else {
      result <- getUserDf(userSpecie = input$userSpecie, userIDtype = input$userIDtype, geneList = geneListVec)
    }
    
    if (is.data.frame(result)) { #check result and how render it
      output$table <- renderReactable({
        reactable(result)
      })
    } else {
      output$text <- renderText(result)
    }
  })
}
