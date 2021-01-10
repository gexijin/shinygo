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
if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)} 
pacman::p_load(shiny, reactable, stringr, rebus, RSQLite)
setwd("C:/Users/ericw/Documents/ge_lab/idep/database_shiny_app/shinygo/")
source("src/databaseView.R")

path <- "../convertIDs.db"
convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path)
specie <- dbGetQuery(convertIDsDatabase, paste("SELECT * FROM orginfo"))
specieList <- sort(specie$name2)
id <- dbGetQuery(convertIDsDatabase, paste("SELECT * FROM idIndex"))
idtypeList <- c("None", sort(id$idType))
RSQLite::dbDisconnect(convertIDsDatabase)
rm(convertIDsDatabase, specie, id)


server <- function(input, output, session) {
  ##ui logic that needs to be here see lines 21 and 24
  updateSelectizeInput(session = session, inputId = "userSpecie",
                       choices = specieList, server = TRUE)
  updateSelectizeInput(session = session, inputId = "userIDtype",
                       choices = idtypeList, server = TRUE)
  
  observeEvent(input$submit, {
    #Decide on what to pass to function
    if (input$geneList == "" && input$userIDtype == "None") {#user just gives species 
      result <- getUserDf(userSpecie = input$userSpecie, path2Database = path)
    } else if (input$geneList == "" && input$userIDtype != "None") { #if user doesn't give genelist
      ## and give id type 
      result <- getUserDf(userSpecie = input$userSpecie, path2Database = path, userIDtype = input$userIDtype)
    } else if (input$geneList != "" && input$userIDtype == "None") {#if user gives geneList and not id type
      geneListVec <- stringr::str_to_upper(as.vector(str_split(input$geneList, pattern = rebus.base::or(SPC, "\n"),
                                                               simplify = TRUE)))
      result <- getUserDf(userSpecie = input$userSpecie, path2Database = path, geneList = geneListVec)
    } else {# if user gives both geneList and id type
      geneListVec <- str_to_upper(as.vector(str_split(input$geneList, pattern = or(SPC, "\n"), simplify = TRUE)))
      result <- getUserDf(userSpecie = input$userSpecie, path2Database = path, 
                          userIDtype = input$userIDtype, geneList = geneListVec)
    }
    
    if (is.data.frame(result)) { #check result and how render it
      output$tableResult <- renderReactable({
        reactable::reactable(data = result, searchable = TRUE, bordered = TRUE,
                  highlight = TRUE, resizable = TRUE, minRows = 5)
      })
    } else {
      output$textResult <- renderText(result)
    }
  })
}
