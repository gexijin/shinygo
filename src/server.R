####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: server.R
# Purpose of file: server file for shiny
# Start data: 01-03-2021 (mm-dd-yyyy)
# Data last modified: 01-20-2021 (mm-dd-yyyy)
#######################################################
if (!require('pacman')) {install.packages('pacman', dependencies = TRUE)} 
pacman::p_load(shiny, shinyjs, reactable, RSQLite, vroom) #see purpose of package
setwd('C:/Users/ericw/Documents/ge_lab/idep/database_shiny_app/shinygo/')
source('src/databaseView.R')

#Prep work to present species and database ID to user in UI
path <- '../data/convertIDs.db'
path2 <- '../data/example_of_id.feather'
convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path)
specie <- dbGetQuery(convertIDsDatabase, paste("SELECT * FROM orginfo"))
specieList <- unique(c("Human", sort(specie$name2)))
id <- dbGetQuery(convertIDsDatabase, paste('SELECT * FROM idIndex'))
idtypeList <- c("None", sort(id$idType))
RSQLite::dbDisconnect(convertIDsDatabase)
rm(convertIDsDatabase, specie, id)

#Show first Option example to user when first load
default <- getExampleDfID(userSpecie = specieList[1], path2Database = path2)
firstTime <- TRUE
MAX_WIDTH_COL <- 150


#################################################################
# FUNCTION : readFile (Helper function)
# DESCRIPTION : Reads file from user into a workable data
# INPUT ARGS : Character vector of path to file and file name
# OUTPUT ARGS : 
# IN/OUT ARGS :
# RETURN : Returns vector that should be a gene list
# Implementation notes : This seem more like a server end/shiny app function 
#################################################################
readFile <- function(datapath = NULL, name = NULL) {
  #Determines what type of file type is for the necessary delim
  ext <- tools::file_ext(name)
  switch(ext,
         csv = delim <- ",",
         tsv = delim <- "\t",
         txt = delim <- "\n",
         validate("Invalid file; Please upload a .csv, .tsv, or .txt file")
  )#end of switch 
  df <- vroom::vroom(datapath, delim = delim, col_names = FALSE)
  return(df$X1)
} # end of readFile

################################################################
# FUNCTION : colDefType (Helper function)
# DESCRIPTION : Used to format columns in tables 
# INPUT ARGS : input
# OUTPUT ARGS : 
# IN/OUT ARGS :
# RETURN : list of how to format each column in table 
#################################################################
colDefType <- function(input) {
  if (input$geneList == "" && input$userIDtype == "None") {#user just gives species 
    return(list(id = colDef(maxWidth = MAX_WIDTH_COL)))
  } else if (input$geneList == "" && input$userIDtype != "None") { #if user doesn't give genelist
    ## and give id type 
    return(list(User_ID = colDef(maxWidth = MAX_WIDTH_COL),
                Ensembl_ID = colDef(maxWidth = MAX_WIDTH_COL)))
  } else if (input$geneList != "" && input$userIDtype == "None" ) {#if user gives geneList and not id type
    return(NULL)
  } else {# if user gives both geneList file and id type
    return(list(User_ID = colDef(maxWidth = MAX_WIDTH_COL),
                Ensembl_ID = colDef(maxWidth = MAX_WIDTH_COL)))
  }# end of if/else
}


#################################################################
# FUNCTION : server
# DESCRIPTION : Code for back end of shiny app
# INPUT ARGS : inputs/output from ui.R 
#################################################################
server <- function(input, output, session) {
  ##ui logic that needs to be here see lines 21 and 24
  updateSelectizeInput(session = session, inputId = "userSpecie",
                       choices = specieList, server = TRUE)
  updateSelectizeInput(session = session, inputId = "userIDtype",
                       choices = idtypeList, server = TRUE)
  if (firstTime == TRUE) {
    output$tableDefault <- renderReactable({
      reactable::reactable(data = default,
                           columns = list(
                             id = colDef(maxWidth = MAX_WIDTH_COL)
                           ),
                           searchable = TRUE, bordered = TRUE,
                           highlight = TRUE, resizable = TRUE, minRows = 5)
    })
  }
  
  observeEvent(input$submit, {
    firstTime <- FALSE
    #Decide on what to pass to function
    if (input$geneList == "" && input$userIDtype == "None" && is.null(input$geneListFile)) {#user just gives species 
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExampleDfID(userSpecie = input$userSpecie, path2Database = path2, shiny = TRUE) 
        })
        return(result)
      })
      
    } else if (input$geneList == "" && input$userIDtype != "None" && is.null(input$geneListFile)) { #if user doesn't give genelist
      ## and give id type 
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExample(userSpecie = input$userSpecie, path2Database = path, userIDtype = input$userIDtype, shiny = TRUE) 
        })
        return(result)
      })
      
    } else if (input$geneList != "" && input$userIDtype == "None" && is.null(input$geneListFile)) {#if user gives geneList and not id type
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExample(userSpecie = input$userSpecie, path2Database = path, geneList = input$geneList, shiny = TRUE) 
        })
        return(result)
      })
      
    } else if(input$geneList == "" && input$userIDtype == "None" && !is.null(input$geneListFile)) {#if user gives geneList file and not id type
      
      geneListVec <- readFile(datapath = input$geneListFile$datapath, name = input$geneListFile$name)
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExample(userSpecie = input$userSpecie, path2Database = path, geneList = geneListVec, shiny = TRUE) 
        })
        return(result)
      })
      
    } else if (input$geneList != "" && input$userIDtype != "None" && is.null(input$geneListFile)) {# if user gives both geneList and id type
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExample(userSpecie = input$userSpecie, path2Database = path, 
                               userIDtype = input$userIDtype, geneList = input$geneList, shiny = TRUE) 
        })
        return(result)
      })
      
    } else {# if user gives both geneList file and id type
      
      geneListVec <- readFile(datapath = input$geneListFile$datapath, name = input$geneListFile$name)
      
      getExampleSer <- shiny::reactive({
        withProgress(message = 'Work be done...', value = 0, {
          result <- getExample(userSpecie = input$userSpecie, path2Database = path, 
                               userIDtype = input$userIDtype, geneList = geneListVec, shiny = TRUE) 
        })
        return(result)
      })
      
    }# end of if/else
    
    res <- getExampleSer()
    
    if (is.data.frame(res)) { #check getExampleSer and how render it
      shinyjs::hide(id = "tableDefault")
      shinyjs::hide(id = "textResult")
      shinyjs::show(id = "tableResult")
      
      output$tableResult <- renderReactable({
        reactable::reactable(data = res,
                             columns = colDefType(input),
                             searchable = TRUE, bordered = TRUE, 
                             highlight = TRUE, resizable = TRUE, minRows = 5)
      })
      
    } else {
      shinyjs::hide(id = "tableDefault")
      shinyjs::hide(id = "tableResult")
      shinyjs::show(id = "textResult")
      output$textResult <- renderText(res)
      
    }# end of if/else
  }) #end of observeEvent
  
  observeEvent(input$reset, {
    shinyjs::reset(id = "geneListFile")
    shinyjs::reset(id = "geneList")
    shinyjs::hide(id = "tableResult")
    shinyjs::hide(id = "textResult")
  }) #end of observeEvent
} # of server 