####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: databaseView.R
# Purpose of file: Allow users example of database
# Start data: 12-21-2020 (mm-dd-yyyy)
# Data last modified: 12-23-2020 (mm-dd-yyyy)
#######################################################


#################################################################
# FUNCTION : getUserDf
# DESCRIPTION : Gives user example of our IDs, 
# and allows user to check if there genes are in our database by compare = true
# INPUT ARGS : Organism picked by user, user's Gene list, user's database option
# compare is bool variable that if not put true will not check user genes
# OUTPUT ARGS : data frame of genes , or message not found
# IN/OUT ARGS :
# RETURN : returnDf
#################################################################

getUserDf <- function(userSpecie = NULL, userIDtype = NULL, bestMatchIDtype = NULL, 
                      compareGenes = FALSE, geneList = NULL) {
  pacman::p_load(RSQLite, rlang) #used for SQL datebase, condition handing 
  
  returnDf = NULL
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), "../convertIDs.db")
  
  query4Specie <- paste("SELECT * FROM orginfo WHERE name2 =", shQuote(userSpecie))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  if (compareGenes == TRUE && bestMatchIDtype == TRUE) { #if user gives geneList and not id type
    
    geneList4SQL <- sprintf("'%s'", paste(geneList, collapse = "','"))
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                          "AND id IN (", geneList4SQL, ")")
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    idTypes <- unique(foundGenesDf$idType)
    idTypes4SQL <- paste(idTypes, collapse = ", ")
    query4idTypeMatch <- paste("SELECT * FROM idIndex WHERE id IN (", idTypes4SQL, ")")
    idIndexDf <- dbGetQuery(convertIDsDatabase, query4idTypeMatch)
    
    bestMatchIDtypeDf <- data.frame(matrix(NA, nrow = length(foundGenesDf$id),
                               ncol = length(idIndexDf$idType)))
    colnames(bestMatchIDtypeDf) <- idIndexDf$idType
    rownames(bestMatchIDtypeDf) <- make.names(foundGenesDf$id, unique = TRUE)
    
    for (indexR in 1:length(foundGenesDf$id)) {
      indexC <- which(!is.na(match(idIndexDf$id, foundGenesDf$idType[indexR])))
      bestMatchIDtypeDf[indexR, indexC] <- foundGenesDf$ens[indexR]
    } # end of for loop
    
    
    
    returnDf = bestMatchIDtypeDf
    
  } else if (compareGenes == FALSE && bestMatchIDtype == FALSE) { #if user doesn't give genelist
    ## and give id type 
    query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", shQuote(userIDtype))
    userIDtype <- dbGetQuery(convertIDsDatabase, query4IDtype)
    
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND idType =", as.numeric(userIDtype$id))
    userIDdf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(userIDdf) == 0) { 
      outputText <- "This combination of database and species did not return any results"
      message(outputText)
      returnDf = outputText
    } else {
      returnDf = userIDdf
    } # end of inner if/else 
    
  } else { ## if user gives both geneList and id type
    geneList4SQL <- sprintf("'%s'", paste(geneList, collapse = "','"))
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND idType =", as.numeric(userIDtype$id), "AND id IN (", geneList4SQL, ")")
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(foundGenesDf) == 0) {
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf = outputText
    } else {
      returnDf = foundGenesDf
    } # end of inner if/else
    
  } # end of outer if/else 
  
  dbDisconnect(convertIDsDatabase)
  return(returnDf)
  
} # end of function


####################################################
###                                              ###
###                     MAIN                     ### 
###                                              ###
####################################################

##testing getUserDf() with different inputs
userSpecie <- "Mouse"
#userIDtype <- 'entrezgene'
geneList <- scan(what = character(), multi.line = TRUE)
geneList <- sort(geneList)

df <- getUserDf(userSpecie = userSpecie, bestMatchIDtype = TRUE, compareGenes = TRUE, geneList = geneList)
View(df)
#query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id))
#foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap) 
#sampleRun <- sample(foundGenesDf$id, 90)