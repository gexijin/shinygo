####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: databaseView.R
# Purpose of file: Allow users example of database
# Start data: 12-21-2020 (mm-dd-yyyy)
# Data last modified: 01-03-2021 (mm-dd-yyyy)
#######################################################


#################################################################
# FUNCTION : getUserDf
# DESCRIPTION : Gives user example of our IDs, 
# and allows user to check if there genes are in our database.
# INPUT ARGS : Organism picked by user, user's Gene list, user's database option
# OUTPUT ARGS : data frame of genes , or message not found
# IN/OUT ARGS :
# RETURN : returnDf
#################################################################
getUserDf <- function(userSpecie = NULL, userIDtype = NULL, geneList = NULL) {
  pacman::p_load(RSQLite, rlang) #used for SQL datebase, condition handing 
  returnDf = NULL
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), "../convertIDs.db")
  query4Specie <- paste("SELECT * FROM orginfo WHERE name2 =", shQuote(userSpecie))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  if (!is.null(userIDtype)) {
    query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", shQuote(userIDtype))
    userIDtypeNum <- dbGetQuery(convertIDsDatabase, query4IDtype)
  }
  if (!is.null(geneList)) {
    geneList <- sort(geneList)
    geneList4SQL <- sprintf("'%s'", paste(geneList, collapse = "','"))
  }
  
  if (!is.null(geneList) && is.null(userIDtype)) { #if user gives geneList and not id type
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND id IN (", geneList4SQL, ")")
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    if (nrow(foundGenesDf) == 0) { # check results
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf <- outputText
    } else {
      idTypes <- unique(foundGenesDf$idType)
      idTypes4SQL <- paste(idTypes, collapse = ", ")
      query4idTypeMatch <- paste("SELECT * FROM idIndex WHERE id IN (", idTypes4SQL, ")")
      idIndexDf <- dbGetQuery(convertIDsDatabase, query4idTypeMatch)
      
      ##Make table for users to see where each gene turns into ens and what database
      rowname <- c("Count", foundGenesDf$id)
      bestMatchIDtypeDf <- data.frame(matrix(NA, nrow = length(rowname),
                                             ncol = length(idIndexDf$idType)))
      colnames(bestMatchIDtypeDf) <- idIndexDf$idType
      rownames(bestMatchIDtypeDf) <- make.names(rowname, unique = TRUE)
      for (indexR in 2:length(rowname)) {
        indexC <- which(!is.na(match(idIndexDf$id, foundGenesDf$idType[indexR-1])))
        bestMatchIDtypeDf[indexR, indexC] <- foundGenesDf$ens[indexR-1]
      } # end of for loop
      for (indexC in 1:length(idIndexDf$idType)) { #get total count
        bestMatchIDtypeDf[1, indexC] <- length(na.omit(bestMatchIDtypeDf[, indexC]))
      } # end of for loop
      returnDf <- bestMatchIDtypeDf
    } # end of inner if/else
    
  } else if (is.null(geneList) && !is.null(userIDtype)) { #if user doesn't give genelist
    ## and give id type 
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND idType =", as.numeric(userIDtypeNum$id))
    userIDdf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(userIDdf) == 0) { # check results
      outputText <- "This combination of database and species did not return any results"
      message(outputText)
      returnDf <- outputText
    } else {
      returnDf <- userIDdf[-c(3,4)]
    } # end of inner if/else
    
  } else { ## if user gives both geneList and id type
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND idType =", as.numeric(userIDtypeNum$id), "AND id IN (", geneList4SQL, ")")
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(foundGenesDf) == 0) { # check results
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf <- outputText
    } else {
      returnDf <- foundGenesDf[-c(3,4)]
    } # end of inner if/else
  } # end of outer if/else 
  
  RSQLite::dbDisconnect(convertIDsDatabase)
  return(returnDf)
} # end of function