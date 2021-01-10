####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: databaseView.R
# Purpose of file: Allow users example of database
# Start data: 12-21-2020 (mm-dd-yyyy)
# Data last modified: 01-10-2021 (mm-dd-yyyy)
#######################################################
if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)} 
pacman::p_load(RSQLite, rlang, tuple) #used for SQL datebase, condition handing, matchALL

#################################################################
# FUNCTION : getUserDf
# DESCRIPTION : Gives user example of our IDs, 
# and allows user to check if there genes are in our database.
# INPUT ARGS : Organism picked by user, user's Gene list, user's database option, and path to database
# OUTPUT ARGS : data frame of genes , or message not found
# IN/OUT ARGS :
# RETURN : returnDf
#################################################################
getUserDf <- function(userSpecie = NULL, path2Database = NULL, userIDtype = NULL, geneList = NULL) {
  returnDf = NULL
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path2Database)
  query4Specie <- paste("SELECT * FROM orginfo WHERE name2 =", shQuote(userSpecie))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  if (is.null(geneList) && is.null(userIDtype)) {#user just gives species 
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id))
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    idTypes <- unique(foundGenesDf$idType)
    idTypes4SQL <- paste(idTypes, collapse = ", ")
    query4idTypeMatch <- paste("SELECT * FROM idIndex WHERE id IN (", idTypes4SQL, ")")
    idIndexDf <- dbGetQuery(convertIDsDatabase, query4idTypeMatch)
    matchIDtypeDf <- data.frame(matrix(NA, nrow = length(5),
                                       ncol = length(idIndexDf$idType)))
    colnames(matchIDtypeDf) <- idIndexDf$idType
    
    for (indexC in 1:length(idIndexDf$idType)) {
      tmpDf <- foundGenesDf[foundGenesDf$idType == idTypes[indexC],]
      for (indexR in 1:5) {
        matchIDtypeDf[indexR, indexC] <- tmpDf$id[indexR]
      } # end of inner for
    } # end of outer for
    returnDf <- matchIDtypeDf
    
  } else if (is.null(geneList) && !is.null(userIDtype)) { #if user doesn't give genelist
    ## and give id type 
    query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", shQuote(userIDtype))
    userIDtypeNum <- dbGetQuery(convertIDsDatabase, query4IDtype)
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id), "AND idType =", as.numeric(userIDtypeNum$id))
    userIDdf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(userIDdf) == 0) { # check results
      outputText <- "This combination of database and species did not return any results"
      rlang::message(outputText)
      returnDf <- outputText
    } else {
      returnDf <- userIDdf[-c(3,4)]
    } # end of inner if/else
    
  } else if (!is.null(geneList) && is.null(userIDtype)) { #if user gives geneList and not id type
    geneList <- sort(geneList)
    geneList4SQL <- sprintf("'%s'", paste(geneList, collapse = "','"))
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id), "AND id IN (", geneList4SQL, ")")
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
      uniqueGene <- unique(foundGenesDf$id)
      rowname <- c("Count", uniqueGene)
      colname <- c("Name", idIndexDf$idType)
      bestMatchIDtypeDf <- data.frame(matrix(NA, nrow = length(rowname),
                                             ncol = length(colname)))
      colnames(bestMatchIDtypeDf) <- colname
      bestMatchIDtypeDf$Name <- rowname
      for (indexR in 2:length(rowname)) {
        tmpDf <- foundGenesDf[foundGenesDf$id == rowname[indexR],]
        match4IdRow <- tuple::matchAll(tmpDf$idType, idIndexDf$id)
        i = 1
        for (indexC in match4IdRow) {
          bestMatchIDtypeDf[indexR, indexC+1] <- tmpDf$ens[i]
          i = i + 1
        }
      } # end of for loop
      for (indexC in 2:length(colname)) { #get total count
        bestMatchIDtypeDf[1, indexC] <- length(na.omit(bestMatchIDtypeDf[, indexC]))
      } # end of for loop
      returnDf <- bestMatchIDtypeDf
    } # end of inner if/else 
  } else {# if user gives both geneList and id type
    query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", shQuote(userIDtype))
    userIDtypeNum <- dbGetQuery(convertIDsDatabase, query4IDtype)
    geneList <- sort(geneList)
    geneList4SQL <- sprintf("'%s'", paste(geneList, collapse = "','"))
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id), "AND idType =", as.numeric(userIDtypeNum$id), "AND id IN (", geneList4SQL, ")")
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(foundGenesDf) == 0) { # check results
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf <- outputText
    } else {
      returnDf <- foundGenesDf[-c(3,4)]
    } # end of inner if/else 
  }# end of outer if/else
  
  RSQLite::dbDisconnect(convertIDsDatabase)
  return(returnDf)
} # end of function
