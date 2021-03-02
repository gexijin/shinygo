####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: databaseView.R
# Purpose of file: Allow users example of database
# Start data: 12-21-2020 (mm-dd-yyyy)
# Data last modified: 02-03-2021 (mm-dd-yyyy)
#######################################################
if (!require('pacman')) {install.packages('pacman', dependencies = TRUE)} 
pacman::p_load(RSQLite, rlang, stringr, rebus, tuple, shiny, feather) #see purpose of package

#################################################################
# FUNCTION : geneListFormatter
# DESCRIPTION : Format input of gene list to work with SQL
# INPUT ARGS : input vector 
# OUTPUT ARGS : 
# IN/OUT ARGS :
# RETURN : vector of length 1 of characters
#################################################################
geneListFormatter <- function(inputVec = NULL) {
  inputVec <- as.vector(as.character(inputVec))
  inputVec <- stringr::str_to_upper(as.vector(str_split(inputVec, pattern = rebus.base::or(SPC, "\n"),
                                                        simplify = TRUE)))
  inputVec <- sprintf('"%s"', paste(inputVec, collapse = '","'))
  return(inputVec)
}


#################################################################
# FUNCTION : getExample 
# DESCRIPTION : Gives user example of our IDs, 
# and allows user to check if there genes are in our database.
# INPUT ARGS : Organism picked by user, user's Gene list, user's database option, and path to database
# OUTPUT ARGS : data frame of genes , or message not found
# IN/OUT ARGS :
# RETURN : returnDf
# Implementation notes : shiny is only used when this is used in shiny app
#################################################################
getExample <- function(userSpecie = NULL, path2Database = NULL, userIDtype = NULL,
                       geneList = NULL, shiny = FALSE) {
  returnDf = NULL
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path2Database)
  query4Specie <- paste('SELECT * FROM orginfo WHERE name2 =', shQuote(userSpecie))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  if (is.null(geneList) && !is.null(userIDtype)) { #if user doesn't give genelist
    ## and give id type 
    
    query4IDtype <- paste('SELECT * FROM idIndex WHERE idType =', shQuote(userIDtype))
    userIDtypeNum <- dbGetQuery(convertIDsDatabase, query4IDtype)
    query4IDmap <- paste('SELECT * FROM mapping WHERE species =', as.numeric(specie$id),
                         'AND idType =', as.numeric(userIDtypeNum$id))
    userIDdf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(userIDdf) == 0) { # check results
      
      outputText <- 'This combination of database and species did not return any results'
      rlang::message_cnd(outputText)
      returnDf <- outputText
    } else {
      returnDf <- userIDdf[-c(3,4)]
      
      colnames(returnDf) <- c('User_ID', 'Ensembl_ID')
    } # end of inner if/else
    if (shiny) {incProgress(1)}
    
  } else if (!is.null(geneList) && is.null(userIDtype)) { #if user gives geneList and not id type
    
    geneList4SQL <- geneListFormatter(inputVec = geneList)
    query4IDmap <- paste('SELECT * FROM mapping WHERE species =', as.numeric(specie$id),
                         'AND id IN (', geneList4SQL, ')')
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    if (nrow(foundGenesDf) == 0) { # check results
      
      outputText <- 'No matches were found in database of provided gene list.'
      message_cnd(outputText)
      returnDf <- outputText
    } else {
      
      idTypes <- unique(foundGenesDf$idType)
      idTypes4SQL <- paste(idTypes, collapse = ", ")
      query4idTypeMatch <- paste('SELECT * FROM idIndex WHERE id IN (', idTypes4SQL, ')')
      idIndexDf <- dbGetQuery(convertIDsDatabase, query4idTypeMatch)
      
      ##Make table for users to see where each gene turns into ens and what database
      uniqueGene <- unique(foundGenesDf$id)
      rowname <- c('Count', uniqueGene)
      colname <- c('Name', idIndexDf$idType)
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
        } # end of inner for loop
        if (shiny) {incProgress(1/length(rowname))}
      } # end of outer for loop
      for (indexC in 2:length(colname)) { #get total count
        bestMatchIDtypeDf[1, indexC] <- length(na.omit(bestMatchIDtypeDf[, indexC]))
      } # end of for loop
      returnDf <- bestMatchIDtypeDf
    } # end of inner if/else 
  } else {# if user gives both geneList and id type
    
    query4IDtype <- paste('SELECT * FROM idIndex WHERE idType =', shQuote(userIDtype))
    userIDtypeNum <- dbGetQuery(convertIDsDatabase, query4IDtype)
    geneList4SQL <- geneListFormatter(inputVec = geneList)
    query4IDmap <- paste('SELECT * FROM mapping WHERE species =', as.numeric(specie$id),
                         'AND idType =', as.numeric(userIDtypeNum$id),
                         'AND id IN (', geneList4SQL, ')')
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(foundGenesDf) == 0) { # check results
      outputText <- 'No matches were found in database of provided gene list.'
      message_cnd(outputText)
      returnDf <- outputText
    } else {
      returnDf <- foundGenesDf[-c(3,4)]
      colnames(returnDf) <- c('User_ID', 'Ensembl_ID')
    } # end of inner if/else 
    if (shiny) {incProgress(1)}
  }# end of outer if/else
  
  RSQLite::dbDisconnect(convertIDsDatabase)
  return(returnDf)
} # end of function


#################################################################
# FUNCTION : getExampleDfID
# DESCRIPTION : Gives user example of our IDs, 
# INPUT ARGS : Organism picked by user
# OUTPUT ARGS : data frame of genes
# IN/OUT ARGS :
# RETURN : returnDf
# Implementation notes : shiny is only used when this is used in shiny app
#################################################################
getExampleDfID <- function(userSpecie = NULL, path2Database = NULL, shiny = FALSE) {
  allSample <- feather::read_feather(path2Database)
  returnDF <- allSample[allSample$index == userSpecie,]
  returnDF <- returnDF[, -c(1)]
  colnames(returnDF) <- c('Database', 'Example_genes')
  if (shiny) {incProgress(1)}
  return(returnDF)
}


