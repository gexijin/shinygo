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
# OUTPUT ARGS : data frame of genes , or message not found(still working on)
# IN/OUT ARGS :
# RETURN : returnDf
#################################################################

getUserDf <- function(userSpecie = NULL, userIDtype = NULL, compare = FALSE,
                      geneList = NULL) {
  pacman::p_load(RSQLite, rlang) #used for SQL datebase, condition handing 
  
  returnDf = NULL
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), "../convertIDs.db")
  
  query4Specie <- paste("SELECT * FROM orginfo WHERE name =", shQuote(userSpecie))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", shQuote(userIDtype))
  userIDtype <- dbGetQuery(convertIDsDatabase, query4IDtype)
  
  if (compare == FALSE) {
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
  } else {
    geneList4SQL <- paste(shQuote(geneList), collapse = ", ")
    query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                         "AND idType =", as.numeric(userIDtype$id), "AND id IN (", geneList4SQL, ")")
    foundGenes <- dbGetQuery(convertIDsDatabase, query4IDmap)
    
    if (nrow(foundGenes) == 0) {
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf = outputText
    } else {
      returnDf = foundGenes
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
userSpecie <- 'Saccharomyces cerevisiae genes (R64-1-1)'
userIDtype <- 'entrezgene'
geneList <- c('852004', '850307', '854799', 'YLL027W')

 df <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype)

head(df)
df2 <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype, compare = TRUE,
                geneList = geneList)

 head(df2)

 geneList <- c('YLL827W')

df3 <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype, compare = TRUE,
                 geneList = geneList)

 head(df3)

 userIDtype <- 'pdb'
 df <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype)

 head(df)
#  to view orginfo
# convertIDsDatabase <- dbConnect(RSQLite::SQLite(), "../convertIDs.db")
#
# query4Specie <- paste("SELECT * FROM orginfo")
# specie <- dbGetQuery(convertIDsDatabase, query4Specie)
