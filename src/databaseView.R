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
  
  query4Specie <- paste("SELECT * FROM orginfo WHERE name =", userSpecie)
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  query4IDtype <- paste("SELECT * FROM idIndex WHERE idType =", userIDtype)
  userIDtype <- dbGetQuery(convertIDsDatabase, query4IDtype)
  
  query4IDmap <- paste("SELECT * FROM mapping WHERE species =", as.numeric(specie$id),
                       "AND idType =", as.numeric(userIDtype$id))
  userIDdf <- dbGetQuery(convertIDsDatabase, query4IDmap)
  
  if (is.null(dim(userIDdf))) { 
    outputText <- "This combination of database and species did not return any results"
    message(outputText)
    returnDf = outputText
  } else if (compare == FALSE) {
    returnDf = userIDdf
  } else {
    
    foundGenes <- match(geneList, userIDdf$id)
    if (length(geneList) == sum(is.na(foundGenes))) {
      outputText <- "No matches were found in database of provided gene list."
      message(outputText)
      returnDf = outputText
    } else {
      foundGenes <- as.list(foundGenes)
      foundGenesDf <- data.frame("User_genes" = geneList,
                                 "Ensembl_ID" = character(length(geneList)),
                                 "Found_in_database" = character(length(geneList)),
                                 stringsAsFactors=FALSE)
      index = 1
      for (val in foundGenes) {
        
        if (foundGenes[index] != 'NA') {
          position = as.numeric(foundGenes[index])
          foundGenesDf[index, 2] = userIDdf$ens[position]
          foundGenesDf[index, 3] = "Found"
        } else {
          foundGenesDf[index, 3] = foundGenesDf[index, 2] = "Not Found"
        } # end of if/else 
        index = index + 1
      } # end of for loop
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

userSpecie <- "'Saccharomyces cerevisiae genes (R64-1-1)'"
userIDtype <- "'entrezgene'"
geneList <- c("852004", "850307", "854799", "YLL027W")

df <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype)

head(df)

df2 <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype, compare = TRUE,
                 geneList = geneList)

head(df2)

geneList <- c("YLL027W")

df3 <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype, compare = TRUE,
                 geneList = geneList)

head(df3)

userIDtype <- "'pdb'"
df <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype)

head(df)
