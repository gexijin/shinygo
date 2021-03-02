####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: allsamplePreprocess.R
# Purpose of file: Preprocess data from SQLite database, into a feather file
#  where user can easily find example of gene IDs when species picked
# Start data: 02-02-2021 (mm-dd-yyyy)
# Data last modified: (mm-dd-yyyy)
#######################################################

if (!require('pacman')) {install.packages('pacman', dependencies = TRUE)} 
pacman::p_load(RSQLite, feather) #see purpose of package


#path <- readline(prompt="Enter path to SQLite convertIDs.db file: ")
path <- "../data/convertIDs_96.db"
#filename <- readline(prompt="Enter name for feather file: ")
filename <- "example_of_id"
filename <- paste(filename, '.feather')
filename <- gsub('\\s', '', filename)
convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path)
specie <- dbGetQuery(convertIDsDatabase, paste('SELECT * FROM orginfo'))
specieList <- unique(c("Human", sort(specie$name2)))
RSQLite::dbDisconnect(convertIDsDatabase)


work <- function(sp) {
  print(sp)
  #sp <- "Caenorhabditis elegans"
  #sp <- "Drosophila melanogaster"
  convertIDsDatabase <- dbConnect(RSQLite::SQLite(), path)
  query4Specie <- paste('SELECT * FROM orginfo WHERE name2 =', shQuote(sp))
  specie <- dbGetQuery(convertIDsDatabase, query4Specie)
  
  
  if (specie$totalGenes == 0) {
    matchIDtypeDf <- data.frame('index' = sp,
                                'id' = '0 Genes found',
                                'gene' = '0 Genes found')
  } else {
    query4IDmap <- paste('SELECT * FROM mapping WHERE species =', as.numeric(specie$id))
    foundGenesDf <- dbGetQuery(convertIDsDatabase, query4IDmap)
    idTypes <- unique(foundGenesDf$idType)
    idTypes4SQL <- paste(idTypes, collapse = ", ")
    query4idTypeMatch <- paste('SELECT * FROM idIndex WHERE id IN (', idTypes4SQL, ')')
    idIndexDf <- dbGetQuery(convertIDsDatabase, query4idTypeMatch)
    
    matchIDtypeDf <- data.frame('index' = vector(length = length(idIndexDf$idType)),
                                'id' = idIndexDf$idType)
    for (indexR in 1:length(idIndexDf$idType)) {
      matchIDtypeDf$index[indexR] <- sp
      tmpDf <- foundGenesDf[foundGenesDf$idType == idTypes[indexR],]
      tmpVec <- c(as.character(tmpDf$id[1]))
      for (index in 2:10) {
        tmpVec <- c(tmpVec, tmpDf$id[index])
      } # end of inner for
      tmpVec <- paste(tmpVec, collapse = ", ")
      matchIDtypeDf$gene[indexR] <- tmpVec
    } # end of outer for
  }
  
  RSQLite::dbDisconnect(convertIDsDatabase)
  return(matchIDtypeDf)
}

allSample <- data.frame('index' = vector(), 'id' = vector(),
                        'gene' = vector())

for (sp in specieList) {
  tmp <- work(sp)
  allSample <- rbind(allSample, tmp)

}
feather::write_feather(allSample, filename)




