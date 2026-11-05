
# find idType based on index
findIDtypeById <- function(x) { # find
  return(idIndex$idType[as.numeric(x)])
}

findSpeciesById <- function(speciesID) { # find species name use id
  return(orgInfo[which(orgInfo$id == speciesID), ])
}

# just return name
findSpeciesByIdName <- function(speciesID) { # find species name use id
  return(orgInfo[which(orgInfo$id == speciesID), 3])
}

# Homo sapies --> hsapiens
shortSpeciesNames <- function(tem) {
  tem2 <- strsplit(as.character(tem), " ")
  return(tolower(paste0(substr(tem2[[1]][1], 1, 1), tem2[[1]][2])))
}

# convert sorted species:idType combs into a list for repopulate species choice
matchedSpeciesInfo <- function(x) {
  a <- c()
  for (i in 1:length(x)) {
    a <- c(a, paste(gsub("genes.*", "", findSpeciesByIdName(as.numeric(gsub(" .*", "", names(x[i]))))), " (",
      x[i], " mapped from ", findIDtypeById(gsub(".* ", "", names(x[i]))), ")",
      sep = ""
    ))
  }
  return(a)
}

showGeneIDs <- function(species, nGenes = 10) {
  # Given a species ID, this function returns 10 gene ids for each idType
  if (species == "BestMatch") {
    return(as.data.frame("Select a species above."))
  }
  # connect to the database, this becomes a global variable
  convert_species <- connect_convert_db_org(datapath, species)

  idTypes <- dbGetQuery(
    convert_species,
    paste0(" select DISTINCT idType from mapping;")
  ) # slow

  idTypes <- idTypes[, 1, drop = TRUE]

  if (nGenes > 100) nGenes <- 100 # upper limit

  # for each id Type
  for (k in 1:length(idTypes)) {
    # retrieve 500 gene ids and then random choose 10
    result <- dbGetQuery(
      convert_species,
      paste0(" select  id,idType from mapping where idType ='", idTypes[k], "'
                                 LIMIT ", 50 * nGenes)
    )
    result <- result[sample(1:(50 * nGenes), nGenes), ]
    if (k == 1) {
      resultAll <- result
    } else {
      resultAll <- rbind(resultAll, result)
    }
  }

  # Names of idTypes
  idNames <- dbGetQuery(
    convert_species,
    paste0(
      " SELECT id,idType from idIndex where id IN ('",
      paste(idTypes, collapse = "', '"), "') "
    )
  )
  dbDisconnect(convert_species)
  resultAll <- merge(resultAll, idNames, by.x = "idType", by.y = "id")



  # library(dplyr)
  resultAll <- resultAll %>%
    select(id, idType.y) %>%
    group_by(idType.y) %>%
    summarise(Examples = paste0(id, collapse = "; "))

  colnames(resultAll)[1] <- "ID Type"
  # put symbols first, refseq next, followed by ensembls. Descriptions (long gnee names) last
  resultAll <- resultAll[order(grepl("ensembl", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("refseq", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("symbol", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("description", resultAll$"ID Type"), decreasing = FALSE), ]

  return(resultAll)
}


#' Find a species by ID
#'
#' Find a species in the iDEP database with an
#' ID.
#'
#' @param species_id Species ID to search the database with
#' @param org_info iDEP data org_info file
#'
#' @export
#' @return Only return the species name with this function.
find_species_by_id_name <- function(species_id, org_info) {
  # find species name use id
  return(org_info[which(org_info$id == species_id), 3])
}

#' Find a species id by ensembl dataset name
#'
#' Find a species in the iDEP database with an
#' ID.
#'
#' @param species_id Species ID to search the database with
#' @param org_info iDEP data org_info file
#'
#' @export
#' @return Only return the species name with this function.
find_species_id_by_ensembl <- function(ensembl_dataset, org_info) {
  # find species name use id
  return(org_info[which(org_info$ensembl_dataset == ensembl_dataset), "id"])
}

#' Find taxon ID by species ID
#'
#' Find a species in the iDEP database with an
#' ID.
#'
#' @param species_id Species ID to search the database with
#' @param org_info iDEP data org_info file
#'
#' @export
#' @return Only return the species name with this function.
find_taxon_by_id <- function(species_id, org_info) {
  # find species name use id
  return(org_info[which(org_info$id == species_id), "taxon_id"])
}

