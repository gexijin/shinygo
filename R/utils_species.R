# Species lookup functions
#
# Look up species details in orgInfo (the species table loaded in global.R),
# and fetch example gene IDs for a species.
#
#   findSpeciesById()            species ID -> its orgInfo row
#   findSpeciesByIdName()        species ID -> display name
#   find_species_by_id_name()    the same, with orgInfo passed in
#   find_species_id_by_ensembl() Ensembl dataset name -> species ID
#   find_taxon_by_id()           species ID -> NCBI taxonomy ID
#   showGeneIDs()                example gene IDs of each ID type
#
# Not used anywhere: shortSpeciesNames(), matchedSpeciesInfo() and
# findIDtypeById(). findIDtypeById() would also fail, as the idIndex table it
# reads is no longer loaded.
# ----------------------------------------------------------------------------


# The species' row in orgInfo.
findSpeciesById <- function(speciesID) {
  return(orgInfo[which(orgInfo$id == speciesID), ])
}

# The species' display name (orgInfo's third column, name2), e.g. "Human".
findSpeciesByIdName <- function(speciesID) {
  return(orgInfo[which(orgInfo$id == speciesID), 3])
}

# Same as findSpeciesByIdName(), but with orgInfo passed in.
find_species_by_id_name <- function(species_id, org_info) {
  return(org_info[which(org_info$id == species_id), 3])
}

# The species ID for an Ensembl dataset name, e.g. "hsapiens_gene_ensembl".
# The species table sends this name when a user clicks a species.
find_species_id_by_ensembl <- function(ensembl_dataset, org_info) {
  return(org_info[which(org_info$ensembl_dataset == ensembl_dataset), "id"])
}

# The species' NCBI taxonomy ID, used for STRING-db.
find_taxon_by_id <- function(species_id, org_info) {
  return(org_info[which(org_info$id == species_id), "taxon_id"])
}


# Example gene IDs for each ID type in a species' database, for the "Gene IDs
# examples" pop-up. Returns a data frame with one row per ID type: symbols
# first, then RefSeq, then Ensembl, with descriptions (long gene names) last.
showGeneIDs <- function(species, nGenes = 10) {
  if (species == "BestMatch") {
    return(as.data.frame("Select a species above."))
  }
  convert_species <- connect_convert_db_org(datapath, species)

  idTypes <- dbGetQuery(
    convert_species,
    paste0(" select DISTINCT idType from mapping;")
  ) # slow

  idTypes <- idTypes[, 1, drop = TRUE]

  if (nGenes > 100) nGenes <- 100 # upper limit

  # For each ID type, fetch 50 times as many IDs as needed and pick nGenes at
  # random (500 IDs for the default 10).
  for (k in 1:length(idTypes)) {
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

  # Replace ID type numbers with their names.
  idNames <- dbGetQuery(
    convert_species,
    paste0(
      " SELECT id,idType from idIndex where id IN ('",
      paste(idTypes, collapse = "', '"), "') "
    )
  )
  dbDisconnect(convert_species)
  resultAll <- merge(resultAll, idNames, by.x = "idType", by.y = "id")

  # One row per ID type, with its examples joined by "; ".
  resultAll <- resultAll %>%
    select(id, idType.y) %>%
    group_by(idType.y) %>%
    summarise(Examples = paste0(id, collapse = "; "))

  colnames(resultAll)[1] <- "ID Type"
  # Each sort moves one kind to the top, so the last sort wins: symbols first,
  # then RefSeq, then Ensembl. Descriptions go to the bottom.
  resultAll <- resultAll[order(grepl("ensembl", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("refseq", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("symbol", resultAll$"ID Type"), decreasing = TRUE), ]
  resultAll <- resultAll[order(grepl("description", resultAll$"ID Type"), decreasing = FALSE), ]

  return(resultAll)
}


# Unused Functions -----------------------------------------------------------

# "Homo sapiens" -> "hsapiens"
shortSpeciesNames <- function(tem) {
  tem2 <- strsplit(as.character(tem), " ")
  return(tolower(paste0(substr(tem2[[1]][1], 1, 1), tem2[[1]][2])))
}

# Turns species/ID-type match counts into labels such as
# "Human (40 mapped from symbol)".
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

# ID type name for an ID type number. Reads idIndex, which is no longer loaded.
findIDtypeById <- function(x) {
  return(idIndex$idType[as.numeric(x)])
}
