#' convertEnsembl2Entrez
#'
#' @description Maps a user's Ensembl gene IDs to Entrez gene IDs for a given
#'   species, by querying that species' conversion database. Called by the
#'   KEGG tab (`mod_05_kegg.r`) to build the `gene.data` vector for
#'   `mypathview()`, since KEGG pathway images are keyed by Entrez ID rather
#'   than Ensembl ID.
#'
#' @param query Character vector (or a single tab/space/newline/comma
#'   -separated string) of Ensembl gene IDs to convert, as typed/pasted by
#'   the user.
#' @param Species The Ensembl dataset name for the species
#'   (`orgInfo$ensembl_dataset`), used to look up the species' numeric ID
#'   and connect to its conversion database.
#'
#' @return A data frame with `entrezgene_id` and `ensembl_gene_id` columns,
#'   one row per input gene found in the mapping table; `NULL` if none
#'   matched.
#'
#' @noRd
convertEnsembl2Entrez <- function(query, Species) {
  speciesID <- orgInfo$id[which(orgInfo$ensembl_dataset == Species)] # note uses species Identifying
  # connect to the database, this becomes a global variable
  convert_species <- connect_convert_db_org(datapath, speciesID)
  # finds id index corresponding to entrez gene and KEGG for id conversion
  idType_Entrez <- dbGetQuery(convert_species, paste("select distinct * from idIndex where idType = 'entrezgene_id'"))
  if (dim(idType_Entrez)[1] != 1) {
    cat("Warning! entrezgene ID not found!")
  }
  idType_Entrez <- as.numeric(idType_Entrez[1, 1])

  # given a set of ensembl ids, return a mapping table to Entrez gene ID
  querySet <- cleanGeneSet(unlist(strsplit(toupper(query), "\t| |\n|\\,")))

  result <- dbGetQuery(
    convert_species,
    paste0(
      " SELECT  id,ens from mapping where idType ='", idType_Entrez, "'",
      " AND ens IN ('", paste(querySet, collapse = "', '"), "')"
    )
  )
  dbDisconnect(convert_species)

  if (dim(result)[1] == 0) {
    return(NULL)
  }

  colnames(result) <- c("entrezgene_id", "ensembl_gene_id")

  return(result)
}

#' keggPathwayID
#'
#' @description Looks up a pathway's bare KEGG pathway ID (e.g. "hsa04110")
#'   from its plain-text description (e.g. "Cell cycle"), by querying the
#'   species' pathway SQLite database and restricting the match to entries
#'   in the given gene-set category. Not currently called anywhere in the
#'   app; kept alongside `convertEnsembl2Entrez()` as the other KEGG-specific
#'   lookup helper.
#'
#' @param pathwayDescription The pathway's description/display name (e.g.
#'   "Cell cycle"), as shown in enrichment results.
#' @param Species The Ensembl dataset name for the species, used to locate
#'   the species' pathway file among `gmtFiles`.
#' @param GO The gene-set category prefix (e.g. "KEGG") the match must
#'   belong to, so descriptions aren't confused across categories.
#' @param selectOrg The selected species ID, or the "BestMatch" sentinel;
#'   when not "BestMatch", the pathway file is re-derived from `selectOrg`
#'   directly instead of from `Species`.
#'
#' @return The bare KEGG pathway ID (e.g. "hsa04110"), or `NULL` if no
#'   unique match is found.
#'
#' @noRd
keggPathwayID <- function(pathwayDescription, Species, GO, selectOrg) {
  ix <- grep(Species, gmtFiles)

  if (length(ix) == 0) {
    return(NULL)
  }

  # If selected species is not the default "bestMatch", use that species directly
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(NULL)
    }
    totalGenes <- orgInfo[which(orgInfo$id == as.numeric(selectOrg)), 7]
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)

  # change Parkinson's disease to Parkinson\'s disease    otherwise SQL
  pathwayDescription <- gsub("\'", "\'\'", pathwayDescription)

  pathwayInfo <- dbGetQuery(pathway, paste(" select * from pathwayInfo where description =  '",
    pathwayDescription, "' AND name LIKE '", GO, "%'",
    sep = ""
  ))
  dbDisconnect(pathway)
  if (dim(pathwayInfo)[1] != 1) {
    return(NULL)
  }
  # pathwayInfo's name is like "path:hsa04110"; strip the "path:" prefix and
  # anything after the underscore to get the bare pathway ID
  tem <- gsub(".*:", "", pathwayInfo[1, 2])
  return(gsub("_.*", "", tem))
}
