# Gene ID conversion functions
#
# Turns the gene IDs a user pastes (symbols, Entrez, RefSeq, Ensembl, ...)
# into Ensembl gene IDs, and looks up the annotation for those genes.
#
#   convertID()             pasted IDs -> Ensembl gene IDs (+ species guess)
#   cleanGeneSet()          tidy a vector of IDs before lookup
#   remove_gene_version()   ENSG00000211459.2 -> ENSG00000211459
#   geneInfo()              annotation for every gene in a species
#   drop_duplicate_genes()  keep one Ensembl ID per gene
#   convertEnsembl2Entrez() Ensembl gene IDs -> Entrez IDs (for KEGG)
#
# Species IDs come from orgInfo: positive IDs are Ensembl genomes, negative
# IDs are STRING-db genomes.
# ----------------------------------------------------------------------------


# Convert pasted gene IDs to Ensembl gene IDs.
#
# query:     the pasted text; IDs separated by tabs, spaces, newlines, commas
#            or semicolons.
# selectOrg: species ID, or speciesChoice[[1]] ("BestMatch") to guess the
#            species from the IDs.
#
# Returns NULL if nothing maps. Otherwise a list:
#   originalIDs     the cleaned pasted IDs
#   IDs             the matching Ensembl gene IDs
#   species         the species' orgInfo row
#   speciesMatched  one-column data frame describing the species used
#   conversionTable pasted ID -> Ensembl ID -> species name
convertID <- function(query, selectOrg) {
  # NULL when the species was removed after genes were pasted; without this
  # check the app shut down.
  if (is.null(selectOrg)) {
    return(NULL)
  }

  # Quotes in IDs would break the SQL queries below.
  query <- gsub("\"|\'", "", query)

  querySet <- cleanGeneSet(unlist(strsplit(toupper(query), "\t| |\n|\\,|;")))
  # SQL list, e.g. ('ENSG00000198888', 'ENSG00000198763')
  querSetString <- paste0("('", paste(querySet, collapse = "', '"), "')")

  # Guessing the species only needs a sample of the IDs; 100 random ones keep
  # the query fast.
  testQueriesString <- querSetString
  if (length(querySet) > 100) {
    testQueries <- sample(querySet, 100)
    testQueriesString <- paste0("('", paste(testQueries, collapse = "', '"), "')")
  }

  if (selectOrg == .app$speciesChoice[[1]]) { # "BestMatch": guess the species

    # Count matching IDs per species and ID type.
    query_species <- paste0(
      "SELECT species, idType, COUNT(species)
      as freq FROM
      (SELECT DISTINCT id, species, idType
        FROM mapping WHERE id IN ",
      testQueriesString,
      ")  GROUP BY species,idType"
    )

    species_ranked <- dbGetQuery(.app$convert, query_species)

    if (dim(species_ranked)[1] == 0) {
      return(NULL)
    }

    # For each species, keep only the ID type that matched the most genes.
    species_ranked <- species_ranked[
      order(-species_ranked$freq),
    ]
    species_ranked <- species_ranked[
      !duplicated(species_ranked$species),
    ]

    # Named "<species> <idType>", best match first.
    sortedCounts <- species_ranked$freq
    names(sortedCounts) <- paste(species_ranked$species, species_ranked$idType)
    sortedCounts <- sort(sortedCounts, decreasing = TRUE)

    # Prefer Ensembl over STRING-db: if the best match is a STRING-db genome
    # (negative ID) and the runner-up is an Ensembl genome (positive ID) with
    # nearly as many matches (within 10%), swap them.
    if (length(sortedCounts) > 1) {
      if (sortedCounts[1] <= sortedCounts[2] * 1.1
      && as.numeric(gsub(" .*", "", names(sortedCounts[1]))) < 0
      && as.numeric(gsub(" .*", "", names(sortedCounts[2]))) > 0) {
        tem <- sortedCounts[2]
        sortedCounts[2] <- sortedCounts[1]
        names(sortedCounts)[2] <- names(sortedCounts)[1]
        sortedCounts[1] <- tem
        names(sortedCounts)[1] <- names(tem)
      }
    }
    recognized <- names(sortedCounts[1])

    # Text shown to the user: each matched species with its gene count.
    speciesMatched <- sortedCounts
    speciesMatched <- as.data.frame(speciesMatched)
    orgName <- sapply(as.numeric(gsub(" .*", "", names(sortedCounts))), findSpeciesByIdName)
    speciesMatched <- cbind(orgName, speciesMatched)

    if (length(sortedCounts) == 1) {
      speciesMatched[1, 1] <- paste(speciesMatched[1, 1], "(", speciesMatched[1, 2], ")", sep = "")
      speciesMatched <- speciesMatched[, 1, drop = FALSE]
    } else {
      # The same species can appear more than once, from different mapping
      # sources (Ensembl, ArrayExpress, HPA).
      speciesMatched <- speciesMatched[!duplicated(speciesMatched[, 1]), ]
      speciesMatched[, 1] <- as.character(speciesMatched[, 1])
      speciesMatched[, 1] <- paste(speciesMatched[, 1], " (", speciesMatched[, 2], ")", sep = "")
      speciesMatched[1, 1] <- paste(speciesMatched[1, 1], "   ***Used in mapping***  To change, select from above and resubmit query.")
      speciesMatched <- as.data.frame(speciesMatched[, 1])
    }

    # Map all the IDs, using the chosen species and ID type.
    querySTMT <- paste0(
      "select distinct id,ens,species,idType from mapping where ",
      " species = '", gsub(" .*", "", recognized), "'",
      " AND idType = '", gsub(".* ", "", recognized), "'",
      " AND id IN ", querSetString
    )

    result <- dbGetQuery(.app$convert, querySTMT)

    if (dim(result)[1] == 0) {
      return(NULL)
    }
  } else { # a species was selected: map with that species' own database

    querySTMT <- paste0(
      "select distinct id,ens,idType from mapping where id IN ", querSetString
    )

    convert_species <- connect_convert_db_org(.app$datapath, selectOrg)
    result <- dbGetQuery(convert_species, querySTMT)
    dbDisconnect(convert_species)

    if (dim(result)[1] == 0) {
      return(NULL)
    }

    # IDs can match several ID types; keep the type that matched the most.
    bestIDtype <- as.integer(
      names(
        sort(
          table(result$idType),
          decreasing = TRUE
        )
      )[1]
    )
    result <- result[result$idType == bestIDtype, ]

    speciesMatched <- as.data.frame(paste("Using selected species ", findSpeciesByIdName(selectOrg)))
  }

  # Several pasted IDs can map to the same Ensembl gene: keep one.
  result <- result[which(!duplicated(result[, 2])), ]
  # One pasted ID can map to several Ensembl genes: keep all of them, as some
  # can be non-coding.

  colnames(speciesMatched) <- c("Matched Species (%genes)")
  conversionTable <- result[, 1:2]
  colnames(conversionTable) <- c("User_input", "ensembl_gene_id")
  conversionTable$Species <- sapply(selectOrg, findSpeciesByIdName)

  return(list(
    originalIDs = querySet,
    IDs = unique(result[, 2]),
    species = findSpeciesById(selectOrg),
    speciesMatched = speciesMatched,
    conversionTable = conversionTable
  ))
}


# Tidy a vector of gene IDs: upper case, no spaces or newlines, no version
# suffix, no duplicates, at least two characters.
cleanGeneSet <- function(x) {
  # Order matters: ENSG00000211459.2 and ENSG00000211459.3 are distinct strings,
  # so deduping first leaves both, and stripping the version afterwards turns
  # them into the same gene listed twice. That inflates the query size used in
  # the hypergeometric test.
  x <- toupper(gsub("\n| ", "", x))
  x <- remove_gene_version(x)
  x <- unique(x)
  x <- x[which(nchar(x) > 1)]
  return(x)
}


# Remove the version suffix from Ensembl and RefSeq IDs.
#
# Handles Ensembl genes, transcripts and proteins (ENSG..., ENSMUSG...,
# ENST..., ENSP...: "ENS", letters, then 11 digits) and RefSeq IDs (NM_, NP_,
# NR_, XM_, XR_, XP_ followed by 6-9 digits). Matching is case-insensitive.
#
#   remove_gene_version("ENSG00000211459.2")   # "ENSG00000211459"
#   remove_gene_version("NM_000546.5")         # "NM_000546"
#   remove_gene_version("ENSG00000211459.")    # "ENSG00000211459" (trailing dot)
#   remove_gene_version("ENSG00000222222.333") # unchanged: 3+ digits is not a version
#
# Anything else, including NA, is returned unchanged.
remove_gene_version <- function(ensembl_ids) {
  ensembl_pattern <- "([Ee][Nn][Ss][A-Za-z]+\\d{11})"
  refseq_patterns <- paste0(
    "([Nn][Mm]_\\d{6,9})|",  # NM_ (mRNA)
    "([Nn][Pp]_\\d{6,9})|",  # NP_ (protein)
    "([Nn][Rr]_\\d{6,9})|",  # NR_ (non-coding RNA)
    "([Xx][Mm]_\\d{6,9})|",  # XM_ (predicted mRNA)
    "([Xx][Rr]_\\d{6,9})|",  # XR_ (predicted ncRNA)
    "([Xx][Pp]_\\d{6,9})"    # XP_ (predicted protein)
  )
  # The base ID, then a dot and an optional 1-2 digit version; keep the base.
  full_pattern <- paste0(
    "^((", ensembl_pattern, ")|(", refseq_patterns, "))\\.(\\d{1,2})?$"
  )

  sub(full_pattern, "\\1", ensembl_ids)
}


# Annotation for every gene in the species (location, biotype, symbol, ...),
# with a Set column: "List" for the genes in converted$IDs, "Genome" for the
# rest. Returns a one-cell "ID not recognized!" data frame if nothing mapped.
geneInfo <- function(converted, selectOrg) {
  if (is.null(converted)) {
    return(as.data.frame("ID not recognized!"))
  }
  querySet <- converted$IDs

  if (length(querySet) == 0) {
    return(as.data.frame("ID not recognized!"))
  }

  querySTMT <- paste0(
    "select * from geneInfo;"
  )

  convert_species <- connect_convert_db_org(.app$datapath, selectOrg)
  x <- dbGetQuery(convert_species, querySTMT)
  dbDisconnect(convert_species)

  # Ensembl genomes only: mark duplicated genes, mostly the same gene on
  # patched chromosomes. A gene is a duplicate if its Entrez ID and symbol both
  # match an earlier one; sorting first keeps the protein-coding copy on the
  # main chromosome (shortest chromosome name). Genes without an Entrez ID are
  # never marked.
  if(as.numeric(selectOrg) > 0) {
    x <- x |>
      mutate(coding_status = if_else(gene_biotype == "protein_coding", TRUE, FALSE)) |>
      mutate(chr_name_length = nchar(chromosome_name)) |>    # "20" -> 2
      mutate(entrez_symbol = paste(entrezgene_id, symbol)) |>  # "7105 TSPAN6"
      arrange(entrez_symbol, entrezgene_id, -coding_status, chr_name_length) |>
      mutate(duplicated = duplicated(entrez_symbol)) |>
      mutate(duplicated = if_else(is.na(entrezgene_id), FALSE, duplicated)) |>
      select(-c(coding_status, chr_name_length, entrez_symbol))
  }
  Set <- match(x$ensembl_gene_id, querySet)
  Set[which(is.na(Set))] <- "Genome"
  Set[which(Set != "Genome")] <- "List"
  return(cbind(x, Set))
}


# Keep one Ensembl ID per gene: drop the IDs that geneInfo() marks as
# duplicated (mostly the same gene on patched chromosomes). gene_info must be
# geneInfo()'s full table for an Ensembl species, with its `duplicated` column.
# The IDs come back in gene_info's order. The conversion table is not changed.
drop_duplicate_genes <- function(ids, gene_info) {
  gene_info |>
    filter(!duplicated) |>
    filter(ensembl_gene_id %in% ids) |>
    pull(ensembl_gene_id)
}


# Map Ensembl gene IDs to Entrez gene IDs, for the KEGG diagrams.
#
# query:   Ensembl gene IDs, as a vector or as text.
# Species: the species' ensembl_dataset name (e.g. "hsapiens_gene_ensembl"),
#          not its orgInfo ID.
#
# Returns NULL if nothing maps, otherwise a data frame with columns
# entrezgene_id and ensembl_gene_id.
convertEnsembl2Entrez <- function(query, Species) {
  speciesID <- .app$orgInfo$id[which(.app$orgInfo$ensembl_dataset == Species)]
  convert_species <- connect_convert_db_org(.app$datapath, speciesID)
  # Find which ID type number this species' database uses for Entrez IDs.
  idType_Entrez <- dbGetQuery(convert_species, paste("select distinct * from idIndex where idType = 'entrezgene_id'"))
  if (dim(idType_Entrez)[1] != 1) {
    cat("Warning! entrezgene ID not found!")
  }
  idType_Entrez <- as.numeric(idType_Entrez[1, 1])

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
