
cleanGeneSet <- function(x) {
  # upper case; remove special characters; strip version suffix; then dedupe.
  # Order matters: ENSG00000211459.2 and ENSG00000211459.3 are distinct strings,
  # so deduping first leaves both, and stripping the version afterwards turns
  # them into the same gene listed twice. That inflates the query size used in
  # the hypergeometric test.
  x <- toupper(gsub("\n| ", "", x))
  x <- remove_gene_version(x)
  x <- unique(x)
  x <- x[which(nchar(x) > 1)] # genes should have at least two characters
  return(x)
}


# convert gene IDs to ensembl gene ids and find species
convertID <- function(query, selectOrg) {
  # Solves the issue of app shut down when species is deleted after genes are uploaded.
  if (is.null(selectOrg)) {
    return(NULL)
  }
  query <- gsub("\"|\'", "", query)
  # remove " in gene ids, mess up SQL query
  # remove ' in gene ids
  # |\\.[0-9] remove anything after A35244.1 -> A35244
  #  some gene ids are like Glyma.01G002100

  querySet <- cleanGeneSet(unlist(strsplit(toupper(query), "\t| |\n|\\,|;")))
  # querySet is ensgene data for example, ENSG00000198888, ENSG00000198763, ENSG00000198804
  querSetString <- paste0("('", paste(querySet, collapse = "', '"), "')")
  # ('ENSG00000198888', 'ENSG00000198763', 'ENSG00000198804')

  # use a small set of genes to guess species and idType; to improve speed
  testQueriesString <- querSetString
  if (length(querySet) > 100) {
    testQueries <- sample(querySet, 100)
    testQueriesString <- paste0("('", paste(testQueries, collapse = "', '"), "')")
  }

  if (selectOrg == speciesChoice[[1]]) { # if best match

    # First send a query to determine the species
    query_species <- paste0(
      "SELECT species, idType, COUNT(species)
      as freq FROM
      (SELECT DISTINCT id, species, idType
        FROM mapping WHERE id IN ",
      testQueriesString,
      ")  GROUP BY species,idType"
    )


    species_ranked <- dbGetQuery(convert, query_species)

    if (dim(species_ranked)[1] == 0) {
      return(NULL)
    }

    # for each species only keep the idType with most genes
    species_ranked <- species_ranked[
      order(-species_ranked$freq),
    ]
    species_ranked <- species_ranked[
      !duplicated(species_ranked$species),
    ]

    sortedCounts <- species_ranked$freq
    names(sortedCounts) <- paste(species_ranked$species, species_ranked$idType)
    sortedCounts <- sort(sortedCounts, decreasing = TRUE)

    # Try to use Ensembl instead of STRING-db genome annotation
    if (length(sortedCounts) > 1) { # if more than 1 species matched
      if (sortedCounts[1] <= sortedCounts[2] * 1.1 # if the #1 species and #2 are close
      && as.numeric(gsub(" .*", "", names(sortedCounts[1]))) < 0 #  Ensembl species
      && as.numeric(gsub(" .*", "", names(sortedCounts[2]))) > 0) {
        tem <- sortedCounts[2]
        sortedCounts[2] <- sortedCounts[1]
        names(sortedCounts)[2] <- names(sortedCounts)[1]
        sortedCounts[1] <- tem
        names(sortedCounts)[1] <- names(tem)
      }
    }
    recognized <- names(sortedCounts[1])

    speciesMatched <- sortedCounts
    speciesMatched <- as.data.frame(speciesMatched)
    orgName <- sapply(as.numeric(gsub(" .*", "", names(sortedCounts))), findSpeciesByIdName)
    speciesMatched <- cbind(orgName, speciesMatched)

    if (length(sortedCounts) == 1) { # if only  one species matched
      speciesMatched[1, 1] <- paste(speciesMatched[1, 1], "(", speciesMatched[1, 2], ")", sep = "")
      speciesMatched <- speciesMatched[, 1, drop = FALSE]
    } else { # if more than one species matched
      speciesMatched <- speciesMatched[!duplicated(speciesMatched[, 1]), ] # same species different mapping (ensembl, arayexpress, hpa)
      speciesMatched[, 1] <- as.character(speciesMatched[, 1])
      speciesMatched[, 1] <- paste(speciesMatched[, 1], " (", speciesMatched[, 2], ")", sep = "")
      speciesMatched[1, 1] <- paste(speciesMatched[1, 1], "   ***Used in mapping***  To change, select from above and resubmit query.")
      speciesMatched <- as.data.frame(speciesMatched[, 1])
    }


    querySTMT <- paste0(
      "select distinct id,ens,species,idType from mapping where ",
      " species = '", gsub(" .*", "", recognized), "'",
      " AND idType = '", gsub(".* ", "", recognized), "'",
      " AND id IN ", querSetString
    )

    result <- dbGetQuery(convert, querySTMT)

    if (dim(result)[1] == 0) {
      return(NULL)
    }
  } else { # if species is selected

    querySTMT <- paste0(
      "select distinct id,ens,idType from mapping where id IN ", querSetString
    )

    # connect to the database, this becomes a global variable
    convert_species <- connect_convert_db_org(datapath, selectOrg)
    result <- dbGetQuery(convert_species, querySTMT)
    dbDisconnect(convert_species)

    if (dim(result)[1] == 0) {
      return(NULL)
    } # stop("ID not recognized!")

    # resolve multiple ID types, get the most matched
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
  # if multiple user ids mapped to the same Ensembl id, only keep one.
  result <- result[which(!duplicated(result[, 2])), ] # remove duplicates in ensembl_gene_id

  # If user id maps to multiple Ensembl IDs, keep all of them. Some of them can be non-coding.
  #result <- result[which(!duplicated(result[, 1])), ] # remove duplicates in user ID
  colnames(speciesMatched) <- c("Matched Species (%genes)")
  conversionTable <- result[, 1:2]
  colnames(conversionTable) <- c("User_input", "ensembl_gene_id")
  conversionTable$Species <- sapply(selectOrg, findSpeciesByIdName)

  return(list(
    originalIDs = querySet,
    IDs = unique(result[, 2]),
    species = findSpeciesById(selectOrg),
    # idType = findIDtypeById(result$idType[1] ),
    speciesMatched = speciesMatched,
    conversionTable = conversionTable
  ))
}

geneInfo <- function(converted, selectOrg) {
  if (is.null(converted)) {
    return(as.data.frame("ID not recognized!"))
  } # no ID
  querySet <- converted$IDs

  if (length(querySet) == 0) {
    return(as.data.frame("ID not recognized!"))
  }

  querySTMT <- paste0(
    "select * from geneInfo;"
  )

  # connect to the database, this becomes a global variable
  convert_species <- connect_convert_db_org(datapath, selectOrg)
  x <- dbGetQuery(convert_species, querySTMT)
  dbDisconnect(convert_species)

  # mark duplicated genes; mostly the same genes on pached chromosomes.
  if(as.numeric(selectOrg) > 0) { # if it is a ENSEMBL species
    x <- x |>
      mutate(coding_status = if_else(gene_biotype == "protein_coding", TRUE, FALSE)) |> # TRUE for coding
      mutate(chr_name_length = nchar(chromosome_name)) |>    # chr 20 --> 2
      mutate(entrez_symbol = paste(entrezgene_id, symbol)) |>  # "7105 TSPAN6"
      arrange(entrez_symbol, entrezgene_id, -coding_status, chr_name_length) |>
      mutate(duplicated = duplicated(entrez_symbol)) |> # both entrez and symbol the same?
      #if entrez is missing, does not count
      mutate(duplicated = if_else(is.na(entrezgene_id), FALSE, duplicated)) |>
      select(-c(coding_status, chr_name_length, entrez_symbol)) # clean up
  }
  Set <- match(x$ensembl_gene_id, querySet)
  Set[which(is.na(Set))] <- "Genome"
  Set[which(Set != "Genome")] <- "List"
  # x = cbind(x,Set) } # just for debuging
  return(cbind(x, Set))
}


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


#' Remove version numbers from Ensembl and RefSeq IDs
#'
#' Takes gene, transcript, or protein IDs with version numbers and removes the
#' version suffix to return just the base ID. Supports Ensembl IDs (genes,
#' transcripts, proteins) and RefSeq IDs (mRNA, protein, ncRNA). Uses vectorized
#' operations for efficient processing of multiple IDs.
#'
#' Supported ID patterns:
#' - Ensembl: ENS + characters + 11 digits + optional version
#'   Format: ENS[A-Z]+########### where:
#'   - ENS = prefix (case-insensitive)
#'   - [A-Z]+ = 1+ letters: species code (MUS, DAR, etc.) + type (G, T, P, E)
#'   - ########### = exactly 11 digits
#'   - .# = optional 1-2 digit version
#'   Examples: ENSG00000211459.2, ENSMUSG00000025902.5, ENST00000456328.2
#' - RefSeq: [prefix]_######.# where:
#'   - [prefix] = NM (mRNA), NP (protein), NR (ncRNA), XM (predicted mRNA),
#'     XR (predicted ncRNA), XP (predicted protein)
#'   - ###### = 6-9 digits
#'   Examples: NM_000546.5, NP_000537.3, XM_011545467.2, XR_007058843.1
#'
#' Version numbers with 3+ digits are NOT removed (likely not versions).
#' Trailing dots without version numbers are removed.
#' Prefix matching is case-insensitive.
#' Non-matching strings are returned unchanged.
#'
#' @param ensembl_ids A character vector of gene/transcript/protein IDs with or
#'   without version numbers. Supports Ensembl (ENS...) and RefSeq (NM_, NP_,
#'   NR_, XM_, XP_) formats. Strings that don't match are returned unchanged.
#'   Version numbers with 3+ digits are NOT removed. NA values are preserved.
#'
#' @return A character vector of IDs without version numbers. Valid IDs without
#'   versions are returned unchanged. Invalid/non-matching strings are returned
#'   unchanged. IDs with 3+ digit versions are unchanged. Trailing dots are
#'   removed. NA values are preserved.
#'
#' @export
#' @examples
#' # Ensembl gene IDs
#' remove_gene_version("ENSG00000211459.2")  # Human gene
#' # Returns: "ENSG00000211459"
#'
#' remove_gene_version("ENSMUSG00000025902.5")  # Mouse gene
#' # Returns: "ENSMUSG00000025902"
#'
#' # Ensembl transcript and protein IDs
#' remove_gene_version("ENST00000456328.2")  # Transcript
#' # Returns: "ENST00000456328"
#'
#' remove_gene_version("ENSP00000384458.1")  # Protein
#' # Returns: "ENSP00000384458"
#'
#' # RefSeq IDs
#' remove_gene_version("NM_000546.5")  # mRNA
#' # Returns: "NM_000546"
#'
#' remove_gene_version("NP_000537.3")  # Protein
#' # Returns: "NP_000537"
#'
#' remove_gene_version("XR_007058843.1")  # Predicted ncRNA
#' # Returns: "XR_007058843"
#'
#' # Version with 3+ digits is NOT removed
#' remove_gene_version("ENSG00000222222.333")
#' # Returns: "ENSG00000222222.333"
#'
#' # Trailing dot without digits is removed
#' remove_gene_version(c("ENSG00000211459.", "NM_000546."))
#' # Returns: c("ENSG00000211459", "NM_000546")
#'
remove_gene_version <- function(ensembl_ids) {
  # Remove version suffix from valid Ensembl or RefSeq IDs
  # Pattern explanation:
  #   ^           - start of string
  #   (...)       - capture group 1: entire base ID
  #     ([Ee][Nn][Ss][A-Za-z]+\\d{11}) - Ensembl IDs:
  #       [Ee][Nn][Ss] - ENS prefix (case-insensitive)
  #       [A-Za-z]+ - 1+ letters (species code + type: G/T/P/E/etc)
  #       \\d{11} - exactly 11 digits
  #     |         - OR
  #     ([Nn][Mm]_\\d{6,9}) - RefSeq mRNA: NM_ + 6-9 digits
  #     |         - OR
  #     ([Nn][Pp]_\\d{6,9}) - RefSeq protein: NP_ + 6-9 digits
  #     |         - OR
  #     ([Nn][Rr]_\\d{6,9}) - RefSeq ncRNA: NR_ + 6-9 digits
  #     |         - OR
  #     ([Xx][Mm]_\\d{6,9}) - RefSeq predicted mRNA: XM_ + 6-9 digits
  #     |         - OR
  #     ([Xx][Rr]_\\d{6,9}) - RefSeq predicted ncRNA: XR_ + 6-9 digits
  #     |         - OR
  #     ([Xx][Pp]_\\d{6,9}) - RefSeq predicted protein: XP_ + 6-9 digits
  #   \\.          - literal dot
  #   (\\d{1,2})?  - optional 1 or 2 digit version (or 0 for trailing dot)
  #   $           - end of string
  # Replacement: \\1 (captured base ID without version or dot)
  # Non-matching strings are returned unchanged by sub()
  # Uses sub() for vectorized operation - much faster than loops

  # Build regex pattern (split for readability)
  ensembl_pattern <- "([Ee][Nn][Ss][A-Za-z]+\\d{11})"
  refseq_patterns <- paste0(
    "([Nn][Mm]_\\d{6,9})|",  # NM_ (mRNA)
    "([Nn][Pp]_\\d{6,9})|",  # NP_ (protein)
    "([Nn][Rr]_\\d{6,9})|",  # NR_ (non-coding RNA)
    "([Xx][Mm]_\\d{6,9})|",  # XM_ (predicted mRNA)
    "([Xx][Rr]_\\d{6,9})|",  # XR_ (predicted ncRNA)
    "([Xx][Pp]_\\d{6,9})"    # XP_ (predicted protein)
  )
  full_pattern <- paste0(
    "^((", ensembl_pattern, ")|(", refseq_patterns, "))\\.(\\d{1,2})?$"
  )

  sub(full_pattern, "\\1", ensembl_ids)
}

