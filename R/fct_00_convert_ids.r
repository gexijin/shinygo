#' convertID
#'
#' @description Converts a raw, user-pasted list of gene/protein IDs into
#'   Ensembl gene IDs and determines the species they belong to. When
#'   `selectOrg` is the "BestMatch" sentinel, queries the shared convertIDs
#'   database across all species to guess the species and ID type from the
#'   pasted IDs (ranking matches by frequency, with a tie-break that prefers
#'   Ensembl annotation over STRING-db genome annotation). When a specific
#'   species is selected instead, connects to that species' own conversion
#'   database and picks its best-matched ID type.
#'
#' @param query Character string of raw user-pasted gene/protein IDs,
#'   separated by whitespace, tabs, newlines, commas, or semicolons.
#' @param selectOrg The selected species ID, or the "BestMatch" sentinel
#'   (`speciesChoice[[1]]`) to let the function guess the species instead.
#'
#' @return `NULL` if `selectOrg` is `NULL` or none of the query IDs matched
#'   the convert database. Otherwise a list with `originalIDs` (cleaned user
#'   query IDs), `IDs` (unique matched Ensembl gene IDs), `species` (matched
#'   species row from `orgInfo`, via `findSpeciesById()`), `speciesMatched`
#'   (single-column data frame describing which species/ID type were used),
#'   and `conversionTable` (data frame of `User_input`, `ensembl_gene_id`,
#'   `Species` for every mapped ID).
#'
#' @noRd
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

  # "Best match" path: species not specified, so guess it. Query the shared
  # convert database for every (species, idType) combo the pasted IDs hit,
  # keep only the best-matched idType per species, and rank species by how
  # many IDs they matched. The top-ranked species/idType combo is then used
  # to pull the full ID-to-Ensembl mapping for the query.
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
    # Species already chosen: connect to that species' own conversion
    # database and query it directly, no ranking across species needed.
    # Multiple ID types can still match (e.g. symbol and RefSeq both hit),
    # so keep only whichever idType matched the most query IDs.
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
  # Consolidate whichever branch ran above into the return shape: dedupe on
  # Ensembl ID (not on user ID, since one user ID can legitimately map to
  # several Ensembl IDs), then attach species labels for the conversion table.
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
