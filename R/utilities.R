connect_convert_db <- function(datapath = datapath) {
  if (!file.exists(org_info_file)) {
    # download org_info and demo files to current folder
    withProgress(message = "Download demo data and species database", {
      incProgress(0.2)
      file_name <- paste0(db_ver, ".tar.gz")
      options(timeout = 300)
      download.file(
        url = paste0(db_url, db_ver, "/", file_name),
        destfile = file_name,
        mode = "wb",
        quiet = FALSE
      )
      untar(file_name) # untar and unzip the files
      file.remove(file_name) # delete the tar file to save storage
    })
  }

  return(DBI::dbConnect(
    drv = RSQLite::dbDriver("SQLite"),
    dbname = org_info_file,
    flags = RSQLite::SQLITE_RO
  ))
}


#' Connect to the convertIDs database for the species and return the
#' objects.
#'
#' Create a database connection with the DBI package.
#'
#' @param datapath Folder path to the data file
#' @param select_org The slected species
#' @param idep_data  Data object that includes org_info
#'
#' @export
#' @return Database connection.
connect_convert_db_org <- function(datapath = datapath, select_org) {
  ix <- which(orgInfo$id == select_org)
  db_file <- orgInfo[ix, "file"]
  return(try(
    DBI::dbConnect(
      drv = RSQLite::dbDriver("SQLite"),
      dbname = paste0(datapath, "db/", db_file),
      flags = RSQLite::SQLITE_RO
    )
  ))
}




cleanGeneSet <- function(x) {
  # remove duplicate; upper case; remove special characters
  x <- unique(toupper(gsub("\n| ", "", x)))
  x <- x[which(nchar(x) > 1)] # genes should have at least two characters
  return(x)
}

# read GMT files, does NO cleaning. Assumes the GMT files are created with cleanGeneSet()
readGMT <- function(fileName) {
  x <- scan(fileName, what = "", sep = "\n")
  x <- strsplit(x, "\t")
  # Extract the first vector element and set it as the list element name
  names(x) <- sapply(x, `[[`, 1)
  x <- lapply(x, `[`, -c(1, 2)) # 2nd element is comment, ignored
  x <- x[which(sapply(x, length) > 1)] # gene sets smaller than 1 is ignored!!!
  return(x)
}


# This function convert gene set names
# x="GOBP_mmu_mgi_GO:0000183_chromatin_silencing_at_rDNA"
# chromatin silencing at rDNA
proper <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

extract1 <- function(x) {
  words <- unlist(strsplit(x, "_"))
  if (length(words) <= 4) {
    return(gsub("_", " ", x))
  } else {
    words <- words[-c(1:4)]
    return(proper(paste(words, collapse = " ")))
  }
}

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




# Main function. Find a query set of genes enriched with functional category
# For debug:  converted = converted(); gInfo = tem;  GO=input$selectGO; selectOrg=input$selectOrg;  minFDR=input$minFDR; input_maxTerms=input$maxTerms
FindOverlap <- function(converted, gInfo, GO, selectOrg, convertedB = NULL, gInfoB = NULL, minSetSize = 2, maxSetSize = 4000, gene_count_pathwaydb = FALSE) {
  minFDR <- 0.2 # internal cutoff; avoids passing a large number of pathways
  maxTerms <- 1000 # only keep 1000 pathways at the most

  idNotRecognized <- list(
    x = as.data.frame("ID not recognized!"),
    groupings = as.data.frame("ID not recognized!")
  )
  if (is.null(converted)) {
    return(idNotRecognized)
  } # no ID

  querySet <- converted$IDs

  if (!gene_count_pathwaydb) {
    if (!is.null(gInfo)) {
      if (class(gInfo) == "data.frame") {
        if (dim(gInfo)[1] > 1) { # some species does not have geneInfo. STRING
          # only coding
          querySet <- intersect(
            querySet,
            gInfo[which(gInfo$gene_biotype == "protein_coding"), 1]
          )
        }
      }
    }
  }
  if (length(querySet) == 0) {
    return(idNotRecognized)
  }

  ix <- grep(converted$species[1, 1], gmtFiles)
  totalGenes <- converted$species[1, 7]

  errorMessage <- list(
    x = as.data.frame("Annotation file cannot be found"),
    groupings = as.data.frame("Annotation file cannot be found")
  )
  if (length(ix) == 0) {
    return(errorMessage)
  }

  # If selected species is not the default "bestMatch", use that species directly
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(idNotRecognized)
    }
    totalGenes <- orgInfo[which(orgInfo$id == as.numeric(selectOrg)), 7]
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)

  # Generate a list of geneset categories such as "GOBP", "KEGG" from file
  geneSetCategory <- dbGetQuery(pathway, "select distinct * from categories ")
  geneSetCategory <- geneSetCategory[, 1]
  categoryChoices <- setNames(as.list(geneSetCategory), geneSetCategory)
  categoryChoices <- append(setNames("All", "All available gene sets"), categoryChoices)
  # change GOBO to the full description for display
  names(categoryChoices)[match("GOBP", categoryChoices)] <- "GO Biological Process"
  names(categoryChoices)[match("GOCC", categoryChoices)] <- "GO Cellular Component"
  names(categoryChoices)[match("GOMF", categoryChoices)] <- "GO Molecular Function"

  if (GO != "All") {
    sqlQuery <- paste(" select distinct gene,pathwayID from pathway where category='", GO, "'",
      " AND gene IN ('", paste(querySet, collapse = "', '"), "')",
      sep = ""
    )
  } else {
    sqlQuery <- paste(" select distinct gene,pathwayID from pathway where gene IN ('",
      paste(querySet, collapse = "', '"), "')",
      sep = ""
    )
  }

  result <- dbGetQuery(pathway, sqlQuery)

  if (dim(result)[1] == 0) {
    return(list(x = as.data.frame("No matching pathway data find!")))
  }



  # given a pathway id, it finds the overlapped genes, symbol preferred
  sharedGenesPrefered <- function(pathwayID) {
    tem <- result[which(result[, 2] == pathwayID), 1]
    ix <- match(tem, converted$conversionTable$ensembl_gene_id) # convert back to original
    tem2 <- unique(converted$conversionTable$User_input[ix])
    if (!is.null(gInfo)) {
      if (class(gInfo) == "data.frame") {
        if (dim(gInfo)[1] > 1) {
          if (length(unique(gInfo$symbol)) / dim(gInfo)[1] > .7) { # if 70% genes has symbol in geneInfo
            ix <- match(tem, gInfo$ensembl_gene_id)
            tem2 <- unique(gInfo$symbol[ix])
          }
        }
      }
    }
    return(paste(tem2, collapse = " ", sep = ""))
  }

  x0 <- table(result$pathwayID)

  x0 <- as.data.frame(x0[which(x0 >= Min_overlap)]) # remove low overlaps

  errorMessage <- list(
    x = as.data.frame("Too few genes."),
    groupings = as.data.frame("Too few genes.")
  )
  if (dim(x0)[1] <= 2) {
    return(errorMessage)
  } # no data
  colnames(x0) <- c("pathwayID", "overlap")

  pathwayInfo <- dbGetQuery(pathway, paste(" select distinct id,n,description,memo from pathwayInfo where id IN ('",
    paste(x0$pathwayID, collapse = "', '"), "') ",
    sep = ""
  ))


  #  pathwayInfo$description <- hyperText( pathwayInfo$description, pathwayInfo$memo)
  #  pathwayInfo <- pathwayInfo[, -4] # remove memo/URL

  x <- merge(x0, pathwayInfo, by.x = "pathwayID", by.y = "id")

  if (gene_count_pathwaydb) {
    # only keep the query genes that have one pathway match
    # this is for more accurate size of query in P value
    querySet <- unique(result$gene)

    # if not using background genes, calculate total genes using pathwayDB
    if (is.null(convertedB) || is.null(gInfoB)) {
      sql_query <- "SELECT COUNT ( DISTINCT gene ) FROM pathway "

      if (GO != "All") {
        sql_query <- paste(
          sql_query,
          " WHERE category='", GO, "'",
          sep = ""
        )
      }
      totalGenes <- DBI::dbGetQuery(pathway, sql_query)
      totalGenes <- as.integer(totalGenes)

      # totalGenes within the range of 5k to 30k.
      if (totalGenes > 30000) {
        totalGenes <- 30000
      }
      if (totalGenes < 5000) {
        totalGenes <- 5000
      }
    }
  }

  # filtered pathways with enrichment ratio less than one
  # x <- x[ which( x$overlap/ length(querySet) / (as.numeric(x$n) / totalGenes ) > 1)  ,]
  x$Pval <- phyper(x$overlap - 1,
    length(querySet),
    totalGenes - length(querySet),
    as.numeric(x$n),
    lower.tail = FALSE
  )
  x$fold <- x$overlap / length(querySet) / (as.numeric(x$n) / totalGenes)
  # further filter by nominal P value
  # x <- subset(x, Pval < 0.2)


  # Background genes----------------------------------------------------
  if (!is.null(convertedB) &&
    !is.null(gInfoB)) { # if more than 30k genes, ignore background genes.
    querySetB <- convertedB$IDs
    if (!is.null(gInfoB)) {
      if (dim(gInfoB)[1] > 1) { # some species does not have geneInfo. STRING
        # only coding
        querySetB <- intersect(
          querySetB,
          gInfoB[which(gInfoB$gene_biotype == "protein_coding"), 1]
        )
      }
    }

    # if background and selected genes matches to different organisms, error
    if (length(intersect(querySetB, querySet)) == 0) { # if none of the selected genes are in background genes
      return(list(x = as.data.frame("None of the selected genes are in the background genes!")))
    }

    querySetB <- unique(c(querySetB, querySet)) # just to make sure the background set includes the query set

    sqlQueryB <- paste(" select distinct gene,pathwayID from pathway where gene IN ('",
      paste(querySetB, collapse = "', '"), "')",
      sep = ""
    )
    sqlQueryB <- paste0(sqlQueryB, " AND pathwayID IN ('", paste(x$pathwayID, collapse = "', '"), "')")

    if (GO != "All") sqlQueryB <- paste0(sqlQueryB, " AND category ='", GO, "'")


    # alternative query. Same order as query genes.
    #   if( GO != "All") {
    #     sqlQueryB = paste( " select distinct gene,pathwayID from pathway where category='", GO, "'",
    #                          " AND gene IN ('", paste(querySetB, collapse="', '"),"')" ,sep="")
    #   } else {
    #     sqlQueryB = paste( " select distinct gene,pathwayID from pathway where gene IN ('",
    #                        paste(querySetB, collapse="', '"),"')" ,sep="")
    #   }
    #       sqlQueryB = paste0(sqlQueryB, " AND pathwayID IN ('", paste(x$pathwayID, collapse="', '"),"')"  )

    resultB <- dbGetQuery(pathway, sqlQueryB)
    if (dim(resultB)[1] == 0) {
      return(list(x = as.data.frame("No matching species or gene ID file!")))
    }
    xB <- table(resultB$pathwayID)
    if (gene_count_pathwaydb) {
      # update querySet, only keep genes with one pathway mapping
      querySetB <- unique(resultB$gene)
    }
    rm(resultB)
    xB <- as.data.frame(xB)
    colnames(xB) <- c("pathwayID", "overlapB")
    x2 <- merge(x, xB, by = "pathwayID", all.x = TRUE)


    x$Pval <- phyper(x2$overlap - 1,
      length(querySet),
      length(querySetB) - length(querySet),
      as.numeric(x2$overlapB), # use the number of genes in background set
      lower.tail = FALSE
    )

    # calculate fold enrichment compared to background

    x$fold <- (x$overlap / length(querySet)) / # ratio in query
      (as.numeric(x2$overlapB) / length(querySetB)) # ratio in background
    if (gene_count_pathwaydb) {
      # number of genes in pathways in background genes
      x$n <- as.numeric(x2$overlapB)
    }
    # write.csv(x2, "pathway_table_bg_go.csv", row.names = F)
  }

  # end background genes------------------------------------------------------------
  x <- x[as.integer(x$n) > minSetSize, ] # filter out smaller geneset
  x <- x[as.integer(x$n) < maxSetSize, ] # filter out big genesets
  if (nrow(x) == 0) {
    return(list(x = as.data.frame("None of the selected genes are in the background genes!")))
  }

  x$FDR <- p.adjust(x$Pval, method = "fdr")

  x <- x[order(x$FDR), ] # sort according to FDR



  # Gene groups for high level GOBP terms
  groups <- dbGetQuery(pathway, paste(" select distinct id, description from pathwayInfo
                       where golevel IN ( '2','3') ", sep = ""))

  ix <- match(groups$id, x0$pathwayID)
  if (length(groups) > 0 && length(ix) > 0) groupings <- as.data.frame("No grouping.")
  groups$ngenes <- x0$overlap[ix]
  groups <- groups[which(!is.na(ix)), ]
  groups <- groups[order(-groups$ngenes), ]
  if (max(groups$ngenes) <= 2) {
    groups <- as.data.frame("Too few genes")
  } else {
    groupings <- subset(groups, ngenes > 2) # at least 10 genes
    if (dim(groups)[1] > 100) groups <- groups[1:100, ]
    groups <- cbind(groups, sapply(groups$id, sharedGenesPrefered))
    groups <- groups[, -1]
    groups <- groups[, c(2, 1, 3)]
    colnames(groups) <- c("N", "High level GO category", "Genes")
  }

  if (min(x$FDR, na.rm = TRUE) > minFDR) {
    x <- as.data.frame("No significant enrichment found!")
  } else {
    x <- x[which(x$FDR < minFDR), ]

    x <- cbind(x, sapply(x$pathwayID, sharedGenesPrefered))

    colnames(x)[9] <- "Genes"
    x$n <- as.numeric(x$n) # convert total genes from character to numeric 10/21/19
    x <- subset(x, select = c(FDR, overlap, n, fold, description, memo, Genes))
    x <- x[order(x$FDR), ] # sort by FDR   4/1/2022 related to issue 23
    x <- x[!duplicated(x$description), ] # remove duplicates   4/1/2022
    colnames(x) <- c("Enrichment FDR", "nGenes", "Pathway Genes", "Fold Enrichment", "Pathway", "URL", "Genes")



    # only keep 1000 pathways at the most
    if (dim(x)[1] > maxTerms) x <- x[1:maxTerms, ]
  }

  dbDisconnect(pathway)
  return(list(x = x, groupings = groups, categoryChoices = categoryChoices))
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

# Given a KEGG pathway description, found pathway ids
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
  tem <- gsub(".*:", "", pathwayInfo[1, 2])
  return(gsub("_.*", "", tem))
}

# not working, for updating GO cateory choices
gmtCategory <- function(converted, selectOrg) {
  idNotRecognized <- as.data.frame("ID not recognized!")
  if (is.null(converted)) {
    return(idNotRecognized)
  } # no ID
  querySet <- converted$IDs
  if (length(querySet) == 0) {
    return(idNotRecognized)
  }
  ix <- grep(converted$species[1, 1], gmtFiles)
  if (length(ix) == 0) {
    return(idNotRecognized)
  }

  # If selected species is not the default "bestMatch", use that species directly
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(idNotRecognized)
    }
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)
  # cat(paste("selectOrg:",selectOrg) )
  # Generate a list of geneset categories such as "GOBP", "KEGG" from file
  geneSetCategory <- dbGetQuery(pathway, "select distinct * from categories ")
  geneSetCategory <- sort(geneSetCategory[, 1])
  categoryChoices <- setNames(as.list(geneSetCategory), geneSetCategory)
  categoryChoices <- append(setNames("All", "All available gene sets"), categoryChoices)

  # move one element to the 2nd place
  move1 <- function(i) c(categoryChoices[1], categoryChoices[i], categoryChoices[-c(1, i)])
  i <- which(names(categoryChoices) == "KEGG")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOMF")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOCC")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOBP")
  categoryChoices <- move1(i)
  # change GOBP to the full description for display
  names(categoryChoices)[match("GOBP", categoryChoices)] <- "GO Biological Process"
  names(categoryChoices)[match("GOCC", categoryChoices)] <- "GO Cellular Component"
  names(categoryChoices)[match("GOMF", categoryChoices)] <- "GO Molecular Function"

  dbDisconnect(pathway)
  return(categoryChoices)
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

#' Remove Pathway ID from pathway name
#' Only for GO and KEGG pathways
#'
#' Path:hsa00270 Cysteine and methionine metabolism
#'           --> Cysteine and methionine metabolism
#'
#' @param strings a vector of strings
#' @param select_go   GOBP, GOCC, GOMP or KEGG or something else
#'
#' @export
#' @return a vector of strings
#'
#' @family pathway functions
remove_pathway_id <- function(strings, select_go) {
  if (is.null(strings)) {
    return(NULL)
  } else {
    if (select_go %in% c("GOBP", "GOCC", "GOMF", "KEGG")) {
      strings <- sub(
        "^\\S+\\s",
        "",
        strings
      )
      strings <- proper(strings)
    }
    return(strings)
  }
}


#' Mark Duplicate Strings with Occurrence Index
#'
#' This function takes a character vector and appends an occurrence index
#' to each duplicated string, starting from 1 for the first occurrence. 
#' Strings that only appear once are left unchanged.
#'
#' If the input is not a character vector with at least two elements,
#' the function returns the input object unchanged.
#'
#' @param strings A character vector of strings.
#' 
#' @return A character vector where each duplicated string has an occurrence index
#' appended (e.g., "aa 1", "aa 2"), while unique strings remain unchanged.
#' If the input is not a valid character vector, the same input object is returned.
#' 
#' @examples
#' strings <- c("aa", "bb", "aa", "cc", "aa")
#' mark_duplicates(strings)
#' # Expected output: "aa 1" "bb" "aa 2" "cc" "aa 3"
#'
#' @export
mark_duplicates <- function(strings) {
  # Check if input is a character vector with at least two elements
  if (!is.character(strings) || length(strings) < 2) {
    return(strings)
  }
  
  # Create an index for each occurrence within unique string groups
  counts <- ave(seq_along(strings), strings, FUN = seq_along)
  
  # Check if a string appears more than once, and append the count if so
  result <- ifelse(table(strings)[strings] > 1, paste(strings, counts), strings)
  
  return(result)
}

hyperText <- function(textVector, urlVector) {
  # for generating pathway lists that can be clicked.
  # Function that takes a vector of strings and a vector of URLs
  # and generate hyper text
  # add URL to Description
  # see https://stackoverflow.com/questions/30901027/convert-a-column-of-text-urls-into-active-hyperlinks-in-shiny
  # see https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
  if (sum(is.null(urlVector)) == length(urlVector)) {
    return(textVector)
  }

  if (length(textVector) != length(urlVector)) {
    return(textVector)
  }

  #------------------URL correction
  # URL changed from http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=GO:0000077
  #                  http://amigo.geneontology.org/amigo/term/GO:0000077
  urlVector <- gsub("cgi-bin/amigo/term_details\\?term=", "amigo/term/", urlVector)
  urlVector <- gsub(" ", "", urlVector)


  # first see if URL is contained in memo
  ix <- grepl("http:", urlVector, ignore.case = TRUE)
  if (sum(ix) > 0) { # at least one has http?
    tem <- paste0(
      "<a href='",
      urlVector, "' target='_blank'>",
      textVector,
      "</a>"
    )
    # only change the ones with URL
    textVector[ix] <- tem[ix]
  }
  return(textVector)
}




# a program for ploting enrichment results by highlighting the similarities among terms
# must have columns: Direction, adj.Pval   Pathways Genes
#  Direction	adj.Pval	nGenes	Pathways		Genes
# Down regulated	3.58E-59	131	Ribonucleoprotein complex biogenesis	36	Nsun5 Nhp2 Rrp15
# Down regulated	2.55E-57	135	NcRNA metabolic process	23	Nsun5 Nhp2 Rrp15 Emg1 Ddx56 Rsl1d1 enrichmentPlot <- function( enrichedTerms){
# Up or down regulation is color-coded
# gene set size if represented by the size of marker
enrichmentPlot <- function(enrichedTerms, rightMargin = 33) {
  if (class(enrichedTerms) != "data.frame") {
    return(NULL)
  }
  if (nrow(enrichedTerms) <= 1) {
    return(NULL)
  } # only one term or less
  library(dendextend) # customizing tree

  geneLists <- lapply(enrichedTerms$Genes, function(x) unlist(strsplit(as.character(x), " ")))
  names(geneLists) <- enrichedTerms$Pathways

  # compute overlaps percentage--------------------

  n <- length(geneLists)
  w <- matrix(NA, nrow = n, ncol = n)
  # compute overlaps among all gene lists
  for (i in 1:n) {
    for (j in i:n) {
      u <- unlist(geneLists[i])
      v <- unlist(geneLists[j])
      w[i, j] <- length(intersect(u, v)) / length(unique(c(u, v)))
    }
  }
  # the lower half of the matrix filled in based on symmetry
  for (i in 1:n) {
    for (j in 1:(i - 1)) {
      w[i, j] <- w[j, i]
    }
  }


  # compute overlaps P value---------------------
  if (0) {
    total_elements <- 30000
    n <- length(geneLists)
    w <- matrix(rep(0, n * n), nrow = n, ncol = n)
    # compute overlaps among all gene lists
    for (i in 1:n) {
      for (j in (i + 1):n) {
        u <- unlist(geneLists[i])
        v <- unlist(geneLists[j])
        xx <- length(intersect(u, v))
        if (xx == 0) {
          next
        }
        mm <- length(u)
        nn <- total_elements - mm
        kk <- length(v)
        w[i, j] <- -sqrt(-phyper(xx - 1, mm, nn, kk, lower.tail = FALSE, log.p = TRUE))
      }
    }


    # the lower half of the matrix filled in based on symmetry
    for (i in 1:n) {
      for (j in 1:(i - 1)) {
        w[i, j] <- w[j, i]
      }
    }

    # w =  w-min(w)
    # for( i in 1:n) 		w[i,i] = 0;
  }

  Terms <- paste(
    sprintf("%-2.1e", as.numeric(enrichedTerms$adj.Pval)),
    names(geneLists)
  )
  rownames(w) <- Terms
  colnames(w) <- Terms
  par(mar = c(0, 0, 1, rightMargin)) # a large margin for showing

  dend <- as.dist(1 - w) %>%
    hclust(method = "average")
  ix <- dend$order # permutated order of leaves

  leafType <- as.factor(gsub(" .*", "", enrichedTerms$Direction[ix]))
  # if(length(unique(enrichedTerms$Direction)  ) <=2 )
  if (max(nchar(enrichedTerms$Direction[ix])) >= 1) { # if "Up regulated or Downregulated"; not "A", "B"
    # leafColors = c("green","red")  else  # mycolors # k-Means
    leafColors <- mycolors[1:2]
  } else { # convert c("B","D","E") to c(2, 4, 5)
    # leafType= as.factor( gsub(" .*","", enrichedTerms$Direction[ix] ) )
    leafType <- match(gsub(" .*", "", enrichedTerms$Direction[ix]), toupper(letters))

    leafColors <- mycolors
  }
  # leafSize = unlist( lapply(geneLists,length) ) # leaf size represent number of genes
  # leafSize = sqrt( leafSize[ix] )
  leafSize <- -log10(as.numeric(enrichedTerms$adj.Pval[ix])) # leaf size represent P values
  leafSize <- .9 * (leafSize - min(leafSize)) / (max(leafSize) - min(leafSize) + 1e-50) + .1 # scale more aggressively
  # leafSize = 1.*(leafSize)/max( leafSize ) + .1   # ratio scaling, less agressive


  dend %>%
    as.dendrogram(hang = -1) %>%
    set("leaves_pch", 19) %>% # type of marker
    set("leaves_cex", leafSize) %>% # Size
    set("leaves_col", leafColors[leafType]) %>% # up or down genes
    plot(horiz = TRUE, axes = FALSE)

  return(recordPlot())

  # legend("top",pch=19, col=leafColors[1:2],legend=levels(leafType),bty = "n",horiz =T  )
  # add legend using a second layer
  # 	par(lend = 1)           # square line ends for the color legend
  # add_legend("top",pch=19, col=leafColors,legend=levels(leafType),bty = "n",horiz =T )
}


# numChar=100 maximum number of characters
# n=200  maximum number of nodes
# degree.cutoff = 0    Remove node if less connected
# from PPInfer
enrich.net2 <- function(x, gene.set, node.id, node.name = node.id, pvalue,
                        n = 50, numChar = NULL, pvalue.cutoff = 0.05, edge.cutoff = 0.05,
                        degree.cutoff = 0, edge.width = function(x) {
                          5 * x^2
                        }, node.size = function(x) {
                          2.5 * log10(x)
                        }, group = FALSE, group.color = c("green", "red"), group.shape = c(
                          "circle",
                          "square"
                        ), legend.parameter = list("topright"), show.legend = TRUE, plotting = TRUE,
                        layoutButton = 0, ...) {
  library(igraph)
  set.seed(layoutButton)
  x <- data.frame(x, group)
  colnames(x)[length(colnames(x))] <- "Group"
  x <- x[as.numeric(x[, pvalue]) < pvalue.cutoff, ]
  x <- x[order(x[, pvalue]), ]
  n <- min(nrow(x), n)
  if (n == 0) {
    stop("no enriched term found...")
  }
  x <- x[1:n, ]
  index <- match(x[, node.id], names(gene.set))
  geneSets <- list()
  for (i in 1:n) {
    geneSets[[i]] <- gene.set[[index[i]]]
  }
  names(geneSets) <- x[, node.name]
  if (is.null(numChar)) {
    numChar <- max(nchar(as.character(x[, node.name])))
  } else {
    if (length(unique(substr(x[, node.name], 1, numChar))) <
      nrow(x)) {
      numChar <- max(nchar(as.character(x[, node.name])))
      message("Note : numChar is too small.", "\n")
    }
  }
  x[, node.name] <- paste(substr(x[, node.name], 1, numChar),
    ifelse(nchar(as.character(x[, node.name])) > numChar,
      "...", ""
    ),
    sep = ""
  )
  w <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in i:n) {
      u <- unlist(geneSets[i])
      v <- unlist(geneSets[j])
      w[i, j] <- length(intersect(u, v)) / length(unique(c(
        u,
        v
      )))
    }
  }
  list.edges <- stack(data.frame(w))
  list.edges <- cbind(list.edges[, 1], rep(
    x[, node.name],
    n
  ), rep(x[, node.name], each = n))
  list.edges <- list.edges[list.edges[, 2] != list.edges[, 3], ]
  list.edges <- list.edges[!is.na(list.edges[, 1]), ]
  g <- graph.data.frame(list.edges[, -1], directed = FALSE)
  E(g)$width <- edge.width(as.numeric(list.edges[, 1]))
  V(g)$size <- node.size(lengths(geneSets))
  g <- delete.edges(g, E(g)[as.numeric(list.edges[, 1]) < edge.cutoff])
  index.deg <- igraph::degree(g) >= degree.cutoff
  g <- delete.vertices(g, V(g)[!index.deg])
  x <- x[index.deg, ]
  index <- index[index.deg]
  if (length(V(g)) == 0) {
    stop("no categories greater than degree.cutoff...")
  }
  n <- min(nrow(x), n)
  x <- x[1:n, ]
  group.level <- sort(unique(group))
  pvalues <- log10(x[, pvalue] + 1e-200) # causes error when P value is zero
  for (i in 1:length(group.level)) {
    index <- x[, "Group"] == group.level[i]
    V(g)$shape[index] <- group.shape[i]
    group.pvalues <- pvalues[index]
    if (length(group.pvalues) > 0) {
      if (max(group.pvalues) == min(group.pvalues)) {
        V(g)$color[index] <- adjustcolor(group.color[i],
          alpha.f = 0.5
        )
      } else {
        V(g)$color[index] <- sapply(
          1 - .9 * (group.pvalues -
            min(group.pvalues)) / (max(group.pvalues) - min(group.pvalues)),
          function(x) {
            adjustcolor(group.color[i], alpha.f = .1 + x) # change range?
          }
        )
      }
    }
  }
  if (plotting) {
    plot(g, , vertex.label.dist = 1.2, ...)
    if (show.legend) {
      legend.parameter$legend <- group.level
      legend.parameter$text.col <- group.color
      legend.parameter$bty <- "n"
      do.call(legend, legend.parameter)
    }
  }
  return(g)
}

enrichmentNetwork <- function(enrichedTerms, layoutButton = 0, edge.cutoff = 5) {
  geneLists <- lapply(enrichedTerms$Genes, function(x) unlist(strsplit(as.character(x), " ")))
  names(geneLists) <- enrichedTerms$Pathways
  enrichedTerms$Direction <- gsub(" .*", "", enrichedTerms$Direction)

  g <- enrich.net2(enrichedTerms, geneLists,
    node.id = "Pathways", numChar = 100,
    pvalue = "adj.Pval", pvalue.cutoff = 1, degree.cutoff = 0,
    n = 200, group = enrichedTerms$Direction, vertex.label.cex = 1,
    vertex.label.color = "black", show.legend = FALSE,
    layoutButton = layoutButton, edge.cutoff = edge.cutoff
  )
}



