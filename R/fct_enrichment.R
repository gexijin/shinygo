# Enrichment analysis functions
#
# Find the pathways (GO terms, KEGG, ...) that are enriched in the user's
# genes, and prepare pathway names for display.
#
#   FindOverlap()        the enrichment analysis
#   gmtCategory()        pathway databases available for a species (the menu)
#   hyperText()          pathway names -> clickable links
#   remove_pathway_id()  "Path:hsa00270 Cysteine ..." -> "Cysteine ..."
#   proper()             capitalise the first letter
#   mark_duplicates()    c("aa", "aa") -> c("aa 1", "aa 2")
#
# Not used anywhere: readGMT(), extract1() and keggPathwayID().
#
# Each species has its own pathway database (an SQLite file listed in
# gmtFiles), with tables pathway (gene -> pathway), pathwayInfo (name, size,
# URL, GO level) and categories (GOBP, KEGG, ...).
# ----------------------------------------------------------------------------


# Enrichment analysis: which pathways contain more of the user's genes than
# expected by chance (hypergeometric test, FDR-corrected).
#
# converted, gInfo:   the user's genes, from convertID() and geneInfo().
# GO:                 pathway database ("GOBP", "KEGG", ...) or "All".
# selectOrg:          species ID, or "BestMatch" to use the species convertID()
#                     found.
# convertedB, gInfoB: optional background genes, in the same form. Without
#                     them, all genes in the genome are the background.
# minSetSize, maxSetSize: keep pathways with more than minSetSize and fewer
#                     than maxSetSize genes.
# gene_count_pathwaydb: count only genes that are in at least one pathway of
#                     the chosen database ("Use pathway DB for gene counts").
# genes_column:       what the Genes column shows: "symbol", "input" (the
#                     pasted IDs) or "ensembl".
#
# Returns a list:
#   x               pathways with FDR < 0.2 (at most 1000), sorted by FDR:
#                   Enrichment FDR, nGenes, Pathway Genes, Fold Enrichment,
#                   Pathway, URL, Genes, Membership (Ensembl IDs, for the
#                   tree and network)
#   groupings       the user's genes grouped by high-level GO terms (Groups tab)
#   categoryChoices the species' pathway databases (not used by the app)
# If something goes wrong, x (and sometimes groupings) is instead a one-cell
# data frame holding a message such as "ID not recognized!".
FindOverlap <- function(converted, gInfo, GO, selectOrg, convertedB = NULL, gInfoB = NULL, minSetSize = 2, maxSetSize = 4000, gene_count_pathwaydb = FALSE, genes_column = "symbol") {
  # genes_column controls what the Genes column holds: "symbol" (gene symbols
  # where the database has one, otherwise the pasted ID), "input" (the IDs the
  # user pasted) or "ensembl". NULL can arrive before the input has rendered.
  if (is.null(genes_column) || !genes_column %in% c("symbol", "input", "ensembl")) {
    genes_column <- "symbol"
  }
  minFDR <- 0.2 # internal cutoff; avoids passing a large number of pathways
  maxTerms <- 1000 # only keep 1000 pathways at the most

  idNotRecognized <- list(
    x = as.data.frame("ID not recognized!"),
    groupings = as.data.frame("ID not recognized!")
  )
  if (is.null(converted)) {
    return(idNotRecognized)
  }

  # The query: the user's genes, protein-coding only. STRING-db species have
  # no gene info, so they keep all genes.
  querySet <- converted$IDs

  if (!gene_count_pathwaydb) {
    if (!is.null(gInfo)) {
      if (class(gInfo) == "data.frame") {
        if (dim(gInfo)[1] > 1) {
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

  # Find the species' pathway database; totalGenes (orgInfo's 7th column) is
  # its number of genes, the default background size.
  ix <- grep(converted$species[1, 1], gmtFiles)
  totalGenes <- converted$species[1, 7]

  errorMessage <- list(
    x = as.data.frame("Annotation file cannot be found"),
    groupings = as.data.frame("Annotation file cannot be found")
  )
  if (length(ix) == 0) {
    return(errorMessage)
  }

  # If a species was selected (not "BestMatch"), use it directly.
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(idNotRecognized)
    }
    totalGenes <- orgInfo[which(orgInfo$id == as.numeric(selectOrg)), 7]
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)

  # The species' pathway databases ("GOBP", "KEGG", ...), with display names.
  geneSetCategory <- dbGetQuery(pathway, "select distinct * from categories ")
  geneSetCategory <- geneSetCategory[, 1]
  categoryChoices <- setNames(as.list(geneSetCategory), geneSetCategory)
  categoryChoices <- append(setNames("All", "All available gene sets"), categoryChoices)
  names(categoryChoices)[match("GOBP", categoryChoices)] <- "GO Biological Process"
  names(categoryChoices)[match("GOCC", categoryChoices)] <- "GO Cellular Component"
  names(categoryChoices)[match("GOMF", categoryChoices)] <- "GO Molecular Function"

  # Every (gene, pathway) pair for the query genes, in the chosen database.
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

  # A pathway's query genes, as Ensembl IDs. This is the real membership, used
  # by the tree, the network and the redundancy filter. sharedGenesPrefered()
  # below formats the same genes for display; nothing should parse that string
  # to recover membership, or display formatting would change the analysis.
  pathwayMembers <- function(pathwayID) {
    result[which(result[, 2] == pathwayID), 1]
  }

  # A pathway's query genes as display text (the Genes column), in the form
  # chosen by genes_column, separated by spaces.
  sharedGenesPrefered <- function(pathwayID) {
    # result comes from "select distinct gene, pathwayID", so this is already
    # one row per gene; keep exactly one token per gene.
    ens <- result[which(result[, 2] == pathwayID), 1]

    pasted <- converted$conversionTable$User_input[
      match(ens, converted$conversionTable$ensembl_gene_id)
    ]

    out <- if (genes_column == "ensembl") ens else pasted

    if (genes_column == "symbol") {
      # Substitute per gene rather than wholesale: species like maize have
      # symbols for only some genes, and the rest are blank.
      if (!is.null(gInfo)) {
        if (is.data.frame(gInfo)) {
          if (nrow(gInfo) > 1) {
            # geneInfo pads every symbol with a leading space (" A1BG"), so a
            # gene with no symbol is stored as " ", not "". Trim it, or nzchar()
            # below treats that space as a real symbol.
            sym <- trimws(gInfo$symbol[match(ens, gInfo$ensembl_gene_id)])
            has_symbol <- !is.na(sym) & nzchar(sym)
            out[has_symbol] <- sym[has_symbol]
          }
        }
      }
    }

    # Last resort, so a gene is never represented by an empty token. Trim here
    # too: pasted ids and Ensembl ids are clean, but a whitespace-only value
    # from any source would otherwise slip through as a blank token.
    out <- trimws(out)
    unnamed <- is.na(out) | !nzchar(out)
    out[unnamed] <- ens[unnamed]

    return(paste(out, collapse = " "))
  }

  # Query genes per pathway (Min_overlap is set in global.R).
  x0 <- table(result$pathwayID)

  x0 <- as.data.frame(x0[which(x0 >= Min_overlap)])

  errorMessage <- list(
    x = as.data.frame("Too few genes."),
    groupings = as.data.frame("Too few genes.")
  )
  if (dim(x0)[1] <= 2) {
    return(errorMessage)
  }
  colnames(x0) <- c("pathwayID", "overlap")

  # Add each pathway's size (n), name and URL (memo).
  pathwayInfo <- dbGetQuery(pathway, paste(" select distinct id,n,description,memo from pathwayInfo where id IN ('",
    paste(x0$pathwayID, collapse = "', '"), "') ",
    sep = ""
  ))

  x <- merge(x0, pathwayInfo, by.x = "pathwayID", by.y = "id")

  # "Use pathway DB for gene counts": count only genes that are in at least
  # one pathway, for both the query and (without background genes) the total,
  # which is kept between 5,000 and 30,000.
  if (gene_count_pathwaydb) {
    querySet <- unique(result$gene)

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

      if (totalGenes > 30000) {
        totalGenes <- 30000
      }
      if (totalGenes < 5000) {
        totalGenes <- 5000
      }
    }
  }

  # P value and fold enrichment against the whole genome: the chance of
  # drawing at least `overlap` pathway genes when picking length(querySet)
  # genes from totalGenes.
  x$Pval <- phyper(x$overlap - 1,
    length(querySet),
    totalGenes - length(querySet),
    as.numeric(x$n),
    lower.tail = FALSE
  )
  x$fold <- x$overlap / length(querySet) / (as.numeric(x$n) / totalGenes)


  # Background genes ----------------------------------------------------------
  # With background genes, redo the P value and fold enrichment against them
  # instead of the whole genome.
  if (!is.null(convertedB) &&
    !is.null(gInfoB)) {
    # Protein-coding background genes (all of them for STRING-db species).
    querySetB <- convertedB$IDs
    if (!is.null(gInfoB)) {
      if (dim(gInfoB)[1] > 1) {
        querySetB <- intersect(
          querySetB,
          gInfoB[which(gInfoB$gene_biotype == "protein_coding"), 1]
        )
      }
    }

    # No overlap at all usually means the two lists are from different species.
    if (length(intersect(querySetB, querySet)) == 0) {
      return(list(x = as.data.frame("None of the selected genes are in the background genes!")))
    }

    querySetB <- unique(c(querySetB, querySet)) # the background must include the query genes

    # Background genes per pathway, for the pathways found above.
    sqlQueryB <- paste(" select distinct gene,pathwayID from pathway where gene IN ('",
      paste(querySetB, collapse = "', '"), "')",
      sep = ""
    )
    sqlQueryB <- paste0(sqlQueryB, " AND pathwayID IN ('", paste(x$pathwayID, collapse = "', '"), "')")

    if (GO != "All") sqlQueryB <- paste0(sqlQueryB, " AND category ='", GO, "'")

    resultB <- dbGetQuery(pathway, sqlQueryB)
    if (dim(resultB)[1] == 0) {
      return(list(x = as.data.frame("No matching species or gene ID file!")))
    }
    xB <- table(resultB$pathwayID)
    if (gene_count_pathwaydb) {
      # count only background genes that are in at least one pathway
      querySetB <- unique(resultB$gene)
    }
    rm(resultB)
    xB <- as.data.frame(xB)
    colnames(xB) <- c("pathwayID", "overlapB")
    x2 <- merge(x, xB, by = "pathwayID", all.x = TRUE)

    # The pathway's size is now its number of background genes (overlapB).
    x$Pval <- phyper(x2$overlap - 1,
      length(querySet),
      length(querySetB) - length(querySet),
      as.numeric(x2$overlapB),
      lower.tail = FALSE
    )

    x$fold <- (x$overlap / length(querySet)) / # ratio in query
      (as.numeric(x2$overlapB) / length(querySetB)) # ratio in background
    if (gene_count_pathwaydb) {
      x$n <- as.numeric(x2$overlapB)
    }
  }
  # End of background genes --------------------------------------------------

  # Pathway size limits, then FDR.
  x <- x[as.integer(x$n) > minSetSize, ]
  x <- x[as.integer(x$n) < maxSetSize, ]
  if (nrow(x) == 0) {
    return(list(x = as.data.frame("None of the selected genes are in the background genes!")))
  }

  x$FDR <- p.adjust(x$Pval, method = "fdr")

  x <- x[order(x$FDR), ]

  # Groups tab: the user's genes in high-level GO terms (GO levels 2 and 3),
  # the 100 terms with the most genes. Uses x0, the counts before any
  # filtering. Note: the `groupings` variable set here is never used; the
  # function returns `groups`.
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
    groupings <- subset(groups, ngenes > 2)
    if (dim(groups)[1] > 100) groups <- groups[1:100, ]
    groups <- cbind(groups, sapply(groups$id, sharedGenesPrefered))
    groups <- groups[, -1]
    groups <- groups[, c(2, 1, 3)]
    colnames(groups) <- c("N", "High level GO category", "Genes")
  }

  # The results table: pathways with FDR < minFDR, with their genes.
  if (min(x$FDR, na.rm = TRUE) > minFDR) {
    x <- as.data.frame("No significant enrichment found!")
  } else {
    x <- x[which(x$FDR < minFDR), ]

    x <- cbind(x, sapply(x$pathwayID, sharedGenesPrefered))

    colnames(x)[9] <- "Genes"
    # I() keeps this an AsIs list column so it survives ordering and subsetting
    # below and stays aligned with its row.
    x$Membership <- I(lapply(x$pathwayID, pathwayMembers))
    x$n <- as.numeric(x$n) # stored as text in the database
    x <- subset(x, select = c(FDR, overlap, n, fold, description, memo, Genes, Membership))
    x <- x[order(x$FDR), ]
    x <- x[!duplicated(x$description), ] # same pathway name twice: keep the most significant (issue #23)
    colnames(x) <- c("Enrichment FDR", "nGenes", "Pathway Genes", "Fold Enrichment", "Pathway", "URL", "Genes", "Membership")

    if (dim(x)[1] > maxTerms) x <- x[1:maxTerms, ]
  }

  dbDisconnect(pathway)
  return(list(x = x, groupings = groups, categoryChoices = categoryChoices))
}


# The pathway databases available for the user's species, for the database
# menu: "All", then GO Biological Process, Cellular Component, Molecular
# Function and KEGG, then the rest. Returns a named list (display name ->
# database code), or a one-cell "ID not recognized!" data frame.
gmtCategory <- function(converted, selectOrg) {
  idNotRecognized <- as.data.frame("ID not recognized!")
  if (is.null(converted)) {
    return(idNotRecognized)
  }
  querySet <- converted$IDs
  if (length(querySet) == 0) {
    return(idNotRecognized)
  }
  ix <- grep(converted$species[1, 1], gmtFiles)
  if (length(ix) == 0) {
    return(idNotRecognized)
  }

  # If a species was selected (not "BestMatch"), use it directly.
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(idNotRecognized)
    }
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)
  geneSetCategory <- dbGetQuery(pathway, "select distinct * from categories ")
  geneSetCategory <- sort(geneSetCategory[, 1])
  categoryChoices <- setNames(as.list(geneSetCategory), geneSetCategory)
  categoryChoices <- append(setNames("All", "All available gene sets"), categoryChoices)

  # Move element i to second place, after "All". Moving KEGG, GOMF, GOCC, then
  # GOBP leaves them in the order GOBP, GOCC, GOMF, KEGG.
  move1 <- function(i) c(categoryChoices[1], categoryChoices[i], categoryChoices[-c(1, i)])
  i <- which(names(categoryChoices) == "KEGG")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOMF")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOCC")
  categoryChoices <- move1(i)
  i <- which(names(categoryChoices) == "GOBP")
  categoryChoices <- move1(i)
  # Display names for the GO databases.
  names(categoryChoices)[match("GOBP", categoryChoices)] <- "GO Biological Process"
  names(categoryChoices)[match("GOCC", categoryChoices)] <- "GO Cellular Component"
  names(categoryChoices)[match("GOMF", categoryChoices)] <- "GO Molecular Function"

  dbDisconnect(pathway)
  return(categoryChoices)
}


# Display helpers -------------------------------------------------------------

# Turn pathway names into links that open in a new tab, for the ones whose URL
# starts with http: or https:. Returns textVector unchanged if the URLs are
# missing or don't line up with the names.
# See https://stackoverflow.com/questions/30901027/convert-a-column-of-text-urls-into-active-hyperlinks-in-shiny
# and https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
hyperText <- function(textVector, urlVector) {
  if (sum(is.null(urlVector)) == length(urlVector)) {
    return(textVector)
  }

  if (length(textVector) != length(urlVector)) {
    return(textVector)
  }

  # Update old AmiGO links:
  #   http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=GO:0000077
  #   -> http://amigo.geneontology.org/amigo/term/GO:0000077
  urlVector <- gsub("cgi-bin/amigo/term_details\\?term=", "amigo/term/", urlVector)
  urlVector <- gsub(" ", "", urlVector)

  ix <- grepl("http:|https:", urlVector, ignore.case = TRUE)
  if (sum(ix) > 0) {
    tem <- paste0(
      "<a href='",
      urlVector, "' target='_blank'>",
      textVector,
      "</a>"
    )
    textVector[ix] <- tem[ix]
  }
  return(textVector)
}

# Remove the leading pathway ID from GO and KEGG pathway names, then
# capitalise:
#   "Path:hsa00270 Cysteine and methionine metabolism"
#   -> "Cysteine and methionine metabolism"
# Other databases (select_go not GOBP, GOCC, GOMF or KEGG) are unchanged.
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

# Capitalise the first letter: "chromatin silencing" -> "Chromatin silencing"
proper <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

# Number repeated strings so each is unique; strings that appear once are
# unchanged:
#   mark_duplicates(c("aa", "bb", "aa", "cc", "aa"))
#   # "aa 1" "bb" "aa 2" "cc" "aa 3"
# Anything that isn't a character vector of length 2 or more is returned as is.
mark_duplicates <- function(strings) {
  if (!is.character(strings) || length(strings) < 2) {
    return(strings)
  }

  # Occurrence number of each string: 1 for its first appearance, 2 for its second, ...
  counts <- ave(seq_along(strings), strings, FUN = seq_along)

  result <- ifelse(table(strings)[strings] > 1, paste(strings, counts), strings)

  return(result)
}


# Unused -----------------------------------------------------------------------

# Read a GMT gene set file into a named list of gene IDs. Does no cleaning: the
# files are expected to be made with cleanGeneSet(). The second field of each
# line (a comment) is skipped, and sets with fewer than 2 genes are dropped.
readGMT <- function(fileName) {
  x <- scan(fileName, what = "", sep = "\n")
  x <- strsplit(x, "\t")
  names(x) <- sapply(x, `[[`, 1)
  x <- lapply(x, `[`, -c(1, 2))
  x <- x[which(sapply(x, length) > 1)]
  return(x)
}

# Readable name from a gene set ID:
#   "GOBP_mmu_mgi_GO:0000183_chromatin_silencing_at_rDNA"
#   -> "Chromatin silencing at rDNA"
extract1 <- function(x) {
  words <- unlist(strsplit(x, "_"))
  if (length(words) <= 4) {
    return(gsub("_", " ", x))
  } else {
    words <- words[-c(1:4)]
    return(proper(paste(words, collapse = " ")))
  }
}

# The KEGG pathway ID (e.g. "hsa04110") for a pathway name, or NULL.
keggPathwayID <- function(pathwayDescription, Species, GO, selectOrg) {
  ix <- grep(Species, gmtFiles)

  if (length(ix) == 0) {
    return(NULL)
  }

  # If a species was selected (not "BestMatch"), use it directly.
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], gmtFiles)
    if (length(ix) == 0) {
      return(NULL)
    }
    totalGenes <- orgInfo[which(orgInfo$id == as.numeric(selectOrg)), 7]
  }
  pathway <- dbConnect(sqlite, gmtFiles[ix], flags = SQLITE_RO)

  # Double single quotes for SQL: Parkinson's -> Parkinson''s
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
