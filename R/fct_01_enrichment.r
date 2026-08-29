#' FindOverlap
#'
#' @description Runs the core over-representation (hypergeometric) enrichment
#'   analysis for a query gene list against a chosen pathway/gene-set
#'   category (e.g. GOBP, KEGG, or "All"). Queries the species' pathway
#'   SQLite database for genes that fall in each pathway, computes FDR-
#'   adjusted enrichment P-values (against either the whole genome or, if
#'   supplied, a user background gene list), and also builds a grouping of
#'   genes by high-level GOBP categories.
#'
#' @param converted The query gene list, as returned by `convertID()`
#'   (a list with `$IDs`, `$species`, `$conversionTable`, etc.).
#' @param gInfo Gene info for the query list, as returned by `geneInfo()`;
#'   used to restrict to protein-coding genes and to prefer gene symbols
#'   over Ensembl IDs when listing overlapping genes.
#' @param GO The selected gene-set category (e.g. "GOBP", "KEGG"), or "All".
#' @param selectOrg The selected species ID (or the "BestMatch" sentinel).
#' @param convertedB Optional background gene list, same shape as `converted`.
#' @param gInfoB Optional gene info for the background list, same shape as `gInfo`.
#' @param minSetSize Minimum pathway size (number of genes) to keep.
#' @param maxSetSize Maximum pathway size (number of genes) to keep.
#' @param gene_count_pathwaydb If TRUE, use the pathway database's own gene
#'   counts (rather than genome-wide gene counts) as the background/total
#'   gene universe when computing enrichment.
#'
#' @return A list with `$x` (the enrichment result table, or a 1x1 data
#'   frame with a message such as "ID not recognized!" or "No significant
#'   enrichment found!" when analysis can't proceed), `$groupings` (genes
#'   grouped by high-level GOBP category), and `$categoryChoices` (the
#'   available gene-set categories for the species).
#'
#' @noRd
#
# For debug: converted = converted(); gInfo = tem; GO = input$selectGO;
# selectOrg = input$selectOrg; minFDR = input$minFDR; input_maxTerms = input$maxTerms
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

  # Query the pathway DB for (gene, pathwayID) pairs among the query genes,
  # optionally restricted to a single gene-set category
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

  # Tally overlap counts per pathway, drop pathways below the minimum
  # overlap, and merge in the pathway descriptions/URLs
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

  # Hypergeometric test: probability of this many (or more) overlap genes
  # occurring by chance, given the pathway size and the total gene universe
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

  # Keep only pathways passing the FDR cutoff, attach the overlapping genes,
  # and format the final output table
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
