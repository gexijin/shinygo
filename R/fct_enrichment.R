
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
  ix <- grepl("http:|https:", urlVector, ignore.case = TRUE)
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

# Main function. Find a query set of genes enriched with functional category
# For debug:  converted = converted(); gInfo = tem;  GO=input$selectGO; selectOrg=input$selectOrg;  minFDR=input$minFDR; input_maxTerms=input$maxTerms
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
  # Actual pathway membership, as Ensembl ids. This is what the tree, the
  # network and the redundancy filter need. sharedGenesPrefered() below formats
  # the same thing for display; nothing should parse that string to recover
  # membership, because then display formatting changes the analysis.
  pathwayMembers <- function(pathwayID) {
    result[which(result[, 2] == pathwayID), 1]
  }

  sharedGenesPrefered <- function(pathwayID) {
    # result comes from "select distinct gene, pathwayID", so this is already
    # one row per gene and the output must have exactly one token per gene.
    # enrichmentPlot() and enrichmentNetwork() rebuild pathway membership by
    # splitting this string on spaces, so dropping a gene here silently
    # corrupts the tree and network overlaps as well as the displayed table.
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
    # I() keeps this an AsIs list column so it survives ordering and subsetting
    # below and stays aligned with its row.
    x$Membership <- I(lapply(x$pathwayID, pathwayMembers))
    x$n <- as.numeric(x$n) # convert total genes from character to numeric 10/21/19
    x <- subset(x, select = c(FDR, overlap, n, fold, description, memo, Genes, Membership))
    x <- x[order(x$FDR), ] # sort by FDR   4/1/2022 related to issue 23
    x <- x[!duplicated(x$description), ] # remove duplicates   4/1/2022
    colnames(x) <- c("Enrichment FDR", "nGenes", "Pathway Genes", "Fold Enrichment", "Pathway", "URL", "Genes", "Membership")



    # only keep 1000 pathways at the most
    if (dim(x)[1] > maxTerms) x <- x[1:maxTerms, ]
  }

  dbDisconnect(pathway)
  return(list(x = x, groupings = groups, categoryChoices = categoryChoices))
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
