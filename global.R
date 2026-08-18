###################################################
# Author: Steven Ge Xijin.Ge@sdstate.edu
# Lab: Ge Lab
# R version 4.0.5
# Project: ShinyGO v65
# File: global.R
# Purpose of file:global file for app (need more info here)
# Start data: NA (mm-dd-yyyy)
# Data last modified: 06-16-2021, 11:46 PM CST (mm-dd-yyyy,TIME)
# to help with github merge
#######################################################
library(shiny)
library(RSQLite)
library(ggplot2)
library(gridExtra)
library(plotly)
library(reshape2)
library(visNetwork)
library(dplyr)
library(DT, verbose = FALSE) # for renderDataTable

source("utils_kegg_pathview.R")

# define where database is located
db_ver <<- "data113"
db_url <<- "http://bioinformatics.sdstate.edu/data/"

# if environmental variable is not set, use relative path
datapath <<- Sys.getenv("IDEP_DATABASE")[1]
# if not defined in the environment, use too levels above
if (nchar(datapath) == 0) {
  datapath <<- paste0("../../data/")
}
# Add version
datapath <<- paste0(datapath, "/", db_ver, "/")
org_info_file <<- paste0(datapath, "demo/orgInfo.db")
if (!file.exists(org_info_file)) {
  datapath <<- paste0("./", db_ver, "/")
  org_info_file <<- paste0(datapath, "demo/orgInfo.db")
}

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







STRING_DB_VERSION <- "12.0" # what version of STRINGdb needs to be used
Min_overlap <- 1
minSetSize <- 3
mappingCoverage <- 0.60 # 60% percent genes has to be mapped for confident mapping
mappingEdge <- 0.5 # Top species has 50% more genes mapped
maxTerms <- 30 # max number of enriched terms; no longer used
PvalGeneInfo <- 0.05
minGenes <- 10 # min number of genes for plotting
PvalGeneInfo1 <- 0.01
PvalGeneInfo2 <- 0.001
maxGenesBackground <- 100000
redudantGeneSetsRatio <- 0.95 # remove redundant pathways if they share 90% of genes.
min_gene_fold <- 10 # minimum number of  genes in pathways, when sorting by fold.
pdf(NULL) # this prevents error Cannot open file 'Rplots.pdf'
ExampleGeneList2 <-
  "Hus1 Rad1 Tp63 Tp73 Usp28 Rad9b Fanci Hus1b
Cdk1 Cry1 D7Ertd443e Chek1 Foxo4 Zak Pea15a
Mapkapk2 Brca1 Taok1 Cdk5rap3 Ddx39b Mdm2 Fzr1
Rad17 Prkdc Cdkn1a Cdc5l Wac Thoc1 Prpf19 Rad9a
Pidd1 Atrip Uimc1Nek6 Atf2 E2f1 Nbn Rpa2 Rint1
Clock Chek2 Casp2 Blm Plk1 Brcc3 Hinfp Fem1b
Tipin Atr Cdc14b Rfwd3 Ccar2 Foxn3 Atm Thoc5
Rps27l Ints7 Dtl Tiprl Rbbp8 Clspn Cradd Rhno1
Sox4 Msh2 Xpc Rad9a Rnaseh2b Fbxo4 Syf2 Cul4a
Gigyf2 Mapk14 Bcat1 Fbxo31 Babam1 Cep63 Ccnd1
Nek11 Fam175a Brsk1 Plk5 Bre Tp53 Taok2 Taok3
Nek1 Mre11a Pml Ptpn11 Zfp830
"
ExampleGeneList1 <- "ENSG00000078900
ENSG00000117614
ENSG00000117748
ENSG00000092853
ENSG00000143155
ENSG00000162889
ENSG00000143493
ENSG00000143476
ENSG00000095002
ENSG00000115966
ENSG00000204120
ENSG00000154767
ENSG00000164053
ENSG00000114670
ENSG00000182923
ENSG00000175054
ENSG00000073282
ENSG00000134852
ENSG00000137601
ENSG00000113456
ENSG00000151876
ENSG00000152942
ENSG00000188996
ENSG00000124766
ENSG00000198563
ENSG00000112062
ENSG00000124762
ENSG00000096401
ENSG00000136273
ENSG00000135249
ENSG00000106144
ENSG00000158941
ENSG00000253729
ENSG00000104320
ENSG00000081377
ENSG00000095787
ENSG00000170312
ENSG00000177595
ENSG00000110107
ENSG00000172613
ENSG00000110092
ENSG00000149311
ENSG00000048028
ENSG00000172273
ENSG00000149554
ENSG00000171792
ENSG00000060982
ENSG00000135679
ENSG00000169372
ENSG00000008405
ENSG00000151164
ENSG00000179295
ENSG00000135090
ENSG00000136104
ENSG00000139842
ENSG00000053254
ENSG00000185088
ENSG00000075131
ENSG00000169018
ENSG00000140464
ENSG00000140525
ENSG00000197299
ENSG00000166851
ENSG00000149930
ENSG00000168411
ENSG00000103264
ENSG00000141510
ENSG00000160551
ENSG00000012048
ENSG00000108465
ENSG00000079134
ENSG00000101773
ENSG00000185988
ENSG00000105325
ENSG00000105393
ENSG00000160469
ENSG00000101412
ENSG00000183765
ENSG00000100296
ENSG00000184481
ENSG00000185515"

# Wrapping long text by adding \n
#  "Mitotic DNA damage checkpoint"  --> "Mitotic DNA damage\ncheckpoint"
# https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles
wrap_strings <- function(vector_of_strings, width = 30) {
  as.character(sapply(vector_of_strings, FUN = function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  }))
}

# function to increase vertical spacing between legend keys
# @clauswilke https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    )
  )
}
# register new key drawing function,
# the effect is global & persistent throughout the R session
GeomBar$draw_key <- draw_key_polygon3

# find peak values in density plots
# for adding annotation texts
# http://ianmadd.github.io/pages/PeakDensityDistribution.html
densMode <- function(x) {
  td <- density(x, na.rm = TRUE)
  maxDens <- which.max(td$y)
  list(x = td$x[maxDens], y = td$y[maxDens])
}

cleanGeneSet <- function(x) {
  # remove duplicate; upper case; remove special characters
  x <- unique(toupper(gsub("\n| ", "", x)))
  x <- remove_gene_version(x)
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

sqlite <- dbDriver("SQLite")
convert <- connect_convert_db()



# Create a list for Select Input options
orgInfo <- dbGetQuery(convert, paste("select distinct * from orgInfo "))
orgInfo <- orgInfo[order(orgInfo$name), ]

top_choices <- c(
  #"Best matching species", 
  #"**NEW SPECIES**", 
  "Human",
  "Mouse",
  "Drosophila melanogaster",
  "Zebrafish",
  "Caenorhabditis elegans",
  "Arabidopsis thaliana",
  "Saccharomyces cerevisiae",
  "Escherichia coli",
  "Rat",
  "Cow",
  "Pig", 
  "Chicken", 
  "Macaque", 
  "Dog",
  "Zea mays", 
  "Glycine max",
  "Oryza sativa Indica Group", 
  "Oryza sativa Japonica Group", 
  "Vitis vinifera"
)

#org_info <- org_info[order(org_info$group), ]
ix <- match(orgInfo$name2, top_choices)
orgInfo <- orgInfo[order(ix), ]
orgInfo <- orgInfo[order(orgInfo$group == paste0("STRINGv", STRING_DB_VERSION)), ]

annotatedSpeciesCounts <- sort(table(orgInfo$group)) # total species, Ensembl, Plants, Metazoa, STRINGv10
speciesChoice <- setNames(as.list(orgInfo$id), orgInfo$name2)
# add a defult element to list    # new element name       value
speciesChoice <- append(setNames("BestMatch", "Best matching species"), speciesChoice)
# move one element to the 2nd place
move2 <- function(i) c(speciesChoice[1], speciesChoice[i], speciesChoice[-c(1, i)])
i <- which(names(speciesChoice) == "Vitis vinifera")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Oryza sativa Japonica Group")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Oryza sativa Indica Group")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Glycine max")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Zea mays")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Arabidopsis thaliana")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Saccharomyces cerevisiae")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Caenorhabditis elegans")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Drosophila melanogaster")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Dog")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Macaque")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Chicken")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Pig")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Zebrafish")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Cow")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Rat")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Mouse")
speciesChoice <- move2(i)
i <- which(names(speciesChoice) == "Human")
speciesChoice <- move2(i)

GO_levels <- dbGetQuery(convert, "select distinct id,level from GO
                                 WHERE GO = 'biological_process'")
level2Terms <- GO_levels[which(GO_levels$level %in% c(2, 3)), 1] # level 2 and 3

# idIndex <- dbGetQuery(convert, paste("select distinct * from idIndex "))

quotes <- dbGetQuery(convert, " select * from quotes")
quotes$quotes <- gsub("\\\"", "", quotes$quotes) # remove the quotes \"
quotes <- paste0(quotes$quotes, " -- ", quotes$author)

columnSelection <- list(
  "-log10(FDR)" = "EnrichmentFDR",
  "Fold Enrichment" = "FoldEnrichment",
  "N. of Genes" = "nGenes",
  "Category Name" = "Pathway"
)



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


# keggSpeciesID = read.csv(paste0(datapath,"data_go/KEGG_Species_ID.csv"))
# List of GMT files in /gmt sub folder
gmtFiles <- orgInfo$file
gmtFiles <- paste(datapath, "/db/", gmtFiles, sep = "")
# geneInfoFiles <- list.files(path = paste0(datapath, "geneInfo"), pattern = ".*GeneInfo\\.csv")
# geneInfoFiles <- paste(datapath, "geneInfo/", geneInfoFiles, sep = "")
# motifFiles <- list.files(path = paste0(datapath, "motif"), pattern = ".*\\.db")
# motifFiles <- paste(datapath, "motif/", motifFiles, sep = "")

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

promoter <- function(converted, selectOrg, radio) {
  idNotRecognized <- as.data.frame("ID not recognized!")

  if (is.null(converted)) {
    return(idNotRecognized)
  } # no ID

  querySet <- converted$IDs

  if (length(querySet) == 0) {
    return(idNotRecognized)
  }
  ix <- grep(converted$species[1, 1], motifFiles)

  # If selected species is not the default "bestMatch", use that species directly
  if (selectOrg != speciesChoice[[1]]) {
    ix <- grep(findSpeciesById(selectOrg)[1, 1], motifFiles)
  }

  ix1 <- grep(as.character(radio), motifFiles[ix]) # match 300bp or 600bp
  if (length(ix1) > 0) ix <- ix[ix1] # if 600 is not found, use 300bp
  if (length(ix) == 0) {
    return(as.data.frame("No matching motif file found"))
  } else {
    if (length(ix) > 1) { # if only one file
      return(as.data.frame("Multiple geneInfo file found!"))
    }

    motifs <- dbConnect(sqlite, motifFiles[ix]) # makes a new file

    sqlQuery <- paste(" select * from scores where row_names IN ('", paste(querySet, collapse = "', '"), "')", sep = "")
    result <- dbGetQuery(motifs, sqlQuery)
    if (dim(result)[1] == 0) {
      return(list(x = as.data.frame("No matching species or gene ID file!")))
    }
    row.names(result) <- result$row_names
    result <- result[, -1]
    TFstat <- as.data.frame(cbind(apply(result, 2, mean), apply(result, 2, sd)))
    colnames(TFstat) <- c("scoreMean1", "scoreSD1")
    rownames(TFstat) <- toupper(colnames(result))

    TFs <- dbGetQuery(motifs, "select ID,TF_Name,Family_Name,DBID,Motif_ID,coreMotif,memo,nGenes,scoreSD,scoreMean from  TF_Information ")
    dbDisconnect(motifs)
    TFs$ID <- toupper(TFs$ID)

    TFs <- merge(TFs, TFstat, by.x = "ID", by.y = "row.names")
    TFs <- TFs[!is.na(TFs$scoreSD), ] # some TFs return NA -Inf
    n1 <- dim(result)[1] # number of genes in query set
    TFs$scoreMean2 <- (TFs$scoreMean * TFs$nGenes - TFs$scoreMean1 * n1) / (TFs$nGenes - n1)
    # SD2 needs to be adjusted too, but ignored for now. use overall SD2
    # t test unequal variance statistic
    TFs$t <- (TFs$scoreMean1 - TFs$scoreMean2) / sqrt(TFs$scoreSD1^2 / n1 + TFs$scoreSD^2 / TFs$nGenes)
    # degree of freedom
    TFs$df <- (TFs$scoreSD1^2 / n1 + TFs$scoreSD^2 / TFs$nGenes)^2 / ((TFs$scoreSD1^2 / n1)^2 / (n1 - 1) + (TFs$scoreSD^2 / TFs$nGenes)^2 / (TFs$nGenes - 1))
    TFs$pVal <- 1 - pt(TFs$t, df = TFs$df) # t distribution
    TFs$FDR <- p.adjust(TFs$pVal, method = "fdr")
    TFs <- TFs[order(TFs$pVal), ]
    TFs$scoreDiff <- round(TFs$scoreMean1 - TFs$scoreMean2, 0)
    # TFs <- TFs[order(-TFs$scoreDiff) ,]

    # does this transcription factor gene in this cluster?
    ix <- match(toupper(TFs$DBID), querySet) # assuming the DBID column in cisbp are ensembl gene ids
    TFs$note <- ""
    if (sum(!is.na(ix)) > 0) {
      TFs$note[which(!is.na(ix))] <- "* Query Gene"
    }
    TFs <- subset(TFs, FDR < 0.25, select = c(coreMotif, TF_Name, Family_Name, pVal, FDR, scoreDiff, note))
    colnames(TFs) <- c("Enriched motif in promoter", "TF", "TF family", "P val.", "FDR", "Score", "Note")
    if (dim(TFs)[1] > 30) {
      TFs <- TFs[1:30, ]
    }
    if (dim(TFs)[1] == 0) {
      return(as.data.frame("No significant TF binding motif detected."))
    } else {
      return(TFs)
    }
  }
}


mycolors <- sort(rainbow(20))[c(1, 20, 10, 11, 2, 19, 3, 12, 4, 13, 5, 14, 6, 15, 7, 16, 8, 17, 9, 18)] # 20 colors for kNN clusters

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

keggSpeciesID <- orgInfo[, c("ensembl_dataset", "name", "KEGG")]
colnames(keggSpeciesID)[3] <- "kegg"



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



#' Change ggplot2 plots
#'
#'
#' @param p ggplot2 object
#' @param gridline TRUE of FALSE
#'
#' @export
#' @return ggplot2 object
refine_ggplot2 <- function(p, gridline, ggplot2_theme = "light") {

  # apply theme based on selection
  p <- switch(ggplot2_theme,
    "linedraw" = p + ggplot2::theme_linedraw(),
    "classic" = p + ggplot2::theme_classic(),
    "gray" = p + ggplot2::theme_gray(),
    "light" = p + ggplot2::theme_light(),
    "dark" = p + ggplot2::theme_dark(),
    "bw" = p + ggplot2::theme_bw(),
    p # default, no change
  )

  if (ggplot2_theme != "Add grid") { # keep grid
    if (!gridline) { # by default it has gridlines
      p <- p +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }
  }

  return(p)
}

# generates a fake ggplot2, with some message like: "Not available."
fake_plot <- function(some_text) {
  p <- ggplot2::ggplot() +
    geom_point() +
    xlim(-10, 10) +
    ylim(-10, 10) +
    annotate("text",
      x = 0,
      y = 0,
      label = some_text
    ) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  return(p)
}

# 0.000234   <- 2.3E-4 *
mark_significance <- function(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo) {
  sig <- paste("P=", formatC(Pval, digits = 2, format = "G"), sep = "")
  if (Pval < PvalGeneInfo2) {
    sig <- paste(sig, "***")
  } else
  if (Pval < PvalGeneInfo1) {
    sig <- paste(sig, "**")
  } else
  if (Pval < PvalGeneInfo) sig <- paste(sig, "*")
  return(sig)
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
