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


# define where database is located
db_ver <<- "data115"
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

mycolors <- sort(rainbow(20))[c(1, 20, 10, 11, 2, 19, 3, 12, 4, 13, 5, 14, 6, 15, 7, 16, 8, 17, 9, 18)] # 20 colors for kNN clusters


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

keggSpeciesID <- orgInfo[, c("ensembl_dataset", "name", "KEGG")]
colnames(keggSpeciesID)[3] <- "kegg"

# keggSpeciesID = read.csv(paste0(datapath,"data_go/KEGG_Species_ID.csv"))
# List of GMT files in /gmt sub folder
gmtFiles <- orgInfo$file
gmtFiles <- paste(datapath, "/db/", gmtFiles, sep = "")
# geneInfoFiles <- list.files(path = paste0(datapath, "geneInfo"), pattern = ".*GeneInfo\\.csv")
# geneInfoFiles <- paste(datapath, "geneInfo/", geneInfoFiles, sep = "")
# motifFiles <- list.files(path = paste0(datapath, "motif"), pattern = ".*\\.db")
# motifFiles <- paste(datapath, "motif/", motifFiles, sep = "")



#______________________________________________________________



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


# , categoryChoices = categoryChoices
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
