# Global settings
#
# Shiny runs this file first, before R/, ui.R and server.R. It loads the
# packages and sets plain constants that never change while the app runs.
# Data from the database is not loaded here: see load_app_data() in
# R/app_data.R. Originally by Steven Ge (Xijin.Ge@sdstate.edu), Ge Lab.
#
#   STRING_DB_VERSION      STRING-db version, used to place STRING genomes
#   Min_overlap            minimum query genes for a pathway to be tested
#   PvalGeneInfo, 1, 2     P value cutoffs for *, **, *** on the Plots tab
#   minGenes               minimum genes needed to draw a Plots-tab chart
#   maxGenesBackground     background genes above this are sampled down
#   redudantGeneSetsRatio  gene overlap at which pathways count as redundant
#   min_gene_fold          minimum pathway size when sorting by fold
#   ExampleGeneList1       the "Demo genes" list (human Ensembl IDs)
#   mycolors               20 colours, used for the tree leaves
#   sqlite                 SQLite driver for the pathway databases
#   columnSelection        Chart tab menu choices
# ----------------------------------------------------------------------------

library(shiny)
library(RSQLite)
library(ggplot2)
library(gridExtra)
library(plotly)
library(reshape2)
library(visNetwork)
library(dplyr)
library(DT, verbose = FALSE) # for renderDataTable


# Settings ---------------------------------------------------------------------

STRING_DB_VERSION <- "12.0"
Min_overlap <- 1
PvalGeneInfo <- 0.05
minGenes <- 10
PvalGeneInfo1 <- 0.01
PvalGeneInfo2 <- 0.001
maxGenesBackground <- 100000
redudantGeneSetsRatio <- 0.95 # share of genes; the name's typo is kept as is
min_gene_fold <- 10

# Send stray base-graphics output nowhere, instead of an Rplots.pdf file that
# the server may not be allowed to write.
pdf(NULL)


# Demo gene list ----------------------------------------------------------------

# Human Ensembl IDs, pasted in by the "Demo genes" button.
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


# Plotting and database helpers ------------------------------------------------

# 20 rainbow colours, reordered so neighbours contrast.
mycolors <- sort(rainbow(20))[c(1, 20, 10, 11, 2, 19, 3, 12, 4, 13, 5, 14, 6, 15, 7, 16, 8, 17, 9, 18)]

sqlite <- dbDriver("SQLite")

# Chart tab menus: label -> column of the enrichment table.
columnSelection <- list(
  "-log10(FDR)" = "EnrichmentFDR",
  "Fold Enrichment" = "FoldEnrichment",
  "N. of Genes" = "nGenes",
  "Category Name" = "Pathway"
)
