mycolors <- sort(rainbow(20))[c(1, 20, 10, 11, 2, 19, 3, 12, 4, 13, 5, 14, 6, 15, 7, 16, 8, 17, 9, 18)] # 20 colors for kNN clusters

# a program for ploting enrichment results by highlighting the similarities among terms
# must have columns: Direction, adj.Pval   Pathways Genes
#  Direction	adj.Pval	nGenes	Pathways		Genes
# Down regulated	3.58E-59	131	Ribonucleoprotein complex biogenesis	36	Nsun5 Nhp2 Rrp15
# Down regulated	2.55E-57	135	NcRNA metabolic process	23	Nsun5 Nhp2 Rrp15 Emg1 Ddx56 Rsl1d1 enrichmentPlot <- function( enrichedTerms){
# Up or down regulation is color-coded
# gene set size if represented by the size of marker

#' enrichmentPlot
#'
#' @description Draws a hierarchical clustering tree ("dendrogram") of
#'   significant pathways, clustering pathways by shared-gene overlap.
#'   Leaf color encodes Up/Down direction (or arbitrary group letters),
#'   leaf size encodes -log10(adjusted P-value). Called by the Tree tab
#'   (`mod_03_tree.r`) to render both the on-screen and downloaded plot.
#'
#' @param enrichedTerms Enrichment result table (as produced by
#'   `FindOverlap()$x`) with `Direction`, `adj.Pval`, `Pathways`, and
#'   `Genes` (space-separated gene symbols/IDs per pathway) columns.
#' @param rightMargin Right-side plot margin (in `par(mar=)` lines),
#'   sized to fit the pathway-name labels.
#'
#' @return A recorded base-R plot object (`recordPlot()`), or `NULL` if
#'   `enrichedTerms` isn't a data frame or has one row or fewer.
#'
#' @noRd
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
