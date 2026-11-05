
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

  # Membership comes from FindOverlap() as Ensembl ids. Do not go back to
  # splitting enrichedTerms$Genes: that column is formatted for display, and
  # parsing it makes formatting changes move these numbers.
  geneLists <- lapply(enrichedTerms$Membership, as.character)
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
  # Membership comes from FindOverlap() as Ensembl ids. Do not go back to
  # splitting enrichedTerms$Genes: that column is formatted for display, and
  # parsing it makes formatting changes move these numbers.
  geneLists <- lapply(enrichedTerms$Membership, as.character)
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
