# Plotting functions
#
# Draw the Tree and Network tabs, plus small ggplot2 helpers used by the
# Chart and Plots tabs.
#
#   enrichmentPlot()     Tree tab: clustering tree of enriched pathways
#   enrichmentNetwork()  Network tab: network of enriched pathways
#   enrich.net2()        builds (and optionally draws) that network
#   network_vis_data()   that network -> visNetwork nodes and edges
#   build_visnetwork()   the interactive network's layout and styling
#   refine_ggplot2()     apply the user's chosen ggplot2 theme
#   fake_plot()          placeholder plot that shows a message
#   mark_significance()  0.000234 -> "P=0.00023 ***"
#   wrap_strings()       wrap long labels onto several lines
#   densMode()           peak of a density curve
#   draw_key_polygon3()  more space between bar chart legend keys
#
# The tree and network both take enriched pathways with columns Pathways,
# adj.Pval, Direction and Membership (the pathway's Ensembl gene IDs, from
# FindOverlap()). Pathways that share more genes are drawn closer together.
# ----------------------------------------------------------------------------


# Clustering tree of enriched pathways, drawn with base graphics. Pathways are
# clustered by the share of genes they have in common (genes in both / genes
# in either). The leaf labels show FDR and pathway name; bigger dots are more
# significant.
#
# rightMargin: room for the labels, in lines of text.
#
# Returns the drawn plot (recordPlot()), so it can be redrawn for downloads,
# or NULL if there are fewer than two pathways.
enrichmentPlot <- function(enrichedTerms, rightMargin = 33) {
  if (class(enrichedTerms) != "data.frame") {
    return(NULL)
  }
  if (nrow(enrichedTerms) <= 1) {
    return(NULL)
  }

  # Membership comes from FindOverlap() as Ensembl ids. Do not go back to
  # splitting enrichedTerms$Genes: that column is formatted for display, and
  # parsing it makes formatting changes move these numbers.
  geneLists <- lapply(enrichedTerms$Membership, as.character)
  names(geneLists) <- enrichedTerms$Pathways

  # w[i, j]: share of genes pathways i and j have in common.
  n <- length(geneLists)
  w <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in i:n) {
      u <- unlist(geneLists[i])
      v <- unlist(geneLists[j])
      w[i, j] <- length(intersect(u, v)) / length(unique(c(u, v)))
    }
  }
  # Fill the lower half from the upper half; the matrix is symmetric.
  for (i in 1:n) {
    for (j in 1:(i - 1)) {
      w[i, j] <- w[j, i]
    }
  }

  # Leaf labels: "<FDR> <pathway name>".
  Terms <- paste(
    sprintf("%-2.1e", as.numeric(enrichedTerms$adj.Pval)),
    names(geneLists)
  )
  rownames(w) <- Terms
  colnames(w) <- Terms
  par(mar = c(0, 0, 1, rightMargin)) # a large right margin for the labels

  dend <- as.dist(1 - w) %>%
    hclust(method = "average")
  ix <- dend$order # leaf order in the tree

  # Leaf colour by Direction. The app always passes Direction = "Diff", so
  # every leaf gets the first colour and the else branch (letter groups
  # "A", "B", ...) isn't used.
  leafType <- as.factor(gsub(" .*", "", enrichedTerms$Direction[ix]))
  if (max(nchar(enrichedTerms$Direction[ix])) >= 1) {
    leafColors <- mycolors[1:2]
  } else { # convert c("B","D","E") to c(2, 4, 5)
    leafType <- match(gsub(" .*", "", enrichedTerms$Direction[ix]), toupper(letters))

    leafColors <- mycolors
  }
  # Leaf size: -log10(FDR), rescaled to 0.1-1.
  leafSize <- -log10(as.numeric(enrichedTerms$adj.Pval[ix]))
  leafSize <- .9 * (leafSize - min(leafSize)) / (max(leafSize) - min(leafSize) + 1e-50) + .1

  dend %>%
    as.dendrogram(hang = -1) %>%
    set("leaves_pch", 19) %>% # filled circle
    set("leaves_cex", leafSize) %>%
    set("leaves_col", leafColors[leafType]) %>%
    plot(horiz = TRUE, axes = FALSE)

  return(recordPlot())
}


# Network of enriched pathways: one node per pathway, with an edge between
# two pathways that share at least edge.cutoff of their genes (as a fraction).
#
# layoutButton: the layout button's click count, used as the random seed, so
#               each click gives a new layout.
#
# Returns an igraph object; it does not draw anything.
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


# Nodes and edges of an enrichmentNetwork() graph for visNetwork, with round
# nodes sized by the number of genes. Used for the Network tab and its
# downloads.
network_vis_data <- function(g) {
  data1 <- toVisNetworkData(g)
  data1$nodes$shape <- "dot"
  data1$nodes$size <- 5 + data1$nodes$size^2
  data1
}


# The interactive network from network_vis_data(): Fruchterman-Reingold
# layout, black-bordered nodes and grey edges. Shared by the Network tab and
# its HTML download. Colour codes: https://www.rapidtables.com/web/color/RGB_Color.html
build_visnetwork <- function(data1) {
  visNetwork(nodes = data1$nodes, edges = data1$edges, height = "700px", width = "700px") %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visNodes(
      color = list(
        border = "#000000",
        highlight = "#FF8000"
      ),
      font = list(
        color = "#000000",
        size = 20
      ),
      borderWidth = 1,
      shadow = list(enabled = TRUE, size = 10)
    ) %>%
    visEdges(
      shadow = FALSE,
      color = list(color = "#A9A9A9", highlight = "#FFD700")
    )
}


# Build a network of gene sets; adapted from enrich.net() in the PPInfer
# package. Only enrichmentNetwork() calls it.
#
# x:        data frame of gene sets, one row each.
# gene.set: named list of gene IDs per set; names match x[, node.id].
# node.id, node.name: columns of x holding each set's ID and label.
# pvalue:   column of x holding P values; sets at or above pvalue.cutoff are
#           dropped, and the n most significant are kept.
# numChar:  maximum label length; longer labels are cut and end in "...".
# edge.cutoff:   drop edges between sets sharing less than this fraction.
# degree.cutoff: drop nodes with fewer edges than this.
# edge.width, node.size: functions of the shared fraction and the set size.
# group, group.color, group.shape: colour and shape of nodes by group; darker
#           colour means more significant.
# plotting, show.legend: draw the network (and its legend) as well.
# layoutButton: random seed for the layout.
# ...:      passed on to plot() when plotting.
#
# Returns the igraph object.
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
  set.seed(layoutButton)

  # Keep the n most significant sets.
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

  # Shorten labels to numChar, unless that would make two labels the same.
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

  # w[i, j]: share of genes sets i and j have in common (upper half only).
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

  # Edge list (share, set A, set B), without self-loops and the empty lower half.
  list.edges <- stack(data.frame(w))
  list.edges <- cbind(list.edges[, 1], rep(
    x[, node.name],
    n
  ), rep(x[, node.name], each = n))
  list.edges <- list.edges[list.edges[, 2] != list.edges[, 3], ]
  list.edges <- list.edges[!is.na(list.edges[, 1]), ]

  # Build the graph, then drop weak edges and poorly connected nodes.
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

  # Node shape by group; colour by group, darker for smaller P values.
  group.level <- sort(unique(group))
  pvalues <- log10(x[, pvalue] + 1e-200) # + 1e-200 avoids log10(0)
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
            adjustcolor(group.color[i], alpha.f = .1 + x)
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


# ggplot2 helpers -------------------------------------------------------------

# Apply the theme chosen in the "Plot theme" menu. Grid lines are removed
# unless gridline is TRUE or the user picked "Add grid". Any other choice
# ("default") leaves the theme unchanged.
refine_ggplot2 <- function(p, gridline, ggplot2_theme = "light") {

  p <- switch(ggplot2_theme,
    "linedraw" = p + ggplot2::theme_linedraw(),
    "classic" = p + ggplot2::theme_classic(),
    "gray" = p + ggplot2::theme_gray(),
    "light" = p + ggplot2::theme_light(),
    "dark" = p + ggplot2::theme_dark(),
    "bw" = p + ggplot2::theme_bw(),
    p # default, no change
  )

  if (ggplot2_theme != "Add grid") {
    if (!gridline) {
      p <- p +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }
  }

  return(p)
}

# An empty plot with a message in the middle, e.g. "Not available.", for when
# there's no data to plot.
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

# P value as a label with stars: "P=2.3E-4 *". The three cutoffs come from
# global.R (PvalGeneInfo2 = 0.001 -> ***, PvalGeneInfo1 = 0.01 -> **,
# PvalGeneInfo = 0.05 -> *).
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

# Wrap long labels onto several lines:
# "Mitotic DNA damage checkpoint" -> "Mitotic DNA damage\ncheckpoint"
# https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles
wrap_strings <- function(vector_of_strings, width = 30) {
  as.character(sapply(vector_of_strings, FUN = function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  }))
}

# The peak (x, y) of a density curve, for placing a label on a density plot.
# http://ianmadd.github.io/pages/PeakDensityDistribution.html
densMode <- function(x) {
  td <- density(x, na.rm = TRUE)
  maxDens <- which.max(td$y)
  list(x = td$x[maxDens], y = td$y[maxDens])
}

# Legend key for bar charts with more space between keys.
# By @clauswilke: https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2
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
# Use it for every bar chart. This changes ggplot2 itself, for the whole R
# session, as soon as this file loads.
GeomBar$draw_key <- draw_key_polygon3
