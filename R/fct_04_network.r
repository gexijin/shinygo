#' enrich.net2
#'
#' @description Builds an `igraph` network from a table of enriched gene
#'   sets: two gene sets (nodes) are connected by an edge weighted by the
#'   fraction of genes they share, node size reflects gene-set size, and
#'   node color reflects (log) P-value. Sparse edges and low-degree nodes are
#'   pruned via `edge.cutoff`/`degree.cutoff`. Internal helper called only by
#'   `enrichmentNetwork()`; adapted from the `PPInfer` package.
#'
#' @param x Data frame of enrichment results, one row per gene set/pathway.
#' @param gene.set Named list of gene sets (`name -> character vector of
#'   genes`), keyed the same as `x[, node.id]`.
#' @param node.id Column name in `x` used to look up each gene set in
#'   `gene.set`.
#' @param node.name Column name in `x` used as the node label (defaults to
#'   `node.id`).
#' @param pvalue Column name in `x` holding the P-value used for filtering,
#'   ranking, and node color.
#' @param n Maximum number of nodes (gene sets) to keep, ranked by P-value.
#' @param numChar Maximum node-label length before truncating with "...";
#'   `NULL` auto-picks the shortest length that still keeps labels unique.
#' @param pvalue.cutoff P-value threshold; gene sets at or above it are
#'   dropped before building the network.
#' @param edge.cutoff Minimum gene-overlap ratio required to keep an edge
#'   between two nodes.
#' @param degree.cutoff Minimum node degree (number of edges) required to
#'   keep a node, applied after edge pruning.
#' @param edge.width,node.size Functions mapping overlap ratio / gene-set
#'   size to plotted edge width / node size.
#' @param group Vector (same length/order as `x`) grouping each gene set,
#'   e.g. up/down direction; controls node shape and color.
#' @param group.color,group.shape Colors/shapes assigned to each level of
#'   `group`, in the same order as `sort(unique(group))`.
#' @param legend.parameter,show.legend Legend placement/options, and whether
#'   to draw a legend; used only when `plotting = TRUE`.
#' @param plotting If `TRUE`, draws the base R plot as a side effect.
#' @param layoutButton Seeds `set.seed()` so the layout stays stable across
#'   re-renders unless the user asks for a new one.
#' @param ... Additional arguments passed to `plot.igraph()`.
#'
#' @return An `igraph` graph object with `size`/`color`/`shape` vertex
#'   attributes and `width` edge attributes set, ready for plotting or for
#'   `visNetwork::toVisNetworkData()`.
#'
#' @noRd
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

  # Attach the group labels, filter out gene sets below the significance
  # cutoff, and keep only the top n most significant to become nodes
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

  # Truncate node labels to numChar, auto-widening if that would make
  # two labels identical
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
  # Pairwise gene-overlap ratio (intersection / union) between every two
  # gene sets; this becomes the edge weight between their nodes
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

  # Reshape the overlap matrix into an edge list (dropping self-pairs and
  # the unfilled half of the matrix), then build the graph and prune weak
  # edges and low-degree nodes
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

  # Color/shape each node by its group and (log) P-value: darker shade
  # within a group means more significant
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
  # Draw the static base-R plot, with an optional legend for the groups
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

#' enrichmentNetwork
#'
#' @description Thin wrapper around `enrich.net2()`: reshapes a table of
#'   enrichment results into the gene-set list format `enrich.net2()`
#'   expects and returns the resulting `igraph` object. Called directly by
#'   the Network tab (`mod_04_network.r`) to build both the static and
#'   interactive (visNetwork) plots.
#'
#' @param enrichedTerms Enrichment result table (as produced by
#'   `FindOverlap()$x`) with `Genes` (space-separated gene symbols/IDs per
#'   pathway), `Pathways` (pathway names, used as node labels), `adj.Pval`,
#'   and `Direction` (e.g. "Up genes"/"Down genes", collapsed to just
#'   "Up"/"Down" for grouping) columns.
#' @param layoutButton Seeds the network layout so it stays stable across
#'   re-renders unless the user clicks "Change layout".
#' @param edge.cutoff Minimum gene-overlap ratio required to connect two
#'   pathways with an edge; passed through to `enrich.net2()`.
#'
#' @return An `igraph` graph object; see `enrich.net2()`.
#'
#' @noRd
enrichmentNetwork <- function(enrichedTerms, layoutButton = 0, edge.cutoff = 5) {
  # Split each pathway's space-separated gene list back into a vector,
  # keyed by pathway name, and collapse "Up genes"/"Down genes" to a
  # bare direction label for grouping
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
