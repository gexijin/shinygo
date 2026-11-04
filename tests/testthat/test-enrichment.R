testthat::local_edition(3)

# Baseline for "no behaviour change" during modularization: the enrichment
# results for a fixed yeast gene list are snapshotted in _snaps/. A refactor
# must leave them unchanged. The snapshot records today's output; it is not a
# claim that today's output is scientifically right.

# Same steps as converted() / geneInfoLookup() in server.R.
convert_list <- function(env, genes) {
  converted <- env$convertID(genes, yeast_id)
  gene_info <- env$geneInfo(converted, yeast_id)
  list(converted = converted, gene_info = gene_info[gene_info$Set == "List", ])
}

run_enrichment <- function(go, ...) {
  env <- app_env()
  q <- convert_list(env, yeast_cell_cycle_genes)
  # ponytail: the max() warning on empty groupings is existing behaviour
  # (KEGG has no GO-level groupings); silenced so it doesn't clutter output.
  suppressWarnings(env$FindOverlap(q$converted, q$gene_info, go, yeast_id, ...))
}

test_that("gene ID conversion is unchanged", {
  skip_if_no_db()
  converted <- app_env()$convertID(yeast_cell_cycle_genes, yeast_id)
  expect_length(converted$IDs, 50) # NOTAGENE1 must not map
  expect_snapshot_value(converted$conversionTable, style = "deparse")
})

test_that("GO Biological Process enrichment is unchanged", {
  skip_if_no_db()
  result <- run_enrichment("GOBP")
  expect_match(result$x$Pathway[1], "cell cycle", ignore.case = TRUE)
  expect_snapshot_value(result[c("x", "groupings")], style = "deparse")
})

test_that("KEGG enrichment is unchanged", {
  skip_if_no_db()
  result <- run_enrichment("KEGG")
  expect_snapshot_value(result$x, style = "deparse")
})

test_that("enrichment with pathway-DB gene counts is unchanged", {
  skip_if_no_db()
  result <- run_enrichment("GOBP", gene_count_pathwaydb = TRUE)
  expect_snapshot_value(result$x, style = "deparse")
})

test_that("enrichment against a custom background is unchanged", {
  skip_if_no_db()
  env <- app_env()
  # Background: the query genes plus the first 2,000 yeast genes by ID.
  # geneInfo() returns every gene in the species; Set marks the query ones.
  all_genes <- env$geneInfo(env$convertID(yeast_cell_cycle_genes, yeast_id), yeast_id)
  background <- paste(
    union(strsplit(yeast_cell_cycle_genes, " ")[[1]],
          head(sort(all_genes$ensembl_gene_id), 2000)),
    collapse = " "
  )
  b <- convert_list(env, background)
  result <- run_enrichment("GOBP", convertedB = b$converted, gInfoB = b$gene_info)
  expect_snapshot_value(result$x, style = "deparse")
})
