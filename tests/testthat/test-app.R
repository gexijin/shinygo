testthat::local_edition(3)

# Baseline for the reactive code in server.R, which the function tests can't
# reach: run the app headless, analyse the yeast gene list, and snapshot what
# the tabs show. Plots render as images, which differ between machines, so the
# Chart and Plots tabs are read through exportTestValues() at the end of
# server.R instead. STRING is left out: it depends on the STRING website.

# Keep only the data of an htmlwidget output (genome plot, network), not its
# styling or random element IDs.
widget_data <- function(json, fields) {
  jsonlite::fromJSON(json)$x[fields]
}

# Every text label in a grid drawing (titles, axis ticks, "P=8.5E-12 ***").
grob_text <- function(g) {
  unname(c(
    if (inherits(g, "text")) g$label,
    unlist(lapply(c(g$grobs, g$children), grob_text))
  ))
}

# Text drawn by a base-graphics plot (the tree's pathway labels, in tree
# order), read from an SVG rendering.
base_plot_text <- function(draw) {
  svg <- tempfile(fileext = ".svg")
  svglite::svglite(svg, width = 20, height = 20)
  tryCatch(draw(), finally = grDevices::dev.off())
  xml2::xml_text(xml2::xml_find_all(xml2::read_xml(svg), "//*[local-name()='text']"))
}

test_that("app outputs for the yeast gene list are unchanged", {
  skip_if_no_db()
  skip_if(is.null(chromote::find_chrome()), "no Chrome/Chromium for shinytest2")

  # Load global.R now: loading it while the tree's SVG device is open sends
  # the plot to a different device.
  enrichment_plot <- app_env()$enrichmentPlot

  withr::local_envvar(IDEP_DATABASE = Sys.getenv("IDEP_DATABASE"))
  app <- shinytest2::AppDriver$new(
    testthat::test_path("..", ".."),
    name = "yeast", load_timeout = 90000, timeout = 120000
  )
  withr::defer(app$stop())

  # Pick yeast the way the species table does, then submit.
  app$set_inputs(clicked_row = "scerevisiae_gene_ensembl", allow_no_input_binding_ = TRUE)
  app$set_inputs(input_text = yeast_cell_cycle_genes)
  app$click("goButton")
  # The database menu is drawn by the server; wait for it before reading.
  expect_equal(app$wait_for_value(input = "selectGO"), "KEGG") # the default

  # Outputs only compute on a visible tab, so visit each one.
  for (tab in c("3", "5", "6", "8", "9", "1")) app$set_inputs(tabs = tab)
  out <- app$get_values(output = TRUE)$output
  plots <- app$get_values(export = TRUE)$export
  expect_snapshot_value(list(
    selected_species = out$selected_species,
    mapping_stats = out$mapping_stats,
    enrichment_kegg = out$EnrichmentTable,
    chart_kegg = plots$enrich_chart$data,
    genes = out$conversionTable,
    # Node x/y are layout positions, not results.
    network_kegg = lapply(
      widget_data(out$enrichmentNetworkPlotInteractive, c("nodes", "edges")),
      function(df) df[setdiff(names(df), c("x", "y"))]
    ),
    genome = widget_data(out$genomePlotly, "data"),
    # Density plots: t-test results are only kept as these labels.
    gene_density_labels = grob_text(plots$gene_density_plot),
    # Tree: the app's own input, drawn by enrichmentPlot() as renderPlot does.
    tree_labels = base_plot_text(function() enrichment_plot(plots$tree_input, 45))
  ), style = "deparse")

  # GO Biological Process also feeds the Groups tab, which KEGG hides.
  app$set_inputs(selectGO = "GOBP")
  app$set_inputs(tabs = "7") # Groups
  app$set_inputs(tabs = "1")
  out <- app$get_values(output = TRUE)$output
  expect_snapshot_value(list(
    enrichment_gobp = out$EnrichmentTable,
    groups = out$grouping
  ), style = "deparse")

  # Every sort option, with the other settings at their defaults.
  sort_options <- c(
    "Sort by FDR", "Sort by Fold Enrichment", "Sort by FDR & Fold Enrichment",
    "Select by FDR, sort by Fold Enrichment", "Sort by Genes", "Sort by Category Name"
  )
  by_sort <- lapply(sort_options, function(option) {
    app$set_inputs(SortPathways = option)
    app$get_value(output = "EnrichmentTable")
  })
  names(by_sort) <- sort_options
  expect_snapshot_value(by_sort, style = "deparse")

  # Non-default sidebar settings, all changed together.
  app$set_inputs(SortPathways = "Select by FDR, sort by Fold Enrichment")
  app$set_inputs(
    minFDR = 0.01, maxTerms = "50", minSetSize = 10, maxSetSize = 2000,
    removeRedudantSets = FALSE, abbreviatePathway = FALSE, show_pathway_id = TRUE
  )
  expect_snapshot_value(app$get_value(output = "EnrichmentTable"), style = "deparse")
})
