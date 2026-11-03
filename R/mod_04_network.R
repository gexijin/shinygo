# R/mod_network.R
mod_network_ui <- function() {
  tabPanel("Network",
    value = 5,
    fluidRow(
      column(2, actionButton("layoutButton", "Change layout")),
      column(2, actionButton("GONetwork", "Static plot")),
      column(2, h5("Edge cutoff:"), align = "left"),
      column(2, numericInput("edgeCutoff", label = NULL, value = 0.30, min = 0, max = 1, step = .1), align = "right"),
      column(2, checkboxInput("wrapTextNetwork", "Wrap text", value = TRUE))
    ),
    visNetworkOutput("enrichmentNetworkPlotInteractive", height = "800px", width = "800px"),
    downloadButton("enrichmentNetworkPlotInteractiveDownload", "Download HTML"),
    downloadButton("downloadEdges", "Edges"),
    downloadButton("downloadNodes", "Nodes"),
    h5("Similar to the Tree tab, this interactive plot also shows the relationship between enriched pathways.
        Two pathways (nodes) are connected if they share 20% (default) or more genes.
        You can move the nodes by dragging them, zoom in and out by scrolling,
        and shift the entire network by click on an empty point and drag.
        Darker nodes are more significantly enriched gene sets.
        Bigger nodes represent larger gene sets.
        Thicker edges represent more overlapped genes."),

    bsModal("InteractiveNetwork", "Interactive enrichment networks ", "GONetwork",
      size = "large",
      fluidRow(
        column(2, actionButton("layoutButtonStatic", "Change layout")),
        column(2, downloadButton("enrichmentNetworkPlotDownload", "Download")),
        column(2, checkboxInput("wrapTextNetworkStatic", "Wrap text", value = FALSE))
      ),
      plotOutput("enrichmentNetworkPlot")
    )
  )
}