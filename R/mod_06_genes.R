# R/mod_genes.R
mod_genes_ui <- function() {
  tabPanel("Genes",
    value = 6,
    fluidRow(
      column(3, downloadButton("downloadGeneInfo", "More info")),
      column(4, checkboxInput("showDetailedGeneInfo", "Detailed Description", value = FALSE))
    ),
    tableOutput("conversionTable")
  )
}