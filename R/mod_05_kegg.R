# R/mod_kegg.R
mod_kegg_ui <- function() {
  tabPanel("KEGG",
    value = 2,
    conditionalPanel(
      "input.selectGO != 'KEGG' ",
      br(), br(),
      h5("Please select KEGG from the pathway databases to conduct enrichment analysis first.
      Then you can visualize your genes on any of the significant pathways. Only for some species.")
    ),
    conditionalPanel(
      "input.selectGO == 'KEGG' ",
      br(),
      uiOutput("listSigPathways"),
      br(), imageOutput("KeggImage", width = "100%", height = "100%"),
      h5("Your genes are highlighted in red. Downloading pathway diagram from KEGG can take 3 minutes. ")
    )
  )
}