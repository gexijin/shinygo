# R/mod_string.R
mod_string_ui <- function() {
  tabPanel("STRING",
    value = 11,

    textOutput("STRINGDB_mapping_stat"),
    tags$head(tags$style("#STRINGDB_mapping_stat{color: blue;font-size: 15px;}")),
    br(),
    actionButton("ModalPPI", "PPI network of DEGs"), br(), br(),
    selectInput("STRINGdbGO",
      label = "Functional Enrichment",
      choices = list(
        "GO Biological Process" = "Process",
        "GO Cellular Component" = "Component",
        "GO Molecular Function" = "Function",
        "KEGG" = "KEGG",
        "Pfam" = "Pfam",
        "InterPro" = "InterPro"
      ),
      selected = "Process"
    ),
    downloadButton("STRING_enrichmentDownload"),
    tableOutput("stringDB_GO_enrichment"),
    br(),br(),
    h5(
      "To validate your results independent of our algorithm and database, 
      your genes are sent to STRING-db website for enrichment analysis. 
      This also enables the
        retrieval of a protein-protein network. If it is running, 
        please wait until it finishes. The second time it is faster."
    ),

    bsModal("ModalExamplePPI", "Protein-protein interaction networks ", "ModalPPI",
      size = "large",
      h5("By sending your genes to the STRING website,
          shinyGO is retrieving a sub-network, calculating PPI enrichment,
          and generating custom URLs to the STRING website containing your genes. This can take 5 minutes. Patience will pay off! "),
      sliderInput("nGenesPPI", label = h5("Genes to include:"), min = 0, max = 400, value = 20, step = 10),
      # ,htmlOutput("stringDB_network_link")
      # ,tags$head(tags$style("#stringDB_network_link{color: blue; font-size: 15px;}"))

      plotOutput("stringDB_network1")
    )
  )
}