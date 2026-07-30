#' 06_genes UI Function
#'
#' @description Renders the Genes tab: a download button, a checkbox to
#'   toggle detailed gene descriptions, and a table listing every submitted
#'   gene alongside its mapped IDs and annotation.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_06_genes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, downloadButton(ns("downloadGeneInfo"), "More info")),
      column(4, checkboxInput(ns("showDetailedGeneInfo"), "Detailed Description", value = FALSE))
    ),
    tableOutput(ns("conversionTable"))
  )
}


#' 06_genes Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param conversionTableData A reactive (defined in the main server) that
#'   returns the merged conversion/annotation table for the user's genes.
#'   Also consumed by the (not yet modularized) STRING tab, so it stays
#'   defined and owned by the main server rather than moving in here.
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#' @param select_org A reactive returning the value of the selected organism
#'   input; read here only to gate rendering until a species is selected.
#'
#' @noRd
mod_06_genes_server <- function(id, conversionTableData, go_button, select_org) {
  moduleServer(id, function(input, output, session) {
    output$conversionTable <- renderTable(
      {
        if (go_button() == 0) {
          return()
        } # still have problems when geneInfo is not found!!!!!
        req(select_org())
        tem <- input$showDetailedGeneInfo
        isolate({
          # STRINGdb species only get the minimal 3-column conversion table;
          # everything else gets the fuller annotated table built below.
          if (dim(conversionTableData())[2] < 9) {
            df <- conversionTableData()
          } else {
            df <- conversionTableData()[, 1:9]
            # show detailed gene info for string species
            if (!input$showDetailedGeneInfo) {
              df$Description <- gsub(";.*|\\[.*", "", df$Description)
            }
            # Remove columns with all missing values; chr and start position in STRINGdb species
            df <- df[, which(!apply(is.na(df), 2, sum) == nrow(df))]
            df$Species <- gsub("STRINGdb", "", df$Species)

            df$Type <- gsub(".*pseudogene", "pseudo", df$Type)
            # coding is not shown
            df$Type <- gsub("protein_coding", "coding", df$Type)
            df$Type <- gsub("_gene", "", df$Type)
            df$Chr[nchar(df$Chr) > 50] <- ""

            # link out to Ensembl for genes with an Ensembl ID
            ix <- grepl("ENS", df$"Ensembl Gene ID")
            if (sum(ix) > 0) {
              tem <- paste0(
                "<a href='http://www.ensembl.org/id/",
                df$"Ensembl Gene ID",
                "' target='_blank'>",
                df$"Ensembl Gene ID",
                "</a>"
              )
              df$"Ensembl Gene ID"[ix] <- tem[ix]
            }
            # link out to NCBI for genes with an Entrez ID
            ix <- !is.na(as.numeric(df$Entrez))
            if (sum(ix) > 0) {
              tem <- paste0(
                "<a href='https://www.ncbi.nlm.nih.gov/gene/",
                df$Entrez,
                "' target='_blank'>",
                df$Entrez,
                "</a>"
              )
              df$Entrez[ix] <- tem[ix]
            }
          }
          return(df)
        }) # avoid showing things initially
      },
      digits = 4,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = T,
      sanitize.text.function = function(x) x
    )

    output$downloadGeneInfo <- downloadHandler(
      filename = function() {
        "geneInfo.csv"
      },
      content = function(file) {
        write.csv(conversionTableData(), file, row.names = FALSE)
      }
    )
  })
}
