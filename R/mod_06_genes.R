# Genes tab module
#
# A table of the user's genes: pasted ID, symbol, Ensembl and Entrez IDs (as
# links), type, location and description, plus a download of the full
# gene information.
#
#   mod_genes_ui(id)      the tab
#   mod_genes_server(id, goButton, selectOrg, conversionTableData)
#
# Arguments of mod_genes_server(), all reactives from the main server:
#   goButton             input$goButton (the Submit button)
#   selectOrg            input$selectOrg (the species ID)
#   conversionTableData  the gene table; built in server.R, as the STRING
#                        tab uses it too
# ----------------------------------------------------------------------------


mod_genes_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Genes",
    value = 6,
    fluidRow(
      column(3, downloadButton(ns("downloadGeneInfo"), "More info")),
      column(4, checkboxInput(ns("showDetailedGeneInfo"), "Detailed Description", value = FALSE))
    ),
    tableOutput(ns("conversionTable"))
  )
}


mod_genes_server <- function(id, goButton, selectOrg, conversionTableData) {
  moduleServer(id, function(input, output, session) {
    output$conversionTable <- renderTable(
      {
        if (goButton() == 0) {
          return()
        } # still have problems when geneInfo is not found!!!!!
        req(selectOrg())
        tem <- input$showDetailedGeneInfo
        isolate({
          if(dim(conversionTableData())[2] < 9) { # STRINGdb species only 3 columns
            df <- conversionTableData()
          } else { # ENSEMBL species
            df <- conversionTableData()[, 1:9]
            # show detailed gene info for string species
            if (!input$showDetailedGeneInfo) {
              df$Description <- gsub(";.*|\\[.*", "", df$Description)
            }
            # Remove columns with all missing values; chr and start possition in STRINGdb species
            df <- df[, which(!apply(is.na(df), 2, sum) == nrow(df))]
            df$Species <- gsub("STRINGdb", "", df$Species)

            #      df$Type <- gsub(".*_", "", df$Type)
            df$Type <- gsub(".*pseudogene", "pseudo", df$Type)
            # coding is not shown
            df$Type <- gsub("protein_coding", "coding", df$Type)
            df$Type <- gsub("_gene", "", df$Type)
            df$Chr[nchar(df$Chr) > 50] <- ""

            # first see if it is Ensembl gene ID-----------------------
            ix <- grepl("ENS", df$"Ensembl Gene ID")
            if (sum(ix) > 0) { # at least one has http?
              tem <- paste0(
                "<a href='http://www.ensembl.org/id/",
                df$"Ensembl Gene ID",
                "' target='_blank'>",
                df$"Ensembl Gene ID",
                "</a>"
              )
              # only change the ones with URL
              df$"Ensembl Gene ID"[ix] <- tem[ix]
            }
            # first see if it is Ensembl gene ID-----------------------
            ix <- !is.na(as.numeric(df$Entrez))
            if (sum(ix) > 0) { # at least one has http?
              tem <- paste0(
                "<a href='https://www.ncbi.nlm.nih.gov/gene/",
                df$Entrez,
                "' target='_blank'>",
                df$Entrez,
                "</a>"
              )
              # only change the ones with URL
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
