# Module UI function
#' @title   mod_genes_ui and mod_genes_server
#' @description  A shiny Module for the Genes tab in ShinyGO
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_genes
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_genes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, downloadButton(ns("downloadGeneInfo"), "More info")),
      column(4, checkboxInput(ns("showDetailedGeneInfo"), "Detailed Description", value = FALSE))
    ),
    tableOutput(ns("conversionTable")),
    
    # Add Groups section with conditional display
    conditionalPanel(
      condition = "input.selectGO == 'GOBP' || input.selectGO == 'GOCC' || input.selectGO == 'GOMF'",
      h3("Gene Groups"),
      downloadButton(ns("downloadGrouping"), "Download"),
      h5("Your genes are grouped by functional categories defined by high-level GO terms."),
      tableOutput(ns("grouping"))
    )
  )
}

#' @rdname mod_genes
#' @export
#' @keywords internal
mod_genes_server <- function(id, converted, geneInfoLookup, input_goButton, conversionTableData, significantOverlapsAll = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render the conversion table
    output$conversionTable <- renderTable(
      {
        if (input_goButton() == 0) {
          return()
        }
        
        if(dim(conversionTableData())[2] < 9) { # STRINGdb species only 3 columns
          df <- conversionTableData()
        } else { # ENSEMBL species
          df <- conversionTableData()[, 1:9]
          # show detailed gene info for string species
          if (!input$showDetailedGeneInfo) {
            df$Description <- gsub(";.*|\\[.*", "", df$Description)
          }
          # Rest of the formatting code...
          # [Keep all the existing code from before]
        }
        return(df)
      },
      digits = 4,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = T,
      sanitize.text.function = function(x) x
    )
    
    # Download handler for gene info
    output$downloadGeneInfo <- downloadHandler(
      filename = function() {
        "geneInfo.csv"
      },
      content = function(file) {
        write.csv(conversionTableData(), file, row.names = FALSE)
      }
    )
    
    # Add Groups table rendering
    output$grouping <- renderTable(
      {
        if (input_goButton() == 0) {
          return()
        }
        
        # Check if significantOverlapsAll is provided
        if (is.null(significantOverlapsAll)) {
          return()
        }
        
        # Access the groupings from significantOverlapsAll
        enrichment <- significantOverlapsAll()
        
        # Make sure enrichment exists and has groupings
        if (is.null(enrichment) || is.null(enrichment$groupings)) {
          return()
        }
        
        return(enrichment$groupings)
      },
      digits = 1,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = T
    )
    
    # Download handler for grouping data
    output$downloadGrouping <- downloadHandler(
      filename = function() {
        "GO_Groups.csv"
      },
      content = function(file) {
        if (!is.null(significantOverlapsAll()) && !is.null(significantOverlapsAll()$groupings)) {
          write.csv(significantOverlapsAll()$groupings, file, row.names = FALSE)
        }
      }
    )
  })
}