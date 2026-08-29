#' 07_groups UI Function
#'
#' @description Renders the Groups tab: a download button and a table
#'   grouping the user's genes by high-level GO Biological Process terms.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_groups_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadGrouping"), "Download"),
    h5("Your genes are grouped by functional categories defined by high-level GO terms. "),
    tableOutput(ns("grouping"))
  )
}


#' 07_groups Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param significantOverlaps A reactive (defined in the main server) that
#'   returns the filtered/sorted enrichment result list; its `$groupings`
#'   element is what's shown here.
#' @param goButton A reactive returning the value of the main "Submit" action button.
#' @param quotes A character vector of quotes shown while the table loads.
#'
#' @noRd
mod_07_groups_server <- function(id, significantOverlaps, goButton, quotes) {
  moduleServer(id, function(input, output, session) {
    output$grouping <- renderTable(
      {
        if (goButton() == 0) {
          return()
        }
        myMessage <- "Just a minute. Matching your genes with level 2 and level 3 Gene Ontology biological process terms.
	       This can take up to 1 minute as we have to glue together a large number of gene names. "
        withProgress(message = sample(quotes, 1), detail = myMessage, {
          tem <- significantOverlaps()

          incProgress(1, detail = paste("Done"))
        })
        tem$groupings
      },
      digits = 1,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = T
    )

    output$downloadGrouping <- downloadHandler(
      filename = function() {
        "GO_Gropus.csv"
      },
      content = function(file) {
        write.csv(significantOverlaps()$groupings, file, row.names = FALSE)
      }
    )
  })
}
