# Groups tab module
#
# The user's genes grouped by high-level GO Biological Process terms (GO levels
# 2 and 3), from FindOverlap(), with a download. The main server hides this
# tab unless a GO database is selected.
#
#   mod_groups_ui(id)      the tab
#   mod_groups_server(id, goButton, significantOverlaps)
#
# Arguments of mod_groups_server(), both reactives from the main server:
#   goButton             input$goButton (the Submit button)
#   significantOverlaps  the enrichment results; the groups are its $groupings
# ----------------------------------------------------------------------------


mod_groups_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Groups",
    value = 7,
    downloadButton(ns("downloadGrouping"), "Download"),
    h5("Your genes are grouped by functional categories defined by high-level GO terms. "),
    tableOutput(ns("grouping"))
  )
}


mod_groups_server <- function(id, goButton, significantOverlaps) {
  moduleServer(id, function(input, output, session) {
    output$grouping <- renderTable(
      {
        if (goButton() == 0) {
          return()
        }
        myMessage <- "Just a minute. Matching your genes with level 2 and level 3 Gene Ontology biological process terms.
	       This can take up to 1 minute as we have to glue together a large number of gene names. "
        withProgress(message = sample(.app$quotes, 1), detail = myMessage, {
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
