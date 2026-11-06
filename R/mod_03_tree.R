# Tree tab module
#
# A clustering tree of the significant pathways (enrichmentPlot()), sized by
# the number of pathways shown, with an aspect-ratio menu and a download.
#
#   mod_tree_ui(id)      the tab
#   mod_tree_server(id, goButton, maxTerms, significantOverlaps2)
#
# Arguments of mod_tree_server(), all reactives from the main server:
#   goButton              input$goButton (the Submit button)
#   maxTerms              input$maxTerms (number of pathways shown)
#   significantOverlaps2  the significant pathways, prepared for the tree; kept
#                         in server.R because the test hook also reads it
#
# The download button is the mod_download_images module, nested inside this
# one as "download_tree".
# ----------------------------------------------------------------------------


mod_tree_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Tree",
    value = 4,
    h5("A hierarchical clustering tree summarizes the correlation among significant pathways
                listed in the Enrichment tab. Pathways with many shared genes are clustered together.
                  Bigger dots indicate more significant P-values. The width of the plot can be
                  changed by adjusting the width of your browser window."),
    fluidRow(
      column(width = 3, selectInput(
        inputId = ns("treeChartAspectRatio"),
        label = h5("Aspect Ratio"),
        choices = .1 * (5:40),
        selected = 2
      )),
      column(3, style = "margin-top: 25px;", mod_download_images_ui(ns("download_tree"), label = "Download"))
    ),
    plotOutput(ns("GOTermsTree"))
  )
}


mod_tree_server <- function(id, goButton, maxTerms, significantOverlaps2) {
  moduleServer(id, function(input, output, session) {
    output$GOTermsTree <- renderPlot(
      {
        if (goButton() == 0) {
          return(NULL)
        }
        if (is.null(significantOverlaps2())) {
          return(NULL)
        }
        tem <- maxTerms()
        # enrichmentPlot(significantOverlaps2(), 56  )
        tree_plot()
      },
      height = function() {
        round(max(350, min(2500, round(18 * as.numeric(maxTerms())))))
      },
      width = function() {
        width1 <- round(max(350, min(1000, round(18 * as.numeric(maxTerms())))) * as.numeric(input$treeChartAspectRatio))
        return(min(width1, 1000)) # max width is 1000
      }
    )



    tree_plot <- reactive({
      if (goButton() == 0) {
        return(NULL)
      }
      if (is.null(significantOverlaps2())) {
        return(NULL)
      }
      tem <- maxTerms()
      p <- enrichmentPlot(significantOverlaps2(), 45)
      return(p)
    })

    download_tree <- mod_download_images_server(
      "download_tree",
      filename = "tree_plot",
      figure = reactive({
        tree_plot()
      }),
      width = 10,
      height = round(10 / as.numeric(input$treeChartAspectRatio), 1)
    )
  })
}
