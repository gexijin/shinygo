#' 03_tree UI Function
#'
#' @description Renders the Tree tab: a hierarchical clustering tree
#'   summarizing the correlation among significant pathways from the
#'   Enrichment tab, plus a button to download the plot.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
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


#' 03_tree Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param significantOverlaps2 A reactive (defined in the main server)
#'   returning the significant enrichment results, with linked pathway
#'   names/IDs stripped down to plain text for use in the tree plot.
#' @param max_terms A reactive returning the value of the "# pathways to
#'   show" input on the Enrichment tab; also drives the plot's height and
#'   width.
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#'
#' @noRd
mod_03_tree_server <- function(id, significantOverlaps2, max_terms, go_button) {
  moduleServer(id, function(input, output, session) {
    # Clustering tree of significant pathways, sized by the number of
    # pathways shown (max_terms) and the user's chosen aspect ratio.
    output$GOTermsTree <- renderPlot(
      {
        if (go_button() == 0) {
          return(NULL)
        }
        if (is.null(significantOverlaps2())) {
          return(NULL)
        }
        tem <- max_terms()
        tree_plot()
      },
      height = function() {
        round(max(350, min(2500, round(18 * as.numeric(max_terms())))))
      },
      width = function() {
        width1 <- round(max(350, min(1000, round(18 * as.numeric(max_terms())))) * as.numeric(input$treeChartAspectRatio))
        return(min(width1, 1000)) # max width is 1000
      }
    )

    tree_plot <- reactive({
      if (go_button() == 0) {
        return(NULL)
      }
      if (is.null(significantOverlaps2())) {
        return(NULL)
      }
      tem <- max_terms()
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
