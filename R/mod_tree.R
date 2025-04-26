#' Tree UI Function
#'
#' @description A shiny Module to display hierarchical clustering tree of enriched pathways.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#'
mod_tree_ui <- function(id) {
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

#' Tree Server Function
#'
#' @param id The module id
#' @param significantOverlaps2 A reactive function that returns a data frame of significant GO terms
#' @param input_goButton A reactive function that returns the value of input$goButton
#' @param input_maxTerms A reactive function that returns the value of input$maxTerms
#'
#' @importFrom shiny moduleServer
#'
mod_tree_server <- function(id, significantOverlaps2, input_goButton, input_maxTerms) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create the tree plot
    tree_plot <- reactive({
      if (input_goButton() == 0) {
        return(NULL)
      }
      if (is.null(significantOverlaps2())) {
        return(NULL)
      }
      tem <- input_maxTerms()
      p <- enrichmentPlot(significantOverlaps2(), 45)
      return(p)
    })
    
    # Render the tree plot
    output$GOTermsTree <- renderPlot(
      {
        tree_plot()
      },
      height = function() {
        round(max(350, min(2500, round(18 * as.numeric(input_maxTerms())))))
      },
      width = function() {
        width1 <- round(max(350, min(1000, round(18 * as.numeric(input_maxTerms())))) * as.numeric(input$treeChartAspectRatio))
        return(min(width1, 1000)) # max width is 1000
      }
    )
    
    # Set up download handler for the tree plot
    mod_download_images_server(
      "download_tree",
      filename = "tree_plot",
      figure = reactive({
        tree_plot()
      }),
      width = 10,
      height = reactive({
        round(10 / as.numeric(input$treeChartAspectRatio), 1)
      })
    )
  })
}