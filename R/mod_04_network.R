# Network tab module
#
# An interactive network of the significant pathways (visNetwork), plus a
# static version in a pop-up, with downloads of the network as HTML, image,
# nodes and edges. Pathways sharing at least the edge cutoff of their genes
# are connected (enrichmentNetwork()).
#
#   mod_network_ui(id)      the tab, including the static-network pop-up
#   mod_network_server(id, goButton, input_text_b, show_pathway_id, selectOrg,
#                      selectGO, significantOverlaps)
#
# Arguments of mod_network_server(), all reactives from the main server:
#   goButton             input$goButton (the Submit button)
#   input_text_b         input$input_text_b (background genes; read so the
#                        network updates when they change)
#   show_pathway_id      input$show_pathway_id
#   selectOrg, selectGO  input$selectOrg and input$selectGO
#   significantOverlaps  the enrichment results
#
# significantOverlaps3() and significantOverlaps4() prepare the pathways for
# the interactive and the static network with prepare_pathways(); they differ
# only in which "Wrap text" box they read.
#
# The interactive network and its downloads share network_vis_data() and
# build_visnetwork() from fct_plots.R.
# ----------------------------------------------------------------------------


mod_network_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Network",
    value = 5,
    fluidRow(
      column(2, actionButton(ns("layoutButton"), "Change layout")),
      column(2, actionButton(ns("GONetwork"), "Static plot")),
      column(2, h5("Edge cutoff:"), align = "left"),
      column(2, numericInput(ns("edgeCutoff"), label = NULL, value = 0.30, min = 0, max = 1, step = .1), align = "right"),
      column(2, checkboxInput(ns("wrapTextNetwork"), "Wrap text", value = TRUE))
    ),
    visNetworkOutput(ns("enrichmentNetworkPlotInteractive"), height = "800px", width = "800px"),
    downloadButton(ns("enrichmentNetworkPlotInteractiveDownload"), "Download HTML"),
    downloadButton(ns("downloadEdges"), "Edges"),
    downloadButton(ns("downloadNodes"), "Nodes"),
    h5("Similar to the Tree tab, this interactive plot also shows the relationship between enriched pathways.
        Two pathways (nodes) are connected if they share 20% (default) or more genes.
        You can move the nodes by dragging them, zoom in and out by scrolling,
        and shift the entire network by click on an empty point and drag.
        Darker nodes are more significantly enriched gene sets.
        Bigger nodes represent larger gene sets.
        Thicker edges represent more overlapped genes."),

    bsModal(ns("InteractiveNetwork"), "Interactive enrichment networks ", ns("GONetwork"),
      size = "large",
      fluidRow(
        column(2, actionButton(ns("layoutButtonStatic"), "Change layout")),
        column(2, downloadButton(ns("enrichmentNetworkPlotDownload"), "Download")),
        column(2, checkboxInput(ns("wrapTextNetworkStatic"), "Wrap text", value = FALSE))
      ),
      plotOutput(ns("enrichmentNetworkPlot"))
    )
  )
}


mod_network_server <- function(id, goButton, input_text_b, show_pathway_id, selectOrg,
                               selectGO, significantOverlaps) {
  moduleServer(id, function(input, output, session) {
    # The pathways for the interactive network, wrapped if "Wrap text" is ticked.
    significantOverlaps3 <- reactive({
      if (goButton() == 0) {
        return()
      }
      tem <- input_text_b() # just to make it re-calculate if user changes background
      prepare_pathways(significantOverlaps(), show_pathway_id(), selectOrg(), selectGO(),
        wrap = input$wrapTextNetwork
      )
    })

    # The pathways for the static network, wrapped if its "Wrap text" is ticked.
    significantOverlaps4 <- reactive({
      if (goButton() == 0) {
        return()
      }
      tem <- input_text_b() # just to make it re-calculate if user changes background
      prepare_pathways(significantOverlaps(), show_pathway_id(), selectOrg(), selectGO(),
        wrap = input$wrapTextNetworkStatic
      )
    })

    output$enrichmentNetworkPlot <- renderPlot(
      {
        if (is.null(significantOverlaps4())) {
          return(NULL)
        }

        enrichmentNetwork(significantOverlaps4(), layoutButton = input$layoutButtonStatic, edge.cutoff = input$edgeCutoff)
      },
      height = 900
    )

    output$enrichmentNetworkPlotDownload <- downloadHandler(
      filename = "enrichmentPlotNetworkPathway.tiff",
      content = function(file) {
        tiff(file, width = 12, height = 12, units = "in", res = 300, compression = "lzw")
        enrichmentNetwork(significantOverlaps4(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
        dev.off()
      }
    )

    # The graph for the interactive network and its downloads. A function, not
    # a reactive(): enrichmentNetwork() sets the random seed that the layout
    # step after it relies on, so each output builds the graph just before use.
    network_graph <- function() {
      enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
    }

    output$enrichmentNetworkPlotInteractive <- renderVisNetwork({
      if (is.null(significantOverlaps3())) {
        return(NULL)
      }

      build_visnetwork(network_vis_data(network_graph())) %>%
        visExport(
          type = "jpeg",
          name = "export-network",
          float = "left",
          label = "Export image",
          background = "white",
          style = ""
        )
    })

    output$enrichmentNetworkPlotInteractiveDownload <- downloadHandler(
      filename = "enrichmentPlotNetwork.html",
      content = function(file) {
        build_visnetwork(network_vis_data(network_graph())) %>%
          visSave(file = file, background = "white")
      }
    )

    output$downloadNodes <- downloadHandler(
      filename = function() {
        "network_nodes.csv"
      },
      content = function(file) {
        write.csv(network_vis_data(network_graph())$nodes, file, row.names = FALSE)
      }
    )

    output$downloadEdges <- downloadHandler(
      filename = function() {
        "network_edges.csv"
      },
      content = function(file) {
        write.csv(network_vis_data(network_graph())$edges, file, row.names = FALSE)
      }
    )
  })
}
