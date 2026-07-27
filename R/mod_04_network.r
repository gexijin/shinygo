mod_04_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
      ) # bsModal 2
  )}

  mod_04_network_server <- function(id, significant_overlaps, select_org, select_go, show_pathway_id, go_button, input_text_b) {
  moduleServer(id, function(input, output, session) {

      # duplicate of the above, with the word wrapping. This is for use in the network plot
  significantOverlaps3 <- reactive({
    if (go_button() == 0) {
      return()
    }
    tem <- input_text_b() # just to make it re-calculate if user changes background

    tem <- significant_overlaps()
    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem <- tem$x
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL

    # remove pathway ID  only in Ensembl species
    if (!show_pathway_id() && as.integer(select_org()) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, select_go())
    }

    if (input$wrapTextNetwork) {
      tem$Pathways <- wrap_strings(tem$Pathways)
    } # wrap long pathway names using default width of 30 10/21/19

    tem$Direction <- "Diff"
    tem
  })

  # duplicate of the above for word wrapping in static networkplot
  significantOverlaps4 <- reactive({
    if (go_button() == 0) {
      return()
    }
    tem <- input_text_b() # just to make it re-calculate if user changes background
    tem <- significant_overlaps()
    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem <- tem$x
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL

    # remove pathway ID  only in Ensembl species
    if (!show_pathway_id() && as.integer(select_org()) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, select_go())
    }
    if (input$wrapTextNetworkStatic) {
      tem$Pathways <- wrap_strings(tem$Pathways)
    } # wrap long pathway names using default width of 30 10/21/19

    tem$Direction <- "Diff"
    tem
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

  # note the same code is used twice as above. They need to be updated together!!!
  output$enrichmentNetworkPlotInteractive <- renderVisNetwork({
    if (is.null(significantOverlaps3())) {
      return(NULL)
    }

    g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
    data1 <- toVisNetworkData(g)

    # Color codes: https://www.rapidtables.com/web/color/RGB_Color.html
    data1$nodes$shape <- "dot"
    # remove the color change of nodes
    # data1$nodes <- subset(data1$nodes, select = -color)

    data1$nodes$size <- 5 + data1$nodes$size^2
    visNetwork(nodes = data1$nodes, edges = data1$edges, height = "700px", width = "700px") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visNodes(
        color = list(
          # background = "#32CD32",
          border = "#000000",
          highlight = "#FF8000"
        ),
        font = list(
          color = "#000000",
          size = 20
        ),
        borderWidth = 1,
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#A9A9A9", highlight = "#FFD700")
      ) %>%
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
      # jpeg(file, width = 12, height = 12, units = 'in', res = 300, compression = 'lzw')

      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)

      # Color codes: https://www.rapidtables.com/web/color/RGB_Color.html
      data1$nodes$shape <- "dot"
      # remove the color change of nodes
      # data1$nodes <- subset(data1$nodes, select = -color)

      data1$nodes$size <- 5 + data1$nodes$size^2
      g2 <-
        visNetwork(nodes = data1$nodes, edges = data1$edges, height = "700px", width = "700px") %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(
          color = list(
            # background = "#32CD32",
            border = "#000000",
            highlight = "#FF8000"
          ),
          font = list(
            color = "#000000",
            size = 20
          ),
          borderWidth = 1,
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#A9A9A9", highlight = "#FFD700")
        ) %>%
        visSave(file = file, background = "white")
    }
  )

  output$downloadNodes <- downloadHandler(
    filename = function() {
      "network_nodes.csv"
    },
    content = function(file) {
      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)
      data1$nodes$shape <- "dot"
      data1$nodes$size <- 5 + data1$nodes$size^2

      write.csv(data1$nodes, file, row.names = FALSE)
    }
  )
  output$downloadEdges <- downloadHandler(
    filename = function() {
      "network_edges.csv"
    },
    content = function(file) {
      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)
      data1$nodes$shape <- "dot"
      data1$nodes$size <- 5 + data1$nodes$size^2

      write.csv(data1$edges, file, row.names = FALSE)
    }
  )
  })}