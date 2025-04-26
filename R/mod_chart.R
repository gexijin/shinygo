# R/mod_chart.R

#' chart UI Function
#'
#' @description A shiny Module for the enrichment chart visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("enrichChart"), width = "100%", height = "100%"),
    fluidRow(
      column(3, selectInput(
        inputId = ns("SortPathwaysPlot"),
        label = h5("Sort Pathway by"),
        choices = columnSelection,
        selected = columnSelection[2]
      )),
      column(3, selectInput(
        inputId = ns("SortPathwaysPlotX"),
        label = h5("x-axis"),
        choices = columnSelection[1:3],
        selected = columnSelection[2]
      )),
      column(3, selectInput(
        inputId = ns("SortPathwaysPlotColor"),
        label = h5("Color"),
        choices = columnSelection[1:3],
        selected = columnSelection[1]
      )),
      column(3, selectInput(
        inputId = ns("SortPathwaysPlotSize"),
        label = h5("Size"),
        choices = columnSelection[1:3],
        selected = columnSelection[3]
      ))
    ), # first row

    fluidRow(
      column(3, numericInput(
        inputId = ns("SortPathwaysPlotFontSize"),
        label = h5("Font Size"),
        value = 12,
        min = 3,
        max = 18,
        step = 1
      )),
      column(3, numericInput(
        inputId = ns("SortPathwaysPlotMarkerSize"),
        label = h5("Circle Size"),
        value = 4,
        min = 0,
        max = 10,
        step = 1
      )),
      column(3, selectInput(
        inputId = ns("SortPathwaysPlotHighColor"),
        label = h5("Color:High"),
        choices = c("red", "orange", "yellow", "green", "blue", "purple"),
        selected = "red"
      )),
      column(3, selectInput(
        inputId = ns("SortPathwaysPlotLowColor"),
        label = h5("Color:Low"),
        choices = c("red", "orange", "yellow", "green", "blue", "purple"),
        selected = "blue"
      ))
    ), # 2nd row

    fluidRow(
      column(width = 3, selectInput(
        inputId = ns("enrichChartType"),
        label = h5("Chart type"),
        choices = c("lollipop", "dotplot", "barplot", "barplot_inside"),
        selected = "lollipop"
      )),
      column(3, selectInput(
        inputId = ns("enrichChartAspectRatio"),
        label = h5("Aspect Ratio"),
        choices = .1 * (5:30),
        selected = 2
      )),
      column(
        width = 3,
        selectInput(
          inputId = ns("ggplot2_theme"),
          label = h5("Plot theme:"),
          choices = c(
            "default", # no change
            "gray",
            "bw",
            "light",
            "dark",
            "classic",
            "minimal",
            "linedraw",
            "Add grid"
          ),
          selected = "default",
          selectize = FALSE
        ),
        tippy::tippy_this(
          ns("ggplot2_theme"),
          "Changes the ggplot2 theme for all plots, including those in the Plots tab.",
          theme = "light-border"
        )
      ),
      column(3, style = "margin-top: 25px;", mod_download_images_ui(ns("download_barplot")))
    ) # 3rd row
  )
}



#' chart Server Functions
#'
#' @noRd
mod_chart_server <- function(id, significantOverlaps, input_goButton, input_selectOrg, 
                            input_selectGO, input_maxTerms, input_show_pathway_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ggplot2 object for the enrichment chart;
    # used both for display and download
    enrichChartObject <- reactive({
      if (input_goButton() == 0) {
        return()
      }
      
      if (is.null(significantOverlaps())) {
        return(NULL)
      }
      if (ncol(significantOverlaps()$x) == 1) {
        return(NULL)
      } # no significant ones found.
      
      # Create local variables from the input parameters passed to the module
      tem <- input_selectOrg()
      tem <- input$SortPathwaysPlot
      tem <- input$SortPathwaysPlotX
      tem <- input$SortPathwaysPlotSize
      tem <- input$SortPathwaysPlotColor
      tem <- input$SortPathwaysPlotFontSize
      tem <- input$SortPathwaysPlotMarkerSize
      tem <- input$SortPathwaysPlotHighColor
      tem <- input$SortPathwaysPlotLowColor
      tem <- input$enrichChartType
      tem <- input$enrichChartAspectRatio
      tem <- input_maxTerms()
      tem <- input$ggplot2_theme
      tem <- input_show_pathway_id()
      
      isolate({
        goTable <- significantOverlaps()$x[, 1:5]
        
        # Remove spaces in col names
        colnames(goTable) <- gsub(" ", "", colnames(goTable))
        
        x <- input$SortPathwaysPlotX
        size <- input$SortPathwaysPlotSize
        colorBy <- input$SortPathwaysPlotColor
        fontSize <- input$SortPathwaysPlotFontSize
        markerSize <- input$SortPathwaysPlotMarkerSize
        
        # validate values; users can input any numeric value outside the range
        if (fontSize < 1 | fontSize >= 20) {
          fontSize <- 12
        }
        if (markerSize < 0 | markerSize > 20) {
          markerSize <- 4
        }
        
        # convert to vector so that we can look up the readable names of columns
        columns <- unlist(columnSelection)
        
        goTable$EnrichmentFDR <- -log10(goTable$EnrichmentFDR)
        ix <- which(colnames(goTable) == input$SortPathwaysPlot)
        
        # sort the pathways
        if (ix > 0 && ix < dim(goTable)[2]) {
          goTable <- goTable[order(goTable[, ix], decreasing = TRUE), ]
        }
        
        # remove pathway ID only in Ensembl species
        if (!input_show_pathway_id() && as.integer(input_selectOrg()) > 0) {
          goTable$Pathway <- remove_pathway_id(goTable$Pathway, input_selectGO())
        }
        
        # Error when two pathways are of the same name due to truncation of long pathways
        goTable$Pathway <- mark_duplicates(goTable$Pathway)
        
        # convert to factor so that the levels are not reordered by ggplot2
        goTable$Pathway <- factor(goTable$Pathway, levels = rev(goTable$Pathway))
        
        p <- ggplot(goTable, aes_string(x = x, y = "Pathway", size = size, color = colorBy)) +
          geom_point() +
          scale_color_continuous(
            low = input$SortPathwaysPlotLowColor,
            high = input$SortPathwaysPlotHighColor,
            name = names(columns)[columns == colorBy],
            guide = guide_colorbar(reverse = TRUE)
          )
        
        p <- refine_ggplot2(
          p = p,
          gridline = FALSE,
          ggplot2_theme = input$ggplot2_theme
        )
        
        p <- p + scale_size(range = c(1, markerSize)) +
          xlab(names(columns)[columns == x]) +
          ylab(NULL) +
          guides(
            size = guide_legend(order = 2, title = names(columns)[columns == size]),
            color = guide_colorbar(order = 1)
          ) +
          theme(axis.text = element_text(size = fontSize), axis.title = element_text(size = 12)) +
          theme(
            legend.title = element_text(size = 12), # decrease legend font
            legend.text = element_text(size = 12)
          ) +
          guides(shape = guide_legend(override.aes = list(size = 5))) +
          guides(color = guide_legend(override.aes = list(size = 5)))
        
        if (input$enrichChartType == "dotplot") {
          p <- p
        } else if (input$enrichChartType == "lollipop") {
          p <- p +
            geom_segment(aes_string(
              x = 0,
              xend = x,
              y = "Pathway",
              yend = "Pathway"
            ),
            size = 1
            )
        } else if (input$enrichChartType == "barplot") {
          p <- ggplot(goTable, aes_string(x = x, y = "Pathway", fill = colorBy)) +
            geom_col(width = 0.8, position = position_dodge(0.7)) +
            scale_fill_continuous(
              low = input$SortPathwaysPlotLowColor,
              high = input$SortPathwaysPlotHighColor,
              name = names(columns)[columns == colorBy],
              guide = guide_colorbar(reverse = TRUE)
            )
          p <- refine_ggplot2(
            p = p,
            gridline = FALSE,
            ggplot2_theme = input$ggplot2_theme
          )
          p <- p +
            xlab(names(columns)[columns == x]) +
            ylab(NULL) +
            theme(axis.text = element_text(size = fontSize))
        } else if(input$enrichChartType == "barplot_inside") {
          p <- ggplot(goTable, aes_string(x = x, y = "Pathway", fill = colorBy)) +
            geom_col(width = 0.8, position = position_dodge(0.7)) +
            scale_fill_continuous(
              low = input$SortPathwaysPlotLowColor,
              high = input$SortPathwaysPlotHighColor,
              name = names(columns)[columns == colorBy],
              guide = guide_colorbar(reverse = TRUE)
            )
          p <- refine_ggplot2(
            p = p,
            gridline = FALSE,
            ggplot2_theme = input$ggplot2_theme
          )
          p <- p +
            xlab(names(columns)[columns == x]) +
            ylab(NULL) +
            theme(axis.text = element_text(size = fontSize))+
            geom_text( # add text inside the bars
              aes_string(x = 0, label = "Pathway"),
              hjust = 0, 
              #vjust = -1, 
              color="black", 
              size=fontSize / 2
            )  +
            theme(
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank()
            )
        }
        
        return(p)
      }) # isolate
    })
    
    # Observe changes to chart type and update settings accordingly
    observeEvent(input$enrichChartType, {
      req(input$enrichChartType == "barplot_inside") 
      updateSliderInput(
        session = session,
        inputId = "SortPathwaysPlotFontSize",
        value = 10
      )
      updateSliderInput(
        session = session,
        inputId = "enrichChartAspectRatio",
        value = 1.5
      )
      updateSelectInput(
        session = session,
        inputId = "SortPathwaysPlotLowColor",
        selected = "yellow"
      )
      updateSelectInput(
        session = session,
        inputId = "maxTerms",
        selected = "15"
      )
      #show notification that maxTerms is set to 15
      showNotification(
        "To improve the chart, we adjusted the aspect ratio, font size, and color:low. The number of pathways to show is set to 15.",
        type = "message",
        duration = 5
      )
    })
    
    # Enrichment plot for display on the screen
    output$enrichChart <- renderPlot(
      {
        enrichChartObject()
      },
      # height increases as the number of terms increase. max at 1200, min 350
      height = function() {
        round(max(350, min(2500, round(18 * as.numeric(input_maxTerms())))))
      },
      width = function() {
        round(max(350, min(2500, round(18 * as.numeric(input_maxTerms())))) * as.numeric(input$enrichChartAspectRatio))
      }
    )
    
    # Download handler for the barplot
    download_barplot <- mod_download_images_server(
      "download_barplot",
      filename = "barplot",
      figure = reactive({
        enrichChartObject()
      }),
      width = 8,
      height = reactive({
        round(8 / as.numeric(input$enrichChartAspectRatio), 1)
      })
    )
    
    # Return the theme selection to be used in other modules
    return(reactive({ input$ggplot2_theme }))
  })
}