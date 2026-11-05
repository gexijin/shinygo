# Figure download module
#
# A "Download" button that opens a pop-up where the user sets the figure size
# and downloads it as PDF, PNG or SVG. Reused by the Chart, Tree and Plots
# tabs. Written by Emma Spors and Ben Derenge.
#
#   mod_download_images_ui()      the button
#   mod_download_images_server()  the pop-up and the three downloads
#
# Usage, with the same id in both:
#   UI:     mod_download_images_ui("download_tree", label = "Download")
#   server: mod_download_images_server("download_tree", filename = "tree_plot",
#                                      figure = reactive(tree_plot()))
# ----------------------------------------------------------------------------


# The button that opens the download pop-up.
mod_download_images_ui <- function(id, label = "Download Plot") {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("download_popup"),
      label = label
    )
  )
}


# id:       the module id, the same as in mod_download_images_ui().
# filename: file name without extension.
# figure:   a reactive that returns the plot; it is drawn with print(), so a
#           ggplot object works directly.
# width, height: default size in inches.
mod_download_images_server <- function(id, filename, figure, width = 8, height = 6) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    min_size <- 2 # min for width or height
    max_size <- 30 # max for width or height

    # Shiny doesn't enforce numericInput's min and max, so clamp the size here.
    # Anything that isn't a number falls back to the default.
    figure_width <- reactive({
      if (is.numeric(input$width)) {
        return(max(min_size, min(max_size, input$width, na.rm = TRUE)))
      } else {
        return(width)
      }
    })

    # BUG NOTE: this checks input$width, not input$height, before using the height.
    figure_height <- reactive({
      if (is.numeric(input$width)) {
        return(max(min_size, min(max_size, input$height, na.rm = TRUE)))
      } else {
        return(height)
      }
    })

    # The pop-up: size inputs and one button per file format.
    observeEvent(
      input$download_popup,
      {
        showModal(
          modalDialog(
            numericInput(
              inputId = ns("width"),
              label = "Width (in)",
              value = width,
              min = min_size,
              max = max_size
            ),
            numericInput(
              inputId = ns("height"),
              label = "Height (in)",
              value = height,
              min = min_size,
              max = max_size
            ),
            h5("The plot will be rendered differently depending on size. When the dimensions are too small,
            error or blank plot will be generated."),
            downloadButton(
              outputId = ns("dl_pdf"),
              label = "PDF"
            ),
            downloadButton(
              outputId = ns("dl_png"),
              label = "PNG"
            ),
            downloadButton(
              outputId = ns("dl_svg"),
              label = "SVG"
            ),
            size = "s" # small dialog
          )
        )
      }
    )

    output$dl_pdf <- downloadHandler(
      filename = paste0(filename, ".pdf"),
      content = function(file) {
        pdf(
          file,
          width = figure_width(),
          height = figure_height()
        )
        print(figure())
        dev.off()
      }
    )

    # PNG at 360 dpi.
    output$dl_png <- downloadHandler(
      filename = paste0(filename, ".png"),
      content = function(file) {
        png(
          file,
          res = 360,
          width = figure_width(),
          height = figure_height(),
          units = "in"
        )
        print(figure())
        dev.off()
      }
    )

    output$dl_svg <- downloadHandler(
      filename = paste0(filename, ".svg"),
      content = function(file) {
        svg(
          file,
          width = figure_width(),
          height = figure_height()
        )
        print(figure())
        dev.off()
      }
    )
  })
}
