# R/mod_chart.R
mod_chart_ui <- function() {
  tabPanel("Chart",
    value = 3,
    plotOutput("enrichChart", width = "100%", height = "100%"),
    fluidRow(
      column(3, selectInput(
        inputId = "SortPathwaysPlot",
        label = h5("Sort Pathway by"),
        choices = columnSelection,
        selected = columnSelection[2]
      )),
      column(3, selectInput(
        inputId = "SortPathwaysPlotX",
        label = h5("x-axis"),
        choices = columnSelection[1:3],
        selected = columnSelection[2]
      )),
      column(3, selectInput(
        inputId = "SortPathwaysPlotColor",
        label = h5("Color"),
        choices = columnSelection[1:3],
        selected = columnSelection[1]
      )),
      column(3, selectInput(
        inputId = "SortPathwaysPlotSize",
        label = h5("Size"),
        choices = columnSelection[1:3],
        selected = columnSelection[3]
      ))
    ), # first row

    fluidRow(
      column(3, numericInput(
        inputId = "SortPathwaysPlotFontSize",
        label = h5("Font Size"),
        value = 12,
        min = 3,
        max = 18,
        step = 1
      )),
      column(3, numericInput(
        inputId = "SortPathwaysPlotMarkerSize",
        label = h5("Circle Size"),
        value = 4,
        min = 0,
        max = 10,
        step = 1
      )),
      column(3, selectInput(
        inputId = "SortPathwaysPlotHighColor",
        label = h5("Color:High"),
        choices = c("red", "orange", "yellow", "green", "blue", "purple"),
        selected = "red"
      )),
      column(3, selectInput(
        inputId = "SortPathwaysPlotLowColor",
        label = h5("Color:Low"),
        choices = c("red", "orange", "yellow", "green", "blue", "purple"),
        selected = "blue"
      ))
    ), # 2nd row

    fluidRow(
      column(width = 3, selectInput(
        inputId = "enrichChartType",
        label = h5("Chart type"),
        choices = c("lollipop", "dotplot", "barplot", "barplot_inside"),
        selected = "lollipop"
      )),
      column(3, selectInput(
        inputId = "enrichChartAspectRatio",
        label = h5("Aspect Ratio"),
        choices = .1 * (5:30),
        selected = 2
      )),
      column(
        width = 3,
        selectInput(
          inputId = "ggplot2_theme",
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
          "ggplot2_theme",
          "Changes the ggplot2 theme for all plots, including those in the Plots tab.",
          theme = "light-border"
        )
      ),
      column(3, style = "margin-top: 25px;", mod_download_images_ui("download_barplot"))
    ) # 3rd row
  )
}