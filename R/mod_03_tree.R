# R/mod_tree.R
mod_tree_ui <- function() {
  tabPanel("Tree",
    value = 4,
    h5("A hierarchical clustering tree summarizes the correlation among significant pathways
                listed in the Enrichment tab. Pathways with many shared genes are clustered together.
                  Bigger dots indicate more significant P-values. The width of the plot can be
                  changed by adjusting the width of your browser window."),
    fluidRow(
      column(width = 3, selectInput(
        inputId = "treeChartAspectRatio",
        label = h5("Aspect Ratio"),
        choices = .1 * (5:40),
        selected = 2
      )),
      column(3, style = "margin-top: 25px;", mod_download_images_ui("download_tree", label = "Download"))
    ),
    plotOutput("GOTermsTree")
  )
}