# R/mod_plots.R
mod_plots_ui <- function() {
  tabPanel("Plots",
    value = 8,
    h5("The characteristics of your genes are compared with the rest in the genome. Chi-squared and Student's
        t-tests are run to see if your genes have special characteristics when compared with all the other genes or, if uploaded, a customized background."),
    fluidRow(
      column(
        width = 4,
        mod_download_images_ui("download_gene_plot_dist", "Download density plots")
      ),
      column(
        width = 4,
        mod_download_images_ui("download_gene_barplot", "Download barplots")
      )
    ),
    br(),
    plotOutput("genePlot2", inline = TRUE, width = "auto", height = "auto"),
    plotOutput("gene_barplot", inline = TRUE, width = "auto", height = "auto")
  )
}