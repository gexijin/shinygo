# R/mod_genome.R
mod_genome_ui <- function() {
  tabPanel("Genome",
    value = 9,
    plotlyOutput("genomePlotly", height = "900px"),
    fluidRow(
      column(3, checkboxInput("labelGeneSymbol", "Label genes", value = FALSE)),
      column(3, checkboxInput("ignoreNonCoding", "Coding genes only", value = TRUE)),
      column(3, checkboxInput("show_all_chr", "All chr.", value = FALSE)),
      column(3, actionButton("gPlotstatic", "Static plot"))
    ),
    fluidRow(
      column(3, selectInput(
        inputId = "MAwindowSize",
        label = h5("Window Size(Mb)"),
        selected = 6,
        choices = c(1, 2, 4, 6, 8, 10, 15, 20)
      )),
      column(3, selectInput(
        inputId = "MAwindowSteps",
        label = h5("Steps in a window"),
        choices = unique(1:4),
        selected = c(2)
      )),
      column(3, selectInput(
        inputId = "chRegionPval",
        label = h5("FDR cutoff for windows"),
        selected = 0.00001,
        choices = c(0.1, 0.05, 0.01, 0.001, 0.0001, 0.00001, 0.000001)
      ))
    ),
    h5("The genes are represented by red dots. The purple lines indicate regions where
                  these genes are statistically enriched, compared to the density of genes in the background.
                  We scanned the genome with a sliding window. Each window is further divided into several
                  equal-sized steps for sliding. Within each window we used the hypergeometric test to
                  determine if your genes are significantly overrepresented. Essentially, the genes in
                  each window define a gene set/pathway, and we carried out enrichment analysis. The
                  chromosomes may be only partly shown as we use the last gene's location to draw the line.
                  Mouse over to see gene symbols. Zoom in regions of interest."),

    bsModal("genomePlotStaticButton", "Static genome plot", "gPlotstatic",
      size = "large",
      h5("Your genes are marked in each of the chromosomes.
              Note that the scale for each chromosomes are different."),
      plotOutput("genomePlot", width = "100%")
    )
  )
}