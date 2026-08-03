#' 01_enrichment UI Function
#'
#' @description Renders the Enrichment tab: the welcome screen shown before
#'   a query is submitted, the pathway sort selector, the enrichment results
#'   table, download buttons for the shown/all pathways, and the static
#'   Methods and Interpreting Results text.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_enrichment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "input.goButton == 0 ", # welcome screen
      br(),
      fluidRow(
        column(
          width = 9,
          h4("ShinyGO: a graphical gene-set enrichment tool for animals and plants")
        ),
        column(
          width = 3,
          img(
            src = "shinygo_logo.png",
            width = "43",
            height = "50"
          )
        )
      ),
      p("9/5/25: v.0.85. Database updated to Ensembl Release 113 and STRING-db v12. "),
      p(
        "You can still use the old versions using links on the About tab.",
        "To support this effort, please cite our paper like ",
        a("these 4000+ papers.", href = "https://scholar.google.com/scholar?oi=bibs&hl=en&cites=4205886424733220184&as_sdt=5"),
        "Just including URL is not enough.",
        a("Email Jenny ", href = "mailto:gelabinfo@gmail.com?Subject=ShinyGO"),
        "(gelabinfo@gmail.com) for questions, suggestions or data contributions.",
        "Follow Dr Ge on ", a("Twitter", href = "https://twitter.com/StevenXGe"), " and ",
        a("LinkedIn", href = "https://www.linkedin.com/in/steven-ge-ab016947/", target = "_blank"),
        " for updates. ",
        "To request to add a new species/genome, fill in this ",
        a("Form.", href = "https://forms.gle/zLtLnqxkW187AgT76"),
        "We will try to accommodate commonly requested genomes. "
      ),
      p("For-profit organizations: contact us for local installation or customization services."),
      p(
        "Under active delovelopment with support from NIH. Report bugs or request features on our ",
        a("GitHub repository.", href = "https://github.com/gexijin/shinygo", target = "_blank")
      ),
      p("NO WARRANTY. Please verify results using other tools.
        Enrichment results may vary depending on gene ID mapping, data sources,
        database versions, and methods (particularily ranking)."),
      h3("GO Enrichment analysis, plus a lot more!"),
      p("Just paste your gene list to get enriched GO terms and othe pathways for over 14,000 species,
        based on annotation from Ensembl and STRING-db."),
      br(), img(src = "enrich.png", align = "center", width = "660", height = "339"),
      br(), img(src = "enrichmentChart.png", align = "center", width = "700", height = "400"),
      br(), br(), img(src = "KEGG2.png", align = "center", width = "541", height = "360"),
      br(), br(), img(src = "GOtree3.png", align = "center", width = "500", height = "258"),
      br(), br(), img(src = "GOnetwork2.png", align = "center", width = "500", height = "248"),
      br(), br(), img(src = "PPInetwork2.png", align = "center", width = "500", height = "391"),
      br(), br(), img(src = "chr.png", align = "center", width = "444", height = "338"),
      br(), br(), img(src = "downSyndrome.png", align = "center", width = "371", height = "276")
      # br(), br(), img(src = "promoter.png", align = "center", width = "717", height = "288")
    ),
    br(),
    conditionalPanel(
      "input.goButton != 0",
      div(
        style = "display:inline-block",
        # ====================================================================
        # DO NOT wrap this inputId in ns()! Its value is read as the bare
        # input$SortPathways inside the significantOverlaps reactive in
        # server.R, which feeds every other tab's module (Chart, Tree,
        # Network, KEGG, Groups). Namespacing it here would silently stop
        # that shared reactive from picking up changes. Same pattern as
        # ggplot2_theme in mod_02_chart.r.
        # ====================================================================
        selectInput(
          inputId = "SortPathways",
          label = NULL,
          choices = c(
            "Sort by FDR" = "Sort by FDR",
            "Sort by Fold Enrichment" = "Sort by Fold Enrichment",
            "Sort by average ranks(FDR & Fold)" = "Sort by FDR & Fold Enrichment",
            "Select by FDR, sort by Fold Enrichment" = "Select by FDR, sort by Fold Enrichment",
            "Sort by Genes" = "Sort by Genes",
            "Sort by Category Name" = "Sort by Category Name"
          ),
          selected = "Select by FDR, sort by Fold Enrichment"
        ),
        style = "algn:right"
      )
    ),
    tableOutput(ns("EnrichmentTable")),
    conditionalPanel(
      "input.goButton != 0",
      downloadButton(ns("downloadEnrichment"), "Top Pathways shown above"),
      downloadButton(ns("downloadEnrichmentAll"), "Results on all Pathways"),
      br(), br(),
      h3("Methods"),
      p("All query genes are converted to ENSEMBL gene IDs or STRING-db protein IDs,
        as our gene ID mapping and pathway data primarily come from these sources.
        For model organisms, we manually compile extensive pathway lists from various
        public databases."),
      p("P-values are calculated using the hypergeometric test, and false discovery
        rates (FDRs) are computed via the Benjamini-Hochberg method to correct for
        multiple testing. Fold enrichment is defined as the percentage of genes in
        your list that are in a pathway divided by the corresponding percentage in
        the background genes. While FDR measures statistical significance, fold
        enrichment indicates effect size."),
      p("We recommend that users provide their own list of background genes, which
        could include all genes detected in an experiment, such as genes with probes
        on a DNA microarray, passed a minimal filter in RNA-seq analysis, or detected
        in a proteomics experiment. If no background genes are uploaded, the default
        is to use all protein-coding genes. Alternatively, you may select the option
        'Use pathway DB for gene counts,' which calculates the background based on
        the total number of unique genes in the chosen pathway database, limited
        between 5,000 and 30,000 genes. When this option is selected, any genes in
        the user's original list that are not in the pathway database are excluded."),
      p("Only pathways within specified size limits, as defined by the
        'Pathway size: (Min, Max)' settings, are considered. Results for smaller
        pathways can be noisy, but some pathways or GO terms have only a few genes.
        After analysis, pathways are filtered by a user-defined FDR cutoff. Significant
        pathways are then sorted in different ways, and only the top-ranked are shown
        in the table above.

        By default, 'Select by FDR, then by Fold Enrichment' is used, where pathways
        are first filtered and sorted by FDR, and then the top 20 are sorted by fold
        enrichment. In other words, the default setting shows the top 20 most
        significant pathways ranked by fold enrichment. When the 'Sort by average
        ranks (FDR & fold enrichment)' option is selected, pathways are sorted by
        the average of their ranks based on both FDR and fold enrichment. When
        'Sort by FDR' is selected, pathways are ranked by FDR and only the top 20 are shown.

        The 'Remove redundancy' option eliminates similar pathways that share 95% of
        their genes and 50% of the words in their names, representing them with the
        pathway that has the highest significance."),
      h3("Interpreting GO Enrichment Results"),
      p("The Gene Ontology (GO) includes tens of thousands of terms (functional
        categories), each tested individually for enrichment. Hundreds or even
        thousands of GO terms can be statistically significant. These terms are
        filtered, ranked, and only the top ones are displayed. Understanding this
        process is crucial for interpreting GO enrichment results."),
      tags$ul(
        tags$li("P-value: Reflects the statistical significance of the enrichment.
                Lower values suggest a lower likelihood of the result occurring by
                chance under the null hypothesis. FDR q-values adjust P-values for
                multiple testing to control the proportion of type I errors."),
        tags$li("Fold Enrichment: Measures the magnitude of enrichment. Higher values
                indicate stronger enrichment and are an important metric of effect size."),
        tags$li("Pathway Genes: The total number of genes in a pathway or GO term."),
        tags$li("nGenes: The number of genes in the pathway that overlap with your gene list.")
      ),
      tags$p("Exercise caution when interpreting FDR values of 0.01 or 0.001 for GO
              terms, as these levels often represent noise due to the vast number of
              terms tested. For a gene list of reasonable size, more significant results
              (FDR < 1E-5) are expected."),
      tags$p("Large pathways, such as the cell cycle, often show smaller FDRs due to
              increased statistical power, while smaller pathways might have higher
              FDRs despite their biological relevance. Enrichment analysis tends to
              favor larger pathways."),
      tags$p("With a default cutoff of FDR < 0.05, thousands of significant GO terms
              may be detected, though only a subset is shown. Therefore, the method of
              filtering and ranking these terms is crucial."),
      tags$p("With large sample sizes, small differences can appear extremely
              significant. In addition to FDR, fold enrichment should also be
              considered when prioritizing pathways, as it reflects the strength
              of the enrichment. We offer several methods that consider both FDR
              q-values and fold enrichment."),
      tags$p("Many GO terms are closely related (e.g., 'Cell Cycle', 'Regulation of
              Cell Cycle') and can dominate the top 20, obscuring other pathways. To
              avoid this, consider examining the top 50 terms. Additionally, use tree
              plots and network plots to identify clusters of related GO terms and
              uncover overarching themes."),
      tags$p("Discuss the most significant pathways first, even if they do not fit
              your initial expectations.")
    )
  )
}


#' 01_enrichment Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param significantOverlaps A reactive (defined in the main server) that
#'   returns the filtered/sorted/ranked enrichment result list; its `$x`
#'   element is the table shown here and downloaded via "downloadEnrichment".
#' @param significantOverlapsAll A reactive (defined in the main server) that
#'   returns the unfiltered enrichment result list, before the FDR/sort/top-N
#'   filtering applied for `significantOverlaps`; downloaded via
#'   "downloadEnrichmentAll".
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#' @param select_org A reactive returning the value of the selected organism
#'   input; used to decide whether pathway IDs can be stripped from pathway
#'   names (Ensembl species only).
#' @param select_go A reactive returning the value of the selected pathway
#'   database input; used when stripping pathway IDs from pathway names.
#' @param show_pathway_id A reactive returning the sidebar's "Show pathway
#'   IDs" checkbox value; used to decide whether to strip pathway IDs from
#'   pathway names.
#' @param input_text_b A reactive returning the background gene list text
#'   area's value; read only as a recalculation trigger, since the actual
#'   background filtering happens upstream in `significantOverlaps()`.
#' @param quotes A character vector of quotes shown while the table loads.
#'
#' @noRd
mod_01_enrichment_server <- function(id,
                                      significantOverlaps,
                                      significantOverlapsAll,
                                      go_button,
                                      select_org,
                                      select_go,
                                      show_pathway_id,
                                      input_text_b,
                                      quotes) {
  moduleServer(id, function(input, output, session) {
    output$EnrichmentTable <- renderTable(
      {
        if (go_button() == 0) {
          return(NULL)
        }
        tem <- input_text_b() # just to make it re-calculate if user changes background

        myMessage <- "Analyzing genes."

        if (is.null(significantOverlaps())) {
          return(NULL)
        }
        # this solves an error when there is no significant enrichment
        if (ncol(significantOverlaps()$x) == 1) {
          return(significantOverlaps()$x)
        }

        # Reshape the raw enrichment table into what's actually displayed:
        # optionally strip pathway IDs, hyperlink pathway names, and coerce
        # a few numeric columns to character so they print without rounding
        # artifacts.
        withProgress(message = sample(quotes, 1), detail = myMessage, {
          pathways <- significantOverlaps()$x
          # remove pathway ID only in Ensembl species
          if (!show_pathway_id() && as.integer(select_org()) > 0) {
            pathways$Pathway <- remove_pathway_id(pathways$Pathway, select_go())
          }
          pathways$Pathway <- hyperText(pathways$Pathway, pathways$URL)

          pathways <- pathways[, -7]
          pathways[, 4] <- as.character(round(pathways[, 4], 1))
          pathways[, 2] <- as.character(pathways[, 2]) # convert total genes into character 10/21/19
          pathways[, 3] <- as.character(pathways[, 3]) # convert total genes into character 10/21/19
          colnames(pathways)[5] <- "Pathways (click for details)"

          incProgress(1, detail = paste("Done"))
        })

        if (dim(pathways)[2] > 1) pathways[, 2] <- as.character(pathways[, 2])

        if (dim(pathways)[2] == 1) {
          return(pathways)
        } else {
          return(pathways[, 1:5])
        } # If no significant enrichment found x only has 1 column.
      },
      digits = -1,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = TRUE,
      sanitize.text.function = function(x) x
    )

    output$downloadEnrichment <- downloadHandler(
      filename = function() {
        "enrichment.csv"
      },
      content = function(file) {
        write.csv(significantOverlaps()$x, file, row.names = FALSE)
      }
    )

    output$downloadEnrichmentAll <- downloadHandler(
      filename = function() {
        "enrichment_all.csv"
      },
      content = function(file) {
        write.csv(significantOverlapsAll()$x, file, row.names = FALSE)
      }
    )
  })
}
