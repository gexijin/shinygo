# Enrichment tab module
#
# The main results: the welcome page before Submit, then the table of
# significant pathways, the sort menu, a download of the top or all pathways,
# and notes on the methods and on reading the results.
#
#   mod_enrichment_ui(id)      the tab
#   mod_enrichment_server(id, goButton, input_text_b, show_pathway_id,
#                         selectOrg, selectGO, genesColumnID,
#                         significantOverlaps, significantOverlapsAll)
#
# Arguments of mod_enrichment_server(), all reactives from the main server:
# the inputs of the same names (input_text_b is the background genes, read so
# the table updates when they change), plus significantOverlaps (the pathways
# shown) and significantOverlapsAll (all pathways, for the "all" download).
#
# Three things deliberately stay with the main app, without namespacing:
#   - the "Sort by" menu (SortPathways), which significantOverlaps() reads, so
#     the sort order also changes the Chart, Tree and Network tabs;
#   - the download pop-up's "Genes column" choice (genesColumnID), which
#     significantOverlapsAll() reads;
#   - the welcome-page conditionalPanel()s, which check input.goButton.
# ----------------------------------------------------------------------------


mod_enrichment_ui <- function(id) {
  ns <- NS(id)
    tabPanel("Enrichment",
      value = 1,
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
        p("8/28/26: v.0.86.0 Ensembl Release 115 (925 species), STRING-db 12.0 (12,535 species), NCBI (v.6/15/2026, 2342 species), and 39 custom genomes. "),
        p("You can still use the old versions using links on the About tab.", 
          "To support this effort, please cite our paper like ",
            a("these 4000+ papers.", href = "https://scholar.google.com/scholar?oi=bibs&hl=en&cites=4205886424733220184&as_sdt=5", target = "_blank"),
            "Just including URL is not enough.",
          a("Email Jenny ", href = "mailto:gelabinfo@gmail.com?Subject=ShinyGO", target = "_blank"),
          "(gelabinfo@gmail.com) for questions, suggestions or data contributions.",
          "Follow Dr Ge on ", a("Twitter", href = "https://twitter.com/StevenXGe", target = "_blank"), " and ",
          a("LinkedIn", href = "https://www.linkedin.com/in/steven-ge-ab016947/", target = "_blank"),
          " for updates. ",
          "To request to add a new species/genome, fill in this ",
          a("Form.", href = "https://forms.gle/zLtLnqxkW187AgT76", target = "_blank"), 
          "We will try to accommodate commonly requested genomes. "
        ),            
        p("Try our mirror site at ",
          a("https://orditus.ai/go/", href = "https://orditus.ai/go/", target = "_blank"),
          "if this site is slow or unavailable."),
        p("For-profit organizations: contact us for local installation or customization services."),
        p("Under active delovelopment with support from NIH. Report bugs or request features on our ", 
            a("GitHub repository.", href = "https://github.com/gexijin/shinygo", target = "_blank")),
        p("NO WARRANTY. Please verify results using other tools.
        Enrichment results may vary depending on gene ID mapping, data sources, 
        database versions, and methods (particularily ranking).
          "),
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
        br(), br(), img(src = "downSyndrome.png", align = "center", width = "371", height = "276"),
      ),
      br(),
      conditionalPanel(
        "input.goButton != 0",
        # The sort input used to carry style= twice ("display:inline-block"
        # and a typo'd "algn:right"); htmltools joins duplicate attributes,
        # so the display rule was invalid and the div fell back to block.
        div(
          style = "display:inline-block",
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
          )
        )
      ),
      tableOutput(ns("EnrichmentTable")),
      conditionalPanel(
        "input.goButton != 0",
        # These open a dialog to pick what the Genes column should hold,
        # then offer the real download. The Genes column is not shown in
        # the table above, so the choice only makes sense at download time.
        actionButton(ns("askDownloadEnrichment"), "Top Pathways shown above", icon = icon("download")),
        actionButton(ns("askDownloadEnrichmentAll"), "Results on all Pathways", icon = icon("download")),
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
              your initial expectations."),
      )
    )
}


mod_enrichment_server <- function(id, goButton, input_text_b, show_pathway_id,
                                  selectOrg, selectGO, genesColumnID,
                                  significantOverlaps, significantOverlapsAll) {
  moduleServer(id, function(input, output, session) {
    output$EnrichmentTable <- renderTable(
      {
        if (goButton() == 0) {
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

        withProgress(message = sample(.app$quotes, 1), detail = myMessage, {
          pathways <- significantOverlaps()$x
          # remove pathway ID  only in Ensembl species
          if (!show_pathway_id() && as.integer(selectOrg()) > 0) {
            pathways$Pathway <- remove_pathway_id(pathways$Pathway, selectGO())
          }
          pathways$Pathway <- hyperText(pathways$Pathway, pathways$URL)

          pathways$Membership <- NULL # internal, never displayed
          pathways <- pathways[, -7]
          # rownames(pathways) <- NULL
          # pathways[, 1] <- as.numeric( format(pathways[, 1], scientific = TRUE, digits = 3 ) )
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

    # Ask what the Genes column should contain, then hand over the real download
    # button. The Genes column is not part of the on-screen table, so this is the
    # only point where the choice is visible to the user.
    genes_column_dialog <- function(download_id, title) {
      shiny::modalDialog(
        title = title,
        radioButtons(
          inputId = "genesColumnID",
          label = "How should genes be listed in the Genes column?",
          choiceNames = c(
            "Gene symbols where available, otherwise the ID you pasted",
            "The gene IDs you pasted",
            "Ensembl gene IDs"
          ),
          choiceValues = c("symbol", "input", "ensembl"),
          selected = if (is.null(genesColumnID())) "symbol" else genesColumnID()
        ),
        helpText(
          "Some species have gene symbols for only part of the genome. For those,
         the first option gives a mix of symbols and IDs. Choose one of the other
         two if you need a single consistent ID type."
        ),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton(session$ns(download_id), "Download")
        ),
        easyClose = TRUE
      )
    }

    observeEvent(input$askDownloadEnrichment, {
      showModal(genes_column_dialog("downloadEnrichment", "Download top pathways"))
    })

    observeEvent(input$askDownloadEnrichmentAll, {
      showModal(genes_column_dialog("downloadEnrichmentAll", "Download all pathways"))
    })

    # Membership is a list column carried for the tree, network and redundancy
    # filter. It is internal, and write.csv cannot serialise it.
    drop_membership <- function(df) {
      if (is.data.frame(df) && "Membership" %in% colnames(df)) df$Membership <- NULL
      df
    }

    output$downloadEnrichment <- downloadHandler(
      filename = function() {
        "enrichment.csv"
      },
      content = function(file) {
        write.csv(drop_membership(significantOverlaps()$x), file, row.names = FALSE)
        removeModal()
      }
    )

    output$downloadEnrichmentAll <- downloadHandler(
      filename = function() {
        "enrichment_all.csv"
      },
      content = function(file) {
        write.csv(drop_membership(significantOverlapsAll()$x), file, row.names = FALSE)
        removeModal()
      }
    )
  })
}
