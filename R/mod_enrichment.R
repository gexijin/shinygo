#' Enrichment UI Function
#'
#' @description A Shiny module for displaying enrichment analysis results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @export
#'
mod_enrichment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    conditionalPanel(
      "input.goButton != 0",
      div(
        style = "display:inline-block",
        selectInput(
          inputId = ns("SortPathways"),
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
            your initial expectations."),
    )
  )
}

#' Enrichment Server Functions
#'
#' @param id Module ID
#' @param significantOverlaps Reactive expression returning filtered enrichment results
#' @param significantOverlapsAll Reactive expression returning all enrichment results
#' @param input_goButton Reactive expression for the goButton input
#' @param input_input_text_b Reactive expression for the background gene input
#' @param input_show_pathway_id Reactive expression for the show_pathway_id input
#' @param input_selectOrg Reactive expression for the selected organism
#' @param input_selectGO Reactive expression for the selected GO category
#'
mod_enrichment_server <- function(id, 
                                  significantOverlaps, 
                                  significantOverlapsAll,
                                  input_goButton,
                                  input_input_text_b,
                                  input_show_pathway_id,
                                  input_selectOrg,
                                  input_selectGO) {
  moduleServer(id, function(input, output, session) {
    
    # Render the enrichment table
    output$EnrichmentTable <- renderTable(
      {
        if (input_goButton() == 0) {
          return(NULL)
        }
        
        myMessage <- "Analyzing genes."
        
        if (is.null(significantOverlaps())) {
          return(NULL)
        }
        
        # This solves an error when there is no significant enrichment
        if (ncol(significantOverlaps()$x) == 1) {
          return(significantOverlaps()$x)
        }
        
        withProgress(message = sample(quotes, 1), detail = myMessage, {
          pathways <- significantOverlaps()$x
          
          # Remove pathway ID only in Ensembl species
          if (!input_show_pathway_id() && as.integer(input_selectOrg()) > 0) {
            pathways$Pathway <- remove_pathway_id(pathways$Pathway, input_selectGO())
          }
          
          pathways$Pathway <- hyperText(pathways$Pathway, pathways$URL)
          
          pathways <- pathways[, -7]
          
          pathways[, 4] <- as.character(round(pathways[, 4], 1))
          pathways[, 2] <- as.character(pathways[, 2]) 
          pathways[, 3] <- as.character(pathways[, 3]) 
          colnames(pathways)[5] <- "Pathways (click for details)"
          
          incProgress(1, detail = paste("Done"))
        })
        
        if (dim(pathways)[2] > 1) pathways[, 2] <- as.character(pathways[, 2])
        
        if (dim(pathways)[2] == 1) {
          return(pathways)
        } else {
          return(pathways[, 1:5])
        } 
      },
      digits = -1,
      spacing = "s",
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = TRUE,
      sanitize.text.function = function(x) x
    )
    
    # Download handlers for the enrichment results
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
    
    # Return the selected sorting method to the main app
    return(reactive(input$SortPathways))
  })
}