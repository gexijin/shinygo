#' 10_string UI Function
#'
#' @description Renders the STRING tab: reports the percentage of the user's
#'   genes successfully mapped to STRING-db protein IDs, lets the user pick a
#'   functional enrichment category (GO, KEGG, Pfam, InterPro) and view or
#'   download the resulting STRING-db enrichment table, and opens a modal
#'   with a protein-protein interaction (PPI) network plot and a link to the
#'   interactive network on the STRING website.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_10_string_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("STRINGDB_mapping_stat")),
    tags$head(tags$style(paste0("#", ns("STRINGDB_mapping_stat"), "{color: blue;font-size: 15px;}"))),
    br(),
    actionButton(ns("ModalPPI"), "PPI network of DEGs"), br(), br(),
    selectInput(ns("STRINGdbGO"),
      label = "Functional Enrichment",
      choices = list(
        "GO Biological Process" = "Process",
        "GO Cellular Component" = "Component",
        "GO Molecular Function" = "Function",
        "KEGG" = "KEGG",
        "Pfam" = "Pfam",
        "InterPro" = "InterPro"
      ),
      selected = "Process"
    ),
    downloadButton(ns("STRING_enrichmentDownload")),
    tableOutput(ns("stringDB_GO_enrichment")),
    br(), br(),
    h5(
      "To validate your results independent of our algorithm and database,
      your genes are sent to STRING-db website for enrichment analysis.
      This also enables the
      retrieval of a protein-protein network. If it is running,
      please wait until it finishes. The second time it is faster."
    ),
    bsModal(ns("ModalExamplePPI"), "Protein-protein interaction networks ", ns("ModalPPI"),
      size = "large",
      h5("By sending your genes to the STRING website,
        shinyGO is retrieving a sub-network, calculating PPI enrichment,
        and generating custom URLs to the STRING website containing your genes. This can take 5 minutes. Patience will pay off! "),
      sliderInput(ns("nGenesPPI"), label = h5("Genes to include:"), min = 0, max = 400, value = 20, step = 10),
      # ,htmlOutput(ns("stringDB_network_link"))
      # ,tags$head(tags$style(paste0("#", ns("stringDB_network_link"), "{color: blue; font-size: 15px;}")))
      plotOutput(ns("stringDB_network1"))
    ) # bsModal 1
  )
}


#' 10_string Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param conversionTableData A reactive (defined in the main server) that
#'   returns the merged conversion/annotation table for the user's genes;
#'   mapped onto STRING-db protein IDs here.
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#' @param select_org A reactive returning the value of the selected organism
#'   input; used to look up the species' NCBI taxonomy ID for STRING-db.
#' @param min_fdr A reactive returning the value of the main FDR cutoff
#'   input; used to filter the STRING-db enrichment results.
#' @param quotes A character vector of quotes shown while STRING-db requests
#'   are in progress.
#'
#' @noRd
mod_10_string_server <- function(id, conversionTableData, go_button, select_org, min_fdr, quotes) {
  moduleServer(id, function(input, output, session) {

    # Look up the NCBI taxonomy ID STRING-db expects, from the species
    # selected in the main sidebar.
    findTaxonomyID <- reactive({
      if (go_button() == 0) {
        return(NULL)
      }
      find_taxon_by_id(select_org(), orgInfo)
    })

    # Map the user's genes onto STRING-db protein IDs and split them into
    # "up"/"down" sets. ShinyGO doesn't have real expression data here, so
    # every gene is sent to STRING-db as "up" (lfc = 1); "down" is always
    # empty but kept so the return shape matches STRING-db's expectations.
    STRINGdb_geneList <- reactive({
      if (go_button() == 0) {
        return(NULL)
      }
      library(STRINGdb, verbose = FALSE)
      tem <- select_org() # re-run mapping if the user changes species

      if (is.null(conversionTableData())) {
        return(NULL)
      } # this has to be outside of isolate() !!!
      # if(input$selectOrg == "NEW" && is.null( input$gmtFile) ) return(NULL) # new but without gmtFile
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }

      isolate({
        withProgress(message = sample(quotes, 1), detail = "Mapping gene ids (5 minutes)", {
          string_db <- STRINGdb$new(
            version = STRING_DB_VERSION, species = taxonomyID,
            score_threshold = 0, input_directory = ""
          )

          genes <- conversionTableData()
          # STRINGdb species the columns are ensemble_gene_id
          ix <- which(colnames(genes) == "Ensembl Gene ID" | colnames(genes) == "STRINGdb ID")
          colnames(genes)[ix] <- c("gene")
          genes$lfc <- 1
          # remove space character in front of gene symbols. Otherwise STRING won't convert
          genes$gene <- gsub(" ", "", genes$gene)
          mapped <- string_db$map(genes, "gene", removeUnmappedRows = TRUE)

          incProgress(1 / 4, detail = paste("up regulated"))
          up <- subset(mapped, lfc > 0, select = "STRING_id", drop = TRUE)

          incProgress(1 / 2, detail = "Down regulated")
          down <- subset(mapped, lfc < 0, select = "STRING_id", drop = TRUE)

          mappingRatio <- nrow(mapped) / nrow(genes)
          if (nrow(mapped) == 0) {
            return(NULL)
          } else {
            return(list(up = up, down = down, ratio = mappingRatio, geneTable = mapped))
          }
          incProgress(1)
        }) # progress
      }) # isolate
    })

    # Percentage of genes STRING-db was able to map, shown above the
    # enrichment controls.
    output$STRINGDB_mapping_stat <- renderText({
      if (go_button() == 0) {
        return(NULL)
      }

      if (is.null(STRINGdb_geneList())) {
        return("No genes mapped by STRINGdb. Please enter or double-check species name above.")
      }
      if (!is.null(STRINGdb_geneList())) {
        tem <- paste0(100 * round(STRINGdb_geneList()$ratio, 3), "% genes mapped by STRING web server.")
        if (STRINGdb_geneList()$ratio < 0.3) {
          tem <- paste(tem, "Warning!!! Very few gene mapped. Double check if the correct species is selected.")
        }
        return(tem)
      }
    })

    # Functional enrichment (GO/KEGG/Pfam/InterPro, per input$STRINGdbGO) on
    # the mapped genes, via STRING-db's own enrichment API. Returns -1
    # before the main analysis has run, and -2 when there are too few genes
    # or no significant results, so the render/download outputs below can
    # tell those cases apart from an actual results data frame.
    stringDB_GO_enrichmentData <- reactive({
      if (go_button() == 0) {
        return(-1)
      }
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }
      library(STRINGdb, verbose = FALSE)
      withProgress(message = sample(quotes, 1), detail = "Enrichment analysis", {
        tem <- input$STRINGdbGO
        string_db <- STRINGdb$new(
          version = STRING_DB_VERSION, species = taxonomyID,
          score_threshold = 0, input_directory = ""
        )

        genes <- conversionTableData()
        minGenesEnrichment <- 1
        if (is.null(genes)) {
          return(-2)
        } else if (dim(genes)[1] <= minGenesEnrichment) {
          return(-2) # if has only few genes
        } else {
          ids <- STRINGdb_geneList()$up
          if (length(ids) <= minGenesEnrichment || is.null(ids)) {
            return(-2)
          }
          incProgress(1 / 3)
          result <- string_db$get_enrichment(ids, category = input$STRINGdbGO, methodMT = "fdr", iea = TRUE)
          if (nrow(result) == 0 || is.null(result)) {
            return(-2)
          } else {
            if (min(result$fdr) > min_fdr()) {
              return(-2)
            } else {
              result <- result[which(result$fdr < min_fdr()), ]
              incProgress(1, detail = paste("Done"))
              return(result)
            } # end of check minFDR
          } # check results
        } # end of check genes if
      }) # progress
    }) # end of stringDB_GO_enrichmentData

    output$stringDB_GO_enrichment <- renderTable(
      {
        result <- stringDB_GO_enrichmentData()

        req(!is.null(result))
        if (class(result) == "numeric") {
          if (result == -1) {
            return(NULL)
          } else if (result == -2) {
            return(as.data.frame("No significant enrichment found."))
          }
        } else {
          result <- dplyr::select(
            result,
            c(
              "fdr", "number_of_genes", "term",
              "description"
            )
          )
          colnames(result) <- c(
            "FDR", "nGenes", "GO terms or pathways",
            "Description"
          )
          result$FDR <- as.character(result$FDR)
          if (nrow(result) > 30) {
            result <- result[1:30, ]
          }
          return(result)
        } # end of if else
      },
      digits = 4,
      spacing = "s",
      include.rownames = F,
      striped = TRUE,
      bordered = TRUE,
      width = "auto",
      hover = T
    ) # renderTable

    output$STRING_enrichmentDownload <- downloadHandler(
      filename = function() {
        paste0("STRING_enrichment", input$STRINGdbGO, ".csv")
      },
      content = function(file) {
        write.csv(stringDB_GO_enrichmentData(), file)
      }
    ) # downloadHandler

    # Static PPI network plot shown in the "PPI network of DEGs" modal,
    # limited to the top `nGenesPPI` mapped genes.
    output$stringDB_network1 <- renderPlot(
      {
        library(STRINGdb)
        if (go_button() == 0) {
          return(NULL)
        }

        tem <- input$STRINGdbGO
        tem <- input$nGenesPPI
        taxonomyID <- findTaxonomyID()
        if (is.null(taxonomyID)) {
          return(NULL)
        }

        if (is.null(STRINGdb_geneList())) {
          return(NULL)
        }

        isolate({
          withProgress(message = sample(quotes, 1), detail = "Enrichment analysis", {
            string_db <- STRINGdb$new(
              version = STRING_DB_VERSION, species = taxonomyID,
              score_threshold = 0, input_directory = ""
            )
            # only up regulated is ploted
            ngenes1 <- input$nGenesPPI
            if (ngenes1 < 2) ngenes1 <- 2
            for (i in c(1:1)) {
              incProgress(1 / 2, detail = paste("Plotting network"))

              ids <- STRINGdb_geneList()[[i]]
              if (length(ids) > ngenes1) { # n of genes cannot be more than 400
                ids <- ids[1:ngenes1]
              }
              incProgress(1 / 3)
              string_db$plot_network(ids, add_link = FALSE)
            }
          }) # progress
        }) # isolate
      },
      width = 1000,
      height = 600
    )

    # Link to the same network, hosted interactively on the STRING website.
    # Currently unused: the UI element that would display it
    # (htmlOutput("stringDB_network_link")) is commented out above.
    output$stringDB_network_link <- renderUI({
      library(STRINGdb, verbose = FALSE)

      tem <- input$STRINGdbGO
      tem <- input$nGenesPPI
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }

      if (is.null(STRINGdb_geneList())) {
        return(NULL)
      }

      isolate({
        withProgress(message = sample(quotes, 1), detail = "PPI Enrichment and link", {
          string_db <- STRINGdb$new(
            version = STRING_DB_VERSION, species = taxonomyID,
            score_threshold = 0, input_directory = ""
          )
          ids <- STRINGdb_geneList()[[1]]

          ngenes1 <- input$nGenesPPI
          if (ngenes1 < 2) ngenes1 <- 2

          if (length(ids) > ngenes1) { # n of genes cannot be more than 400
            ids <- ids[1:ngenes1]
          }
          incProgress(1 / 4)
          link1 <- string_db$get_link(ids)

          tem <- paste("<a href=\"", link1, "\" target=\"_blank\"> Click here for an interactive and annotated network </a>")
          # 	Pval1 = string_db$get_ppi_enrichment( ids)
          #    tem2 = paste("<h5> PPI enrichment P value: ")
          # 	tem2 = paste0(tem2, sprintf("%-3.2e",Pval1[1]))
          # 	tem2 = paste(tem2, ".</h5>  <h5> Small P value indicates more PPIs among your proteins than background. </h5>" )
          # 	tem = paste(tem2,tem )
          return(HTML(tem))

          incProgress(1)
        }) # progress
      }) # isolate
    })
  })
}
