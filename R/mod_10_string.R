# STRING tab module
#
# Sends the user's genes to the STRING-db website: maps them to STRING
# proteins, runs STRING's own enrichment analysis (to check our results with
# an independent tool), and draws a protein-protein interaction network in a
# pop-up. Needs internet access to string-db.org.
#
#   mod_string_ui(id)      the tab, including the network pop-up
#   mod_string_server(id, goButton, selectOrg, minFDR, conversionTableData)
#                          everything behind it
#
# Arguments of mod_string_server(), all reactives from the main server:
#   goButton             input$goButton (the Submit button)
#   selectOrg            input$selectOrg (the species ID)
#   minFDR               input$minFDR (the sidebar's FDR cutoff)
#   conversionTableData  the user's genes, as shown on the Genes tab
# ----------------------------------------------------------------------------


mod_string_ui <- function(id) {
  ns <- NS(id)
  tabPanel("STRING",
    value = 11,

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
    br(),br(),
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
      # ,htmlOutput("stringDB_network_link")
      # ,tags$head(tags$style("#stringDB_network_link{color: blue; font-size: 15px;}"))

      plotOutput(ns("stringDB_network1"))
    )
  )
}


mod_string_server <- function(id, goButton, selectOrg, minFDR, conversionTableData) {
  moduleServer(id, function(input, output, session) {
    # find Taxonomy ID from species official name
    findTaxonomyID <- reactive({
      if (goButton() == 0) {
        return(NULL)
      }
    
      find_taxon_by_id(selectOrg(), .app$orgInfo)

    })


    STRINGdb_geneList <- reactive({
      if (goButton() == 0) {
        return(NULL)
      }
      tem <- selectOrg()

      ####################################

      if (is.null(conversionTableData())) {
        return(NULL)
      } # this has to be outside of isolate() !!!
      # if(selectOrg() == "NEW" && is.null( input$gmtFile) ) return(NULL) # new but without gmtFile
      NoSig <- as.data.frame("No significant enrichment found.")
      taxonomyID <- findTaxonomyID()

      if (is.null(taxonomyID)) {
        return(NULL)
      }

      isolate({
        withProgress(message = sample(.app$quotes, 1), detail = "Mapping gene ids (5 minutes)", {

          # Intialization
          string_db <- STRINGdb::STRINGdb$new(
            version = STRING_DB_VERSION, species = taxonomyID,
            score_threshold = 0, input_directory = ""
          )

          # using expression data
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

    output$STRINGDB_mapping_stat <- renderText({
      if (goButton() == 0) {
        return(NULL)
      }

      if (is.null(STRINGdb_geneList())) {
        return("No genes mapped by STRINGdb. Please enter or double-check species name above.")
      }
      if (!is.null(STRINGdb_geneList())) {
        tem <- paste0(100 * round(STRINGdb_geneList()$ratio, 3), "% genes mapped by STRING web server.")
        if (STRINGdb_geneList()$ratio < 0.3) tem <- paste(tem, "Warning!!! Very few gene mapped. Double check if the correct species is selected.")
        return(tem)
      }
    })

    stringDB_GO_enrichmentData <- reactive({
      if (goButton() == 0) {
        return(-1)
      }
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }
      withProgress(message = sample(.app$quotes, 1), detail = "Enrichment analysis", {
        tem <- input$STRINGdbGO
        # Intialization
        string_db <- STRINGdb::STRINGdb$new(
          version = STRING_DB_VERSION, species = taxonomyID,
          score_threshold = 0, input_directory = ""
        )

        # using expression data
        genes <- conversionTableData()
        minGenesEnrichment <- 1
        if (is.null(genes)) {
          return(-2)
        } else if (dim(genes)[1] <= minGenesEnrichment) {
          return(-2) # if has only few genes
        } else {
          # GO
          ids <- STRINGdb_geneList()$up
          if (length(ids) <= minGenesEnrichment || is.null(ids)) {
            return(-2)
          }
          incProgress(1 / 3)
          result <- string_db$get_enrichment(ids, category = input$STRINGdbGO, methodMT = "fdr", iea = TRUE)
          if (nrow(result) == 0 || is.null(result)) {
            return(-2)
          } else {
            if (min(result$fdr) > minFDR()) {
              return(-2)
            } else {
              result <- result[which(result$fdr < minFDR()), ]
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
        if(class(result) == "numeric") {
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

    # observeEvent(input$STRING, {
    #   taxonomyID = findTaxonomyID()
    #   if(is.null(taxonomyID)) {
    #     return(NULL)
    #   } else {
    #     stringDB_GO_enrichmentData(input = input,
    #                                output = output,
    #                                taxonomyID = taxonomyID)
    #   } #end of if else
    # })# end of observeEvent


    output$stringDB_network1 <- renderPlot(
      {
        if (goButton() == 0) {
          return(NULL)
        }


        tem <- input$STRINGdbGO
        tem <- input$nGenesPPI
        taxonomyID <- findTaxonomyID()
        if (is.null(taxonomyID)) {
          return(NULL)
        }
        ####################################

        if (is.null(STRINGdb_geneList())) {
          return(NULL)
        }

        isolate({
          withProgress(message = sample(.app$quotes, 1), detail = "Enrichment analysis", {
            # Intialization
            string_db <- STRINGdb::STRINGdb$new(
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

    output$stringDB_network_link <- renderUI({

      tem <- input$STRINGdbGO
      tem <- input$nGenesPPI
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }

      ####################################
      if (is.null(STRINGdb_geneList())) {
        return(NULL)
      }

      isolate({
        withProgress(message = sample(.app$quotes, 1), detail = "PPI Enrichment and link", {
          # Intialization
          string_db <- STRINGdb::STRINGdb$new(
            version = STRING_DB_VERSION, species = taxonomyID,
            score_threshold = 0, input_directory = ""
          )
          # upregulated
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
