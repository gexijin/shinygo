# STRING Module for ShinyGO

# UI function for the module
mod_string_ui <- function(id) {
  # Namespace function to prefix IDs
  ns <- NS(id)
  
  tagList(
    textOutput(ns("STRINGDB_mapping_stat")),
    tags$head(tags$style(paste0("#", ns("STRINGDB_mapping_stat"), "{color: blue;font-size: 15px;}"))),
    br(),
    actionButton(ns("ModalPPI"), "PPI network of DEGs"), 
    br(), br(),
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
      This also enables the retrieval of a protein-protein network. If it is running, 
      please wait until it finishes. The second time it is faster."
    ),
    # Modal for PPI network
    bsModal(ns("ModalExamplePPI"), "Protein-protein interaction networks", ns("ModalPPI"),
      size = "large",
      h5("By sending your genes to the STRING website,
         shinyGO is retrieving a sub-network, calculating PPI enrichment,
         and generating custom URLs to the STRING website containing your genes. This can take 5 minutes. Patience will pay off!"),
      sliderInput(ns("nGenesPPI"), label = h5("Genes to include:"), min = 0, max = 400, value = 20, step = 10),
      plotOutput(ns("stringDB_network1"))
    )
  )
}

# Server function for the module
mod_string_server <- function(id, converted, findTaxonomyID, geneInfoLookup, conversionTableData, input_goButton, input_minFDR) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for STRING database gene list
    STRINGdb_geneList <- reactive({
      if (input_goButton() == 0) {
        return(NULL)
      }
      
      library(STRINGdb, verbose = FALSE)
      
      ####################################
      
      if (is.null(conversionTableData())) {
        return(NULL)
      } # this has to be outside of isolate() !!!
      
      # No significant enrichment found message
      NoSig <- as.data.frame("No significant enrichment found.")
      taxonomyID <- findTaxonomyID()
      
      if (is.null(taxonomyID)) {
        return(NULL)
      }
      
      isolate({
        withProgress(message = sample(quotes, 1), detail = "Mapping gene ids (5 minutes)", {
          
          # Intialization
          string_db <- STRINGdb$new(
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
    
    # Display STRING database mapping statistics
    output$STRINGDB_mapping_stat <- renderText({
      if (input_goButton() == 0) {
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
    
    # STRING database GO enrichment data
    stringDB_GO_enrichmentData <- reactive({
      if (input_goButton() == 0) {
        return(-1)
      }
      taxonomyID <- findTaxonomyID()
      if (is.null(taxonomyID)) {
        return(NULL)
      }
      
      library(STRINGdb, verbose = FALSE)
      withProgress(message = sample(quotes, 1), detail = "Enrichment analysis", {
        tem <- input$STRINGdbGO
        # Intialization
        string_db <- STRINGdb$new(
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
            if (min(result$fdr) > input_minFDR()) {
              return(-2)
            } else {
              result <- result[which(result$fdr < input_minFDR()), ]
              incProgress(1, detail = paste("Done"))
              return(result)
            } # end of check minFDR
          } # check results
        } # end of check genes if
      }) # progress
    }) # end of stringDB_GO_enrichmentData
    
    # Display enrichment table
    output$stringDB_GO_enrichment <- renderTable({
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
    })
    
    # Download handler for STRING enrichment
    output$STRING_enrichmentDownload <- downloadHandler(
      filename = function() {
        paste0("STRING_enrichment", input$STRINGdbGO, ".csv")
      },
      content = function(file) {
        write.csv(stringDB_GO_enrichmentData(), file)
      }
    )
    
    # Display STRING-db network
    output$stringDB_network1 <- renderPlot({
      library(STRINGdb)
      if (input_goButton() == 0) {
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
        withProgress(message = sample(quotes, 1), detail = "Enrichment analysis", {
          # Intialization
          string_db <- STRINGdb$new(
            version = STRING_DB_VERSION, species = taxonomyID,
            score_threshold = 0, input_directory = ""
          )
          # only up regulated is plotted
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
    })
    
  }) # moduleServer
}