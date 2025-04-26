####################################################
# Author: Steven Ge Xijin.Ge@sdstate.edu
# Lab: Ge Lab
# R version 4.0.5
# Project: ShinyGO v76
# File: server.R
# Purpose of file:main server logic of app
# Start data: NA (mm-dd-yyyy)
# Data last modified: 09-2-2021
#######################################################
server <- function(input, output, session) {
  options(warn = -1)
  welcome_modal <- shiny::modalDialog(
    title = "Find ShinyGO helpful? Send us an email today so it will be here next year.",
    tags$h3("We are still working on it until June 5th!"),
    tags$p("We need your help to support our NIH grant proposal due June 5th."),
 
    tags$p(
      " Please take a few minutes to send us an email today:  ",
      a(
        " gelabinfo@gmail.com",
        href = "mailto:gelabinfo@gmail.com",
        target = "_blank"
      ), 
      "  Thank you!"
    ),

    easyClose = FALSE,
    size = "l"
  )

  #shiny::showModal(welcome_modal)
  observe({
     # for gene ID example

    updateSelectizeInput(session, "userSpecieIDexample", choices = speciesChoice, selected = speciesChoice[1])

    # load demo data when clicked

    if (input$useDemo1) {
      updateTextInput(session, "input_text", value = ExampleGeneList1)
    }

  })
  # click_saved <- reactiveValues(GO = NULL)
  # observeEvent(eventExpr = input$selectGO, handlerExpr = { click_saved$GO <- input$selectGO })

  observe({
    # Hide genome tab when STRINGdb is matched
    showTab(inputId = "tabs", target = "8")

    if (input$goButton != 0 && !is.null(converted()$speciesMatched)) {
      if (grepl("STRING", converted()$speciesMatched[1, 1])) {
        hideTab(inputId = "tabs", target = "8")
      }
    }
  })

  # connect to species specific database
  observeEvent(input$selectOrg, {

    # connect to the database, this becomes a global variable
    convert_species <- connect_convert_db_org(datapath, input$selectOrg)

    # idIndex
    idIndex <- dbGetQuery(
      convert_species,
      "select * from idIndex;"
    )
    dbDisconnect(convert_species)
  })

  # this defines an reactive object that can be accessed from other rendering functions
  converted <- reactive({
    if (input$goButton == 0 | nchar(input$input_text) < 20) {
      return()
    }

    converted <- convertID(input$input_text, input$selectOrg)

    # remove ensembl gene IDs mapped to the same gene (marked as duplicated in gene info)
    if(as.numeric(input$selectOrg) > 0) { # if it is ENSEMBL, not STRING species
      gene_info <- geneInfo(converted, input$selectOrg)
      converted$IDs <- gene_info |>
        filter(!duplicated) |>
        filter(ensembl_gene_id %in% converted$IDs) |>
        pull(ensembl_gene_id)
      #conversionTable is not changed. Not unique.
    }
    converted

  })

  # Pop-up modal for gene assembl information ----
  observeEvent(input$genome_assembl_button, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = "Click on a row to select a species",
        p("Search annotated species by common or scientific names,
          or NCBI taxonomy id. Click on a row to select.
          Use annotation in STRING-db as a last resort.
          "),
        easyClose = TRUE,

        DT::renderDataTable({
          df <- orgInfo[
            ,
            c("ensembl_dataset", "academicName", "name",  "taxon_id", "group")
          ]
          colnames(df) <- c(
            "Ensembl/STRING-db ID",
            "Academic Name",
            "Name (Assembly)",
            "Taxonomy ID",
            "Source"
          )
          row.names(df) <- NULL
          DT::datatable(
            df,
            selection = "single",
            options = list(
              lengthChange = FALSE,
              pageLength = 20,
              scrollY = "400px"
            ),
            callback = DT::JS(
              paste0(
                "table.on('click', 'tr', function() {
                    var data = table.row(this).data();
                    if (data) {
                      Shiny.setInputValue('clicked_row', data[0]);
                    }
                  });"
              )
            ),
            rownames = FALSE
          )
        })
      )
    )
  })

  # default species name
  selected_species_name <- reactiveVal("Human")

  output$selected_species <- renderText({
    tem <- input$clicked_row
    selected_species_name()
  })

  observeEvent(input$clicked_row, {
    # find species ID from ensembl_dataset
    selected <- find_species_id_by_ensembl(
      input$clicked_row,
      orgInfo
    )
    # assign name
    selected <- setNames(
      selected,
      find_species_by_id_name(selected, orgInfo)
    )

    updateSelectizeInput(
      session = session,
      inputId = "selectOrg",
      choices = selected,
      selected = selected,
      server = TRUE
    )
    # update species name
    selected_species_name(find_species_by_id_name(selected, orgInfo))
  })

  geneInfoLookup <- reactive({
    if (input$goButton == 0) {
      return()
    }
    geneInfo(converted(), input$selectOrg) # uses converted gene ids thru converted() call
  })

  detailedGeneInfoLookup <- reactive({
    if (input$goButton == 0) {
      return()
    }
    geneInfoDetails(converted(), input$selectOrg) # uses converted gene ids thru converted() call
  })
  # this defines an reactive object that can be accessed from other rendering functions
  converted_background <- reactive({
    if (input$goButton == 0 | is.null(input$input_text_b)) {
      return()
    }
    if (nchar(input$input_text_b) < 10) {
      return()
    }
    
    converted <- convertID(input$input_text_b, input$selectOrg)
    if(as.numeric(input$selectOrg) > 0) { # if it is ENSEMBL, not STRING species
      gene_info <- geneInfo(converted, input$selectOrg)
      # remove ensembl gene IDs mapped to the same gene (marked as duplicated in gene info)
      converted$IDs <- gene_info |>
        filter(!duplicated) |>
        filter(ensembl_gene_id %in% converted$IDs) |>
        pull(ensembl_gene_id)
      #conversionTable is not changed. Not unique.
    }

    # if more than 100k genes, take samples
    if(length(converted$IDs) > maxGenesBackground + 1) {
      converted$IDs <- sample(converted$IDs, maxGenesBackground)
    }

    converted

  })

  geneInfoLookup_background <- reactive({
    if (input$goButton == 0 | nchar(input$input_text_b) < 10) {
      return()
    }
    if (is.null(converted_background())) {
      return()
    }
    geneInfo(converted_background(), input$selectOrg) # uses converted gene ids thru converted() call
  })

  significantOverlapsAll <- reactive({
    if (input$goButton == 0 | is.null(input$selectGO) | nchar(input$input_text) < 20) {
      return()
    }
    tem <- input$selectOrg
    tem <- input$selectGO
    tem <- input$gene_count_pathwaydb
    tem <- input$minSetSize
    tem <- input$maxSetSize

    isolate({
      withProgress(message = sample(quotes, 1), detail = "enrichment analysis", {
        # gene info is passed to enable lookup of gene symbols
        tem <- geneInfoLookup()
        tem <- tem[which(tem$Set == "List"), ]
        temb <- geneInfoLookup_background()
        if (class(temb) == "data.frame") {
          temb <- temb[which(temb$Set == "List"), ]
        }
        enrichment <- FindOverlap(converted(), tem, input$selectGO, input$selectOrg,
          converted_background(), temb,
          minSetSize = input$minSetSize, maxSetSize = input$maxSetSize,
          gene_count_pathwaydb = input$gene_count_pathwaydb
        )
        return(enrichment)
      })
    })
  })

  observe({
    req(!is.null(significantOverlapsAll() )) # stop if null
    req(input$goButton != 0)
    req(significantOverlapsAll()$x[1,1] == "ID not recognized!" )

    shiny::showModal(
      shiny::modalDialog(
        size = "s",
        p("None of the gene IDs mapped to the IDs of the selected species.
           From ShinyGO 0.80, you have to select the correct species first.
           If you do not select, it defaults to human.
          "),
        easyClose = TRUE
      )
    )

  })

  # Filtering and ranking pathways
  significantOverlaps <- reactive({
    if (input$goButton == 0 | is.null(input$selectGO) | nchar(input$input_text) < 20) {
      return()
    }
    if (is.null(significantOverlapsAll())) {
      return(NULL)
    }

    enrichment <- significantOverlapsAll()
    withProgress(message = sample(quotes, 1), detail = "Sorting and filtering pathways", {
      if (dim(enrichment$x)[2] > 1) { # when there is no overlap, returns a data frame with 1 row and 1 column

        # filter by FDR-------------------------------------------------------------
        enrichment$x <- enrichment$x[enrichment$x[, 1] < input$minFDR, ]

        incProgress(0.1)
        # Sort and keep top pathways -------------------------------------------------------
        if (input$SortPathways == "Select by FDR, sort by Fold Enrichment") {
          # sort by FDR
          enrichment$x <- enrichment$x[order(enrichment$x[, 1]), ]
          # filter/top 20
          if (dim(enrichment$x)[1] > as.integer(input$maxTerms)) {
            enrichment$x <- enrichment$x[1:as.integer(input$maxTerms), ]
          }
          # rank by fold
          enrichment$x <- enrichment$x[order(enrichment$x[, 4], decreasing = TRUE), ]
        } else {
          if (input$SortPathways == "Sort by FDR") {
            enrichment$x <- enrichment$x[order(enrichment$x[, 1]), ]
          }
          if (input$SortPathways == "Sort by Fold Enrichment") {
            enrichment$x <- enrichment$x[order(enrichment$x[, 4], decreasing = TRUE), ]
            # when sorting by fold, sometimes tiny pathways on top. Here we require at
            # least 10 genes
            enrichment$x <- enrichment$x[which(enrichment$x[, 3] > min_gene_fold), ]
          }
          if (input$SortPathways == "Sort by Genes") {
            enrichment$x <- enrichment$x[order(enrichment$x[, 2], decreasing = TRUE), ]
          }
          if (input$SortPathways == "Sort by Category Name") {
            enrichment$x <- enrichment$x[order(enrichment$x[, 5]), ]
          }
          if (input$SortPathways == "Sort by FDR & Fold Enrichment") {
            fdr_rank <- rank(enrichment$x[, 1]) # rank by FDR
            fold_rank <- rank(-1 * enrichment$x[, 4]) # rank by fold_enrichment, descending
            average_rank <- (fdr_rank + fold_rank) / 2
            enrichment$x <- enrichment$x[order(average_rank), ]
          }
        }
        incProgress(0.3)

        # preliminary filtering to save time on string manipulations
        if (dim(enrichment$x)[1] > 3 * as.integer(input$maxTerms)) {
          enrichment$x <- enrichment$x[1:(3 * as.integer(input$maxTerms)), ]
        }


        # remove redudant gene sets-------------------------------------------
        if (input$removeRedudantSets) reduced <- redudantGeneSetsRatio else reduced <- FALSE
        incProgress(0.2)
        # reduced=FALSE no filtering,  reduced = 0.9 filter sets overlap with 90%
        if (reduced != FALSE && dim(enrichment$x)[1] > 5) {
          n <- nrow(enrichment$x)
          flag1 <- rep(TRUE, n)
          # note that it has to be two space characters for splitting
          geneLists <- lapply(
            enrichment$x$Genes,
            function(y) unlist(strsplit(as.character(y), " |  |   "))
          )
          pathways <- lapply(
            enrichment$x$Pathway,
            function(y) unlist(strsplit(as.character(y), " |  |   "))
          )
          for (i in 2:n) {
            for (j in 1:(i - 1)) {
              if (flag1[j]) { # skip if this one is already removed
                ratio1 <- length(intersect(geneLists[[i]], geneLists[[j]])) /
                  length(union(geneLists[[i]], geneLists[[j]]))

                # if sufficient genes overlap
                if (ratio1 > reduced) {
                  # are pathway names similar
                  ratio2 <- length(intersect(pathways[[i]], pathways[[j]])) /
                    length(union(pathways[[i]], pathways[[j]]))
                  # if 50% of the words in the pathway name shared
                  if (ratio2 > 0.5) {
                    flag1[i] <- FALSE
                  }
                }
              }
            }
          }
          # remove similar pathways
          enrichment$x <- enrichment$x[which(flag1), ]
        }
        incProgress(0.9)

        # keep top pathways
        if (dim(enrichment$x)[1] > as.integer(input$maxTerms)) {
          enrichment$x <- enrichment$x[1:as.integer(input$maxTerms), ]
        }

        if (input$abbreviatePathway) {
          enrichment$x[, 5] <- gsub("Positive regulation", "Pos. reg.", enrichment$x[, 5])
          enrichment$x[, 5] <- gsub("Negative regulation", "Neg. reg.", enrichment$x[, 5])
          enrichment$x[, 5] <- gsub("Regulation", "Reg.", enrichment$x[, 5])
          enrichment$x[, 5] <- gsub(" regulation ", " reg. ", enrichment$x[, 5])
          enrichment$x[, 5] <- gsub(" process ", " proc. ", enrichment$x[, 5])
          enrichment$x[, 5] <- substr(enrichment$x[, 5], 1, 100) # maximum 80 characters
        }
      }
    }) # progress bar

    return(enrichment)
  })

  output$species <- renderTable(
    {
      if (input$goButton == 0) {
        return()
      }
      tem <- input$selectGO
      tem <- input$selectOrg
      tem <- input$minFDR
      isolate({ # tem <- convertID(input$input_text,input$selectOrg );
        withProgress(message = "Converting gene IDs", {
          tem <- converted()
          incProgress(1, detail = paste("Done"))
        })

        if (is.null(tem)) {
          as.data.frame("ID not recognized.")
        } else {
          tem$speciesMatched
        }
      }) # avoid showing things initially
    },
    digits = -1,
    spacing = "s",
    striped = TRUE,
    bordered = TRUE,
    width = "auto",
    hover = T
  )


  # Species match message ---------- stole from Gavin's code 4/20/22
  observe({
    req(
      input$selectOrg == speciesChoice[[1]] # best matching species
      && !is.null(converted()) # finished
    )
    showNotification(
      ui = paste(
        gsub("\\(.*", "", converted()$speciesMatched[1, ]),
        ": is the best matching species. If that is incorrect,
                     please use the dropdown to select
                    your species."
      ),
      id = "species_match",
      duration = NULL,
      type = "error"
    )
  })

  output$showGeneIDs4Species <- renderTable(
    {
      if (input$userSpecieIDexample == 0) {
        return()
      }
      withProgress(message = "Retrieving gene IDs (2 minutes)", {
        geneIDs <- showGeneIDs(species = input$userSpecieIDexample, nGenes = 10)
        incProgress(1, detail = paste("Done"))
      })
      geneIDs
    },
    digits = -1,
    spacing = "s",
    striped = TRUE,
    bordered = TRUE,
    width = "auto",
    hover = T
  )

  output$orgInfoTable <- DT::renderDataTable({
    df <- orgInfo[, c("ensembl_dataset", "name", "totalGenes")]
    colnames(df) <- c("Ensembl/STRING-db ID", "Name (Assembly)", "Total Genes")
    row.names(df) <- NULL
    df
  })

  promoterData <- reactive({
    if (input$goButton == 0) {
      return()
    }
    tem <- input$radio
    tem <- input$selectOrg
    isolate({
      myMessage <- "Promoter analysis"
      withProgress(message = sample(quotes, 1), detail = myMessage, {
        tem <- promoter(converted(), input$selectOrg, input$radio)
        incProgress(1, detail = paste("Done"))
      })

      if (is.null(tem)) {
        return(as.data.frame("ID not recognized."))
      } else {
        return(tem)
      }
    }) # avoid showing things initially
  })

  output$promoter <- renderTable(
    {
      if (input$goButton == 0) {
        return()
      }
      tem <- input$radio
      tem <- input$selectOrg
      isolate({
        promoterData()
      }) # avoid showing things initially
    },
    digits = -1,
    spacing = "s",
    striped = TRUE,
    bordered = TRUE,
    width = "auto",
    hover = T
  )

  output$downloadPromoter <- downloadHandler(
    filename = function() {
      "promoterMotif.csv"
    },
    content = function(file) {
      write.csv(promoterData(), file, row.names = FALSE)
    }
  )

  output$mapping_stats <- renderText({
    req(input$goButton)
    req(converted())
    n_genes <- length(converted()$originalIDs)
    n_mapped <- length(converted()$IDs)

    paste0(n_genes, " IDs mapped to ", n_mapped, " (", round(n_mapped / n_genes * 100, 0), "%) ", tolower(converted()$species$name2), " genes.")
  })


  conversionTableData <- reactive({
    if (input$goButton == 0) {
      return()
    } # still have problems when geneInfo is not found!!!!!
    tem <- input$selectGO
    tem <- input$selectOrg
    tem <- input$minFDR
    isolate({
      withProgress(message = sample(quotes, 1), detail = "Looking up gene Info", {
        tem <- converted()
        incProgress(0.1)
        tem2 <- geneInfoLookup()
        incProgress(0.3)
        incProgress(0.6)
        if (is.null(tem)) {
          as.data.frame("ID not recognized.")
        } else {
          # some STRINGdb species has geneInfo, alought incomplete.
          if (dim(tem2)[1] <= 1 | grepl("STRINGdb", converted()$species$name2)) {
            merged <- tem$conversionTable
            ix <- which(colnames(merged) == "ensembl_gene_id")
            colnames(merged)[ix] <- "STRINGdb ID"
          } else { # if gene info is  available
            #         if('chromosome_name' %in% colnames(tem2)) {
            merged <- merge(tem$conversionTable, tem2, by = "ensembl_gene_id")

            merged <- subset(merged, select = c(
              User_input, symbol, ensembl_gene_id, entrezgene_id,
              gene_biotype, Species, chromosome_name, start_position,
              description, percentage_gc_content, transcript_count,
              genomeSpan, cds_length, transcript_length, FiveUTR,
              ThreeUTR, nExons
            ))

            tem3 <- as.data.frame(tem$originalIDs)
            colnames(tem3) <- "User_input"
            merged <- merge(merged, tem3, all = T)
            merged$ensembl_gene_id[which(is.na(merged$ensembl_gene_id))] <- "Not mapped"
            chrName <- suppressWarnings(as.numeric(as.character(merged$chromosome_name)))
            merged <- merged[order(
              merged$gene_biotype,
              chrName,
              merged$start_position
            ), ]
            merged$start_position <- merged$start_position / 1e6
            colnames(merged)[1:9] <- c(
              "Pasted", "Symbol", "Ensembl Gene ID", "Entrez",
              "Type", "Species", "Chr", "Position (Mbp)", "Description"
            )
          }
        }
        incProgress(0.9)
        return(merged)
      })
    }) # avoid showing things initially
  })

 

  output$EnrichmentTable <- renderTable(
    {
      if (input$goButton == 0) {
        return(NULL)
      }
      tem <- input$input_text_b # just to make it re-calculate if user changes background

      myMessage <- "Analyzing genes."

      if (is.null(significantOverlaps())) {
        return(NULL)
      }
      # this solves an error when there is no significant enrichment

      if (ncol(significantOverlaps()$x) == 1) {
        return(significantOverlaps()$x)
      }

      withProgress(message = sample(quotes, 1), detail = myMessage, {
        pathways <- significantOverlaps()$x
        # remove pathway ID  only in Ensembl species
        if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
          pathways$Pathway <- remove_pathway_id(pathways$Pathway, input$selectGO)
        }
        pathways$Pathway <- hyperText(pathways$Pathway, pathways$URL)

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

  significantOverlaps2 <- reactive({
    if (input$goButton == 0) {
      return()
    }
    tem <- input$input_text_b # just to make it re-calculate if user changes background

    tem <- significantOverlaps()
    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem <- tem$x
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL
    tem$Direction <- "Diff"
    # remove pathway ID  only in Ensembl species
    if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, input$selectGO)
    }
    tem
  })

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


  #----------------------------------------------------
  # STRING-db functionality
  # find Taxonomy ID from species official name
  findTaxonomyID <- reactive({
    if (input$goButton == 0) {
      return(NULL)
    }
    
    find_taxon_by_id(input$selectOrg, orgInfo)

  })

  output$selectGO1 <- renderUI({ # gene set for pathway analysis
    if (input$goButton == 0) {
      return(NULL)
    }

    choices <- gmtCategory(converted(), input$selectOrg)
    if (length(choices) > 12) { # more than 12 categories in human and mouse, we default to GOBP
      selected <- "GOBP"
    } else { # otherwise all gene sets
      selected <- "All"
    }
    if ("KEGG" %in% choices) {
      selected <- "KEGG"
    }


    selectInput("selectGO",
      label = h5("Pathway database:"),
      choices = choices,
      selected = selected
    )
  })

  output$tableDetail <- renderTable(
    {
      if (input$goButton == 0) {
        return()
      }

      tem <- significantOverlaps()
      tem$x
    },
    digits = -1,
    spacing = "s",
    striped = TRUE,
    bordered = TRUE,
    width = "auto",
    hover = T
  )

 
  # ggplot2 object for the enrichment chart;
  # used both for display and download
  enrichChartObject <- reactive({
    if (input$goButton == 0) {
      return()
    }

    if (is.null(significantOverlaps())) {
      return(NULL)
    }
    if (ncol(significantOverlaps()$x) == 1) {
      return(NULL)
    } # no significant ones found.
    tem <- input$selectOrg
    tem <- input$SortPathwaysPlot
    tem <- input$SortPathwaysPlotX
    tem <- input$SortPathwaysPlotSize
    tem <- input$SortPathwaysPlotColor
    tem <- input$SortPathwaysPlotFontSize
    tem <- input$SortPathwaysPlotMarkerSize
    tem <- input$SortPathwaysPlotHighColor
    tem <- input$SortPathwaysPlotLowColor
    tem <- input$enrichChartType
    tem <- input$enrichChartAspectRatio
    tem <- input$maxTerms
    tem <- input$abbreviatePathway
    req(input$ggplot2_theme)
    tem <- input$show_pathway_id

    isolate({
      goTable <- significantOverlaps()$x[, 1:5]

      # Remove spaces in col names
      colnames(goTable) <- gsub(" ", "", colnames(goTable))


      x <- input$SortPathwaysPlotX
      size <- input$SortPathwaysPlotSize
      colorBy <- input$SortPathwaysPlotColor
      fontSize <- input$SortPathwaysPlotFontSize
      markerSize <- input$SortPathwaysPlotMarkerSize
      # validate values; users can input any numeric value outside the range
      if (fontSize < 1 | fontSize >= 20) {
        fontSize <- 12
      }
      if (markerSize < 0 | markerSize > 20) {
        markerSize <- 4
      }

      # convert to vector so that we can look up the readable names of columns
      columns <- unlist(columnSelection)

      goTable$EnrichmentFDR <- -log10(goTable$EnrichmentFDR)
      ix <- which(colnames(goTable) == input$SortPathwaysPlot)

      # sort the pathways
      if (ix > 0 && ix < dim(goTable)[2]) {
        goTable <- goTable[order(goTable[, ix], decreasing = TRUE), ]
      }

      # remove pathway ID  only in Ensembl species
      if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
        goTable$Pathway <- remove_pathway_id(goTable$Pathway, input$selectGO)
      }

      # Error when two pathways are of the same name due to truncation of long pathways
      goTable$Pathway <- mark_duplicates(goTable$Pathway)

      # convert to factor so that the levels are not reordered by ggplot2
      goTable$Pathway <- factor(goTable$Pathway, levels = rev(goTable$Pathway))

      p <- ggplot(goTable, aes_string(x = x, y = "Pathway", size = size, color = colorBy)) +
        geom_point() +
        scale_color_continuous(
          low = input$SortPathwaysPlotLowColor,
          high = input$SortPathwaysPlotHighColor,
          name = names(columns)[columns == colorBy],
          guide = guide_colorbar(reverse = TRUE)
        )

      p <- refine_ggplot2(
        p = p,
        gridline = FALSE,
        ggplot2_theme = input$ggplot2_theme
      )

      p <- p + scale_size(range = c(1, markerSize)) +
        xlab(names(columns)[columns == x]) +
        ylab(NULL) +
        guides(
          size = guide_legend(order = 2, title = names(columns)[columns == size]),
          color = guide_colorbar(order = 1)
        ) +
        theme(axis.text = element_text(size = fontSize), axis.title = element_text(size = 12)) +
        theme(
          legend.title = element_text(size = 12), # decrease legend font
          legend.text = element_text(size = 12)
        ) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        guides(color = guide_legend(override.aes = list(size = 5)))

      if (input$enrichChartType == "dotplot") {
        p <- p
      } else if (input$enrichChartType == "lollipop") {
        p <- p +
          geom_segment(aes_string(
            x = 0,
            xend = x,
            y = "Pathway",
            yend = "Pathway"
          ),
          size = 1
          )
      } else if (input$enrichChartType == "barplot") {
        p <- ggplot(goTable, aes_string(x = x, y = "Pathway", fill = colorBy)) +
          geom_col(width = 0.8, position = position_dodge(0.7)) +
          scale_fill_continuous(
            low = input$SortPathwaysPlotLowColor,
            high = input$SortPathwaysPlotHighColor,
            name = names(columns)[columns == colorBy],
            guide = guide_colorbar(reverse = TRUE)
          )
        p <- refine_ggplot2(
          p = p,
          gridline = FALSE,
          ggplot2_theme = input$ggplot2_theme
        )
        p <- p +
          xlab(names(columns)[columns == x]) +
          ylab(NULL) +
          theme(axis.text = element_text(size = fontSize))
      } else if(input$enrichChartType == "barplot_inside") {
        p <- ggplot(goTable, aes_string(x = x, y = "Pathway", fill = colorBy)) +
          geom_col(width = 0.8, position = position_dodge(0.7)) +
          scale_fill_continuous(
            low = input$SortPathwaysPlotLowColor,
            high = input$SortPathwaysPlotHighColor,
            name = names(columns)[columns == colorBy],
            guide = guide_colorbar(reverse = TRUE)
          )
        p <- refine_ggplot2(
          p = p,
          gridline = FALSE,
          ggplot2_theme = input$ggplot2_theme
        )
        p <- p +
          xlab(names(columns)[columns == x]) +
          ylab(NULL) +
          theme(axis.text = element_text(size = fontSize))+
          geom_text( # add text inside the bars
            aes_string(x = 0, label = "Pathway"),
            hjust = 0, 
            #vjust = -1, 
            color="black", 
            size=fontSize / 2
          )  +
          theme(
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank()
          )

      }

      return(p)
    }) # isolate
  })

  observeEvent(input$enrichChartType, {
    req (input$enrichChartType == "barplot_inside") 
    updateSliderInput(
      inputId = "SortPathwaysPlotFontSize",
      value = 10
    )
    updateSliderInput(
      inputId = "enrichChartAspectRatio",
      value = 1.5
    )
    updateSelectInput(
      inputId = "SortPathwaysPlotLowColor",
      selected = "yellow"
    )
    updateSelectInput(
      inputId = "maxTerms",
      selected = "15"
    )
    #show notification that maxTerms is set to 15
    showNotification(
      "To improve the chart, we adjusted the aspect ratio, font size, and color:low. The number of pathways to show is set to 15.",
      type = "message",
      duration = 5
    )
  })


  # Enrichment plot for display on the screen
  # https://stackoverflow.com/questions/34792998/shiny-variable-height-of-renderplot
  output$enrichChart <- renderPlot(
    {
      enrichChartObject()
    },
    # height increases as the number of terms increase. max at 1200, min 350
    height = function() {
      round(max(350, min(2500, round(18 * as.numeric(input$maxTerms)))))
    },
    width = function() {
      round(max(350, min(2500, round(18 * as.numeric(input$maxTerms)))) * as.numeric(input$enrichChartAspectRatio))
    }
  )

  download_barplot <- mod_download_images_server(
    "download_barplot",
    filename = "barplot",
    figure = reactive({
      enrichChartObject()
    }),
    width = 8,
    height = round(8 / as.numeric(input$enrichChartAspectRatio), 1)
  )

    mod_tree_server("tree", 
    significantOverlaps2 = reactive({
      if (input$goButton == 0) {
        return()
      }
      tem <- input$input_text_b # just to make it re-calculate if user changes background

      tem <- significantOverlaps()
      if (dim(tem$x)[2] == 1) {
        return(NULL)
      }
      tem <- tem$x
      colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes")
      tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL
      tem$Direction <- "Diff"
      # remove pathway ID only in Ensembl species
      if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
        tem$Pathways <- remove_pathway_id(tem$Pathways, input$selectGO)
      }
      tem
    }),
    input_goButton = reactive(input$goButton),
    input_maxTerms = reactive(input$maxTerms)
  )

  mod_chart_server("chart", 
    significantOverlaps = significantOverlaps,
    input_goButton = reactive(input$goButton),
    input_selectOrg = reactive(input$selectOrg),
    input_selectGO = reactive(input$selectGO),
    input_maxTerms = reactive(input$maxTerms),
    input_show_pathway_id = reactive(input$show_pathway_id)
  )


  mod_network_server("network", 
    significantOverlaps = significantOverlaps,
    input_goButton = reactive(input$goButton),
    input_input_text_b = reactive(input$input_text_b),
    input_show_pathway_id = reactive(input$show_pathway_id),
    input_selectOrg = reactive(input$selectOrg),
    input_selectGO = reactive(input$selectGO)
  )


  mod_kegg_server("kegg", 
    converted = converted, 
    significantOverlaps = significantOverlaps, 
    input_selectGO = reactive(input$selectGO),
    input_goButton = reactive(input$goButton),
    input_selectOrg = reactive(input$selectOrg)
  )

  mod_genes_server("genes", 
    converted = converted, 
    geneInfoLookup = geneInfoLookup,
    input_goButton = reactive(input$goButton),
    conversionTableData = conversionTableData,  
    significantOverlapsAll = significantOverlapsAll  
  )

  mod_plots_server("plots", 
    geneInfoLookup = geneInfoLookup, 
    converted = converted, 
    geneInfoLookup_background = geneInfoLookup_background, 
    converted_background = converted_background,
    input_goButton = reactive(input$goButton),
    input_ggplot2_theme = reactive(input$ggplot2_theme),
    PvalGeneInfo = 0.05,    
    PvalGeneInfo1 = 0.01,   
    PvalGeneInfo2 = 0.001,  
    minGenes = 10           
  )

  mod_genome_server("genome",
    geneInfoLookup = geneInfoLookup, 
    converted = converted,
    geneInfoLookup_background = geneInfoLookup_background,
    converted_background = converted_background,
    input_goButton = reactive(input$goButton),
    quotes = quotes
  )

  mod_string_server("string", 
    converted = converted,
    findTaxonomyID = findTaxonomyID,
    geneInfoLookup = geneInfoLookup,
    conversionTableData = conversionTableData,
    input_goButton = reactive(input$goButton),
    input_minFDR = reactive(input$minFDR)
  )
  # Call the module server function
  mod_about_server("about")
}
