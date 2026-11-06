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
  load_app_data() # database data shared by all sessions; only the first loads it
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

    updateSelectizeInput(session, "userSpecieIDexample", choices = .app$speciesChoice, selected = .app$speciesChoice[1])

    # load demo data when clicked

    if (input$useDemo1) {
      updateTextInput(session, "input_text", value = ExampleGeneList1)
    }


    # update species for STRING-db related API access
    # tried to solve the double reflashing problems
    # https://stackoverflow.com/questions/30991900/avoid-double-refresh-of-plot-in-shiny


  })
  # click_saved <- reactiveValues(GO = NULL)
  # observeEvent(eventExpr = input$selectGO, handlerExpr = { click_saved$GO <- input$selectGO })

  #-----------hide tabs when N/A----------------------------------
  observeEvent(input$selectGO, {

    # Show KEGG tab only when KEGG is selected  #disabled as of 4/8/2022. Confused biologists.
    # if(input$selectGO == "KEGG") {
    #  showTab(inputId = "tabs", target = "2")
    # } else {
    # hideTab(inputId = "tabs", target = "2")
    # }

    # Show Groups tab only when GOBP is selected
    if (input$selectGO == "GOBP" | input$selectGO == "GOCC" | input$selectGO == "GOMF") {
      showTab(inputId = "tabs", target = "7")
    } else {
      hideTab(inputId = "tabs", target = "7")
    }
  })

  observe({
    # Hide genome tab when STRINGdb is matched
    showTab(inputId = "tabs", target = "8")

    if (input$goButton != 0 && !is.null(converted()$speciesMatched)) {
      if (grepl("STRING", converted()$speciesMatched[1, 1])) {
        hideTab(inputId = "tabs", target = "8")
      }
    }
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

      # geneInfo() returns a single-column "ID not recognized!" frame when
      # nothing maps, most often because the gene list and the selected species
      # do not match. That frame has no `duplicated` column, so filter(!duplicated)
      # resolves to base::duplicated and aborts the reactive, taking every
      # downstream tab with it. Warn and carry on instead.
      if (is.data.frame(gene_info) && "duplicated" %in% colnames(gene_info)) {
        converted$IDs <- gene_info |>
          filter(!duplicated) |>
          filter(ensembl_gene_id %in% converted$IDs) |>
          pull(ensembl_gene_id)
        #conversionTable is not changed. Not unique.
      } else {
        showNotification(
          paste(
            "No gene information found for these genes in",
            findSpeciesByIdName(input$selectOrg),
            "- please check that the selected species is correct. Reset and try again."
          ),
          id = "species_mismatch",
          type = "warning",
          duration = 15
        )
      }
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
          Ensembl annotation is preferred when available.
          "),
        easyClose = TRUE,

        DT::renderDataTable({
          df <- .app$orgInfo[
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
      .app$orgInfo
    )
    # assign name
    selected <- setNames(
      selected,
      findSpeciesByIdName(selected)
    )

    updateSelectizeInput(
      session = session,
      inputId = "selectOrg",
      choices = selected,
      selected = selected,
      server = TRUE
    )
    # update species name
    selected_species_name(findSpeciesByIdName(selected))
  })

  geneInfoLookup <- reactive({
    if (input$goButton == 0) {
      return()
    }
    geneInfo(converted(), input$selectOrg) # uses converted gene ids thru converted() call
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
    # Must be read outside the isolate() below, or changing it never
    # invalidates this reactive and the Genes column silently never updates.
    tem <- input$genesColumnID

    isolate({
      withProgress(message = sample(.app$quotes, 1), detail = "enrichment analysis", {
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
          gene_count_pathwaydb = input$gene_count_pathwaydb,
          genes_column = input$genesColumnID
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
    withProgress(message = sample(.app$quotes, 1), detail = "Sorting and filtering pathways", {
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
          # Membership as Ensembl ids, straight from FindOverlap(). The old
          # split on " |  |   " was a workaround for parsing the display
          # string; there is nothing to parse now.
          geneLists <- lapply(enrichment$x$Membership, as.character)
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
      input$selectOrg == .app$speciesChoice[[1]] # best matching species
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
    df <- .app$orgInfo[, c("ensembl_dataset", "name", "totalGenes")]
    colnames(df) <- c("Ensembl/STRING-db ID", "Name (Assembly)", "Total Genes")
    row.names(df) <- NULL
    df
  })

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
      withProgress(message = sample(.app$quotes, 1), detail = "Looking up gene Info", {
        tem <- converted()
        incProgress(0.1)
        tem2 <- geneInfoLookup()
        incProgress(0.3)
        incProgress(0.6)
        if (is.null(tem)) {
          # Must be assigned: the function ends in return(merged), so leaving
          # this unassigned falls through to "object 'merged' not found" and
          # the user gets a stack trace instead of the message.
          merged <- as.data.frame("ID not recognized.")
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

  # Genes tab
  mod_genes_server(
    "genes",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    conversionTableData = conversionTableData
  )

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

      withProgress(message = sample(.app$quotes, 1), detail = myMessage, {
        pathways <- significantOverlaps()$x
        # remove pathway ID  only in Ensembl species
        if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
          pathways$Pathway <- remove_pathway_id(pathways$Pathway, input$selectGO)
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
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes", "Membership")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL
    tem$Direction <- "Diff"
    # remove pathway ID  only in Ensembl species
    if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, input$selectGO)
    }
    tem
  })

  # duplicate of the above, with the word wrapping. This is for use in the network plot
  significantOverlaps3 <- reactive({
    if (input$goButton == 0) {
      return()
    }
    tem <- input$input_text_b # just to make it re-calculate if user changes background

    tem <- significantOverlaps()
    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem <- tem$x
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes", "Membership")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL

    # remove pathway ID  only in Ensembl species
    if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, input$selectGO)
    }

    if (input$wrapTextNetwork) {
      tem$Pathways <- wrap_strings(tem$Pathways)
    } # wrap long pathway names using default width of 30 10/21/19

    tem$Direction <- "Diff"
    tem
  })

  # duplicate of the above for word wrapping in static networkplot
  significantOverlaps4 <- reactive({
    if (input$goButton == 0) {
      return()
    }
    tem <- input$input_text_b # just to make it re-calculate if user changes background
    tem <- significantOverlaps()
    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem <- tem$x
    colnames(tem) <- c("adj.Pval", "nGenesList", "nGenesCategor", "Fold", "Pathways", "URL", "Genes", "Membership")
    tem$Pathways <- gsub(".*'_blank'>|</a>", "", tem$Pathways) # remove URL

    # remove pathway ID  only in Ensembl species
    if (!input$show_pathway_id && as.integer(input$selectOrg) > 0) {
      tem$Pathways <- remove_pathway_id(tem$Pathways, input$selectGO)
    }
    if (input$wrapTextNetworkStatic) {
      tem$Pathways <- wrap_strings(tem$Pathways)
    } # wrap long pathway names using default width of 30 10/21/19

    tem$Direction <- "Diff"
    tem
  })

  output$GOTermsTree <- renderPlot(
    {
      if (input$goButton == 0) {
        return(NULL)
      }
      if (is.null(significantOverlaps2())) {
        return(NULL)
      }
      tem <- input$maxTerms
      # enrichmentPlot(significantOverlaps2(), 56  )
      tree_plot()
    },
    height = function() {
      round(max(350, min(2500, round(18 * as.numeric(input$maxTerms)))))
    },
    width = function() {
      width1 <- round(max(350, min(1000, round(18 * as.numeric(input$maxTerms)))) * as.numeric(input$treeChartAspectRatio))
      return(min(width1, 1000)) # max width is 1000
    }
  )



  tree_plot <- reactive({
    if (input$goButton == 0) {
      return(NULL)
    }
    if (is.null(significantOverlaps2())) {
      return(NULL)
    }
    tem <- input$maxTerms
    p <- enrichmentPlot(significantOverlaps2(), 45)
    return(p)
  })

  download_tree <- mod_download_images_server(
    "download_tree",
    filename = "tree_plot",
    figure = reactive({
      tree_plot()
    }),
    width = 10,
    height = round(10 / as.numeric(input$treeChartAspectRatio), 1)
  )


  output$enrichmentNetworkPlot <- renderPlot(
    {
      if (is.null(significantOverlaps4())) {
        return(NULL)
      }

      enrichmentNetwork(significantOverlaps4(), layoutButton = input$layoutButtonStatic, edge.cutoff = input$edgeCutoff)
    },
    height = 900
  )

  output$enrichmentNetworkPlotDownload <- downloadHandler(
    filename = "enrichmentPlotNetworkPathway.tiff",
    content = function(file) {
      tiff(file, width = 12, height = 12, units = "in", res = 300, compression = "lzw")
      enrichmentNetwork(significantOverlaps4(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      dev.off()
    }
  )

  # note the same code is used twice as above. They need to be updated together!!!
  output$enrichmentNetworkPlotInteractive <- renderVisNetwork({
    if (is.null(significantOverlaps3())) {
      return(NULL)
    }

    g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
    data1 <- toVisNetworkData(g)

    # Color codes: https://www.rapidtables.com/web/color/RGB_Color.html
    data1$nodes$shape <- "dot"
    # remove the color change of nodes
    # data1$nodes <- subset(data1$nodes, select = -color)

    data1$nodes$size <- 5 + data1$nodes$size^2
    visNetwork(nodes = data1$nodes, edges = data1$edges, height = "700px", width = "700px") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visNodes(
        color = list(
          # background = "#32CD32",
          border = "#000000",
          highlight = "#FF8000"
        ),
        font = list(
          color = "#000000",
          size = 20
        ),
        borderWidth = 1,
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#A9A9A9", highlight = "#FFD700")
      ) %>%
      visExport(
        type = "jpeg",
        name = "export-network",
        float = "left",
        label = "Export image",
        background = "white",
        style = ""
      )
  })

  output$enrichmentNetworkPlotInteractiveDownload <- downloadHandler(
    filename = "enrichmentPlotNetwork.html",
    content = function(file) {
      # jpeg(file, width = 12, height = 12, units = 'in', res = 300, compression = 'lzw')

      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)

      # Color codes: https://www.rapidtables.com/web/color/RGB_Color.html
      data1$nodes$shape <- "dot"
      # remove the color change of nodes
      # data1$nodes <- subset(data1$nodes, select = -color)

      data1$nodes$size <- 5 + data1$nodes$size^2
      g2 <-
        visNetwork(nodes = data1$nodes, edges = data1$edges, height = "700px", width = "700px") %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(
          color = list(
            # background = "#32CD32",
            border = "#000000",
            highlight = "#FF8000"
          ),
          font = list(
            color = "#000000",
            size = 20
          ),
          borderWidth = 1,
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(
          shadow = FALSE,
          color = list(color = "#A9A9A9", highlight = "#FFD700")
        ) %>%
        visSave(file = file, background = "white")
    }
  )

  output$downloadNodes <- downloadHandler(
    filename = function() {
      "network_nodes.csv"
    },
    content = function(file) {
      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)
      data1$nodes$shape <- "dot"
      data1$nodes$size <- 5 + data1$nodes$size^2

      write.csv(data1$nodes, file, row.names = FALSE)
    }
  )
  output$downloadEdges <- downloadHandler(
    filename = function() {
      "network_edges.csv"
    },
    content = function(file) {
      g <- enrichmentNetwork(significantOverlaps3(), layoutButton = input$layoutButton, edge.cutoff = input$edgeCutoff)
      data1 <- toVisNetworkData(g)
      data1$nodes$shape <- "dot"
      data1$nodes$size <- 5 + data1$nodes$size^2

      write.csv(data1$edges, file, row.names = FALSE)
    }
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
        selected = if (is.null(input$genesColumnID)) "symbol" else input$genesColumnID
      ),
      helpText(
        "Some species have gene symbols for only part of the genome. For those,
         the first option gives a mix of symbols and IDs. Choose one of the other
         two if you need a single consistent ID type."
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton(download_id, "Download")
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


  # STRING tab
  mod_string_server(
    "string",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    minFDR = reactive(input$minFDR),
    conversionTableData = conversionTableData
  )


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

  # Groups tab
  mod_groups_server(
    "groups",
    goButton = reactive(input$goButton),
    significantOverlaps = significantOverlaps
  )


  download_gene_barplot <- mod_download_images_server(
    "download_gene_barplot",
    filename = "gene_characteristics_barplot",
    figure = reactive({
      gene_barplot_object()
    }),
    width = 8,
    height = 20
  )

  output$gene_barplot <- renderPlot(
    {
      gene_barplot_object()
    },
    width = 600,
    height = 1500
  )
  # barplots using R base graphics
  gene_barplot_object <- reactive({
    if (input$goButton == 0) {
      return()
    }
    tem <- input$selectOrg
    isolate({
      withProgress(message = "Ploting gene characteristics", {
        x <- geneInfoLookup()
        x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses


        # background genes	--------------------------------------------------------------
        xB <- geneInfoLookup_background()
        convertedB <- converted_background()
        if (!is.null(xB) &&
          !is.null(convertedB) ) { 
          x <- x[x$Set == "List", ] # remove background from selected genes
          xB <- xB[xB$Set == "List", ] # remove Genome genes from background
          xB$Set <- "Background"
          x <- rbind(x, xB)
          x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses
        }
        # end background genes


        if (dim(x)[1] >= minGenes) # only making plots if more than 20 genes
          { # only plot when there 10 genes or more   # some columns have too many missing values
            par(mfrow = c(4, 1))
            par(mar = c(8, 6, 8, 2))
            # chromosomes
            if (sum(!is.na(x$chromosome_name)) >= minGenes && length(unique(x$chromosome_name)) > 2 && length(which(x$Set == "List")) > minGenes) {
              freq <- table(x$chromosome_name, x$Set)
              freq <- as.matrix(freq[which(nchar(row.names(freq)) < 50), ]) # remove unmapped chromosomes
              if (dim(freq)[2] > 1 && dim(freq)[1] > 1) { # some organisms do not have fully seuqence genome: chr. names: scaffold_99816
                Pval <- chisq.test(freq)$p.value
                sig <- paste("Distribution of query genes on chromosomes \nChi-squared test P=", formatC(Pval, digits = 2, format = "G"))

                if (Pval < PvalGeneInfo2) {
                  sig <- paste(sig, "***")
                } else
                if (Pval < PvalGeneInfo1) {
                  sig <- paste(sig, "**")
                } else
                if (Pval < PvalGeneInfo) sig <- paste(sig, "*")

                freq <- freq[order(as.numeric(row.names(freq))), ]
                freq[, 1] <- freq[, 1] * colSums(freq)[2] / colSums(freq)[1] # expected
                freq <- freq[, c(2, 1)] # reverse order
                barplot(t(freq),
                  beside = TRUE, las = 3, col = c("red", "lightgrey"), ylab = "Number of Genes", main = sig,
                  cex.lab = 1.5, cex.axis = 2, cex.names = 2, cex.main = 1.5
                )

                legend("topright", c("List", "Expected"), pch = 15, col = c("red", "lightgrey"), bty = "n", cex = 2)
              }
            } else { # Create empty plot
              plot(x = 0:1, y = 0:1, ann = F, bty = "n", type = "n", xaxt = "n", yaxt = "n")
              text(
                x = 0.5, y = 0.5, # Add text to empty plot
                "Chromosome plot not available.",
                cex = 1.8
              )
            }
            incProgress(1 / 8)


            # gene type
            if (sum(!is.na(x$gene_biotype)) >= minGenes && length(unique(x$gene_biotype)) > 2 && length(which(x$Set == "List")) > minGenes) {
              freq <- table(x$gene_biotype, x$Set)
              freq <- as.matrix(freq[which(freq[, 1] / colSums(freq)[1] > .01), ])
              if (dim(freq)[2] > 1 && dim(freq)[1] > 1) {
                Pval <- chisq.test(freq)$p.value
                sig <- paste("Distribution by gene type \nChi-squared test P=", formatC(Pval, digits = 2, format = "G"))
                if (Pval < PvalGeneInfo2) {
                  sig <- paste(sig, "***")
                } else
                if (Pval < PvalGeneInfo1) {
                  sig <- paste(sig, "**")
                } else
                if (Pval < PvalGeneInfo) sig <- paste(sig, "*")
                freq <- freq[order(freq[, 1], decreasing = T), ]
                freq[, 1] <- freq[, 1] * colSums(freq)[2] / colSums(freq)[1]
                tem <- gsub("protein_coding", "Coding", rownames(freq))
                tem <- gsub("pseudogene", "pseudo", tem)
                tem <- gsub("processed", "proc", tem)
                row.names(freq) <- tem
                par(mar = c(20, 6, 4.1, 2.1))
                freq <- freq[, c(2, 1)] # reverse order

                barplot(t(freq),
                  beside = TRUE, las = 2, col = c("red", "lightgrey"), ylab = "Number of Genes",
                  main = sig, cex.lab = 1.2, cex.axis = 1.2, cex.names = 1.2, cex.main = 1.2
                )
                legend("topright", c("List", "Expected"), pch = 15, col = c("red", "lightgrey"), bty = "n", cex = 2)
              }
            } else { # Create empty plot
              plot(x = 0:1, y = 0:1, ann = F, bty = "n", type = "n", xaxt = "n", yaxt = "n")
              text(
                x = 0.5, y = 0.5, # Add text to empty plot
                "Gene type plot not available.",
                cex = 1.8
              )
            }


            incProgress(1 / 8)
            par(mar = c(12, 6, 4.1, 2.1))
            # N. exons

            if (sum(!is.na(x2$nExons)) >= minGenes && length(unique(x2$nExons)) > 2 && length(which(x2$Set == "List")) > minGenes) {
              freq <- table(x2$nExons, x2$Set)
              freq <- as.matrix(freq[which(freq[, 1] / colSums(freq)[1] > .02), ])
              if (dim(freq)[2] > 1 && dim(freq)[1] > 1) {
                Pval <- chisq.test(freq)$p.value
                sig <- paste("Number of exons (coding genes only) \nChi-squared test P=", formatC(Pval, digits = 2, format = "G"))
                if (Pval < PvalGeneInfo2) {
                  sig <- paste(sig, "***")
                } else
                if (Pval < PvalGeneInfo1) {
                  sig <- paste(sig, "**")
                } else
                if (Pval < PvalGeneInfo) sig <- paste(sig, "*")
                # freq <- freq[order(    freq[,1], decreasing=T), ]
                freq[, 1] <- freq[, 1] * colSums(freq)[2] / colSums(freq)[1]
                freq <- freq[, c(2, 1)] # reverse order
                barplot(t(freq),
                  beside = TRUE, las = 2, col = c("red", "lightgrey"), ylab = "Number of Genes",
                  main = sig, xlab = c("Number of exons"), cex.lab = 1.5, cex.axis = 2, cex.names = 1.5, cex.main = 1.5
                )
                legend("topright", c("List", "Expected"), pch = 15, col = c("red", "lightgrey"), bty = "n", cex = 2)
              }
            } else { # Create empty plot
              plot(x = 0:1, y = 0:1, ann = F, bty = "n", type = "n", xaxt = "n", yaxt = "n")
              text(
                x = 0.5, y = 0.5, # Add text to empty plot
                "Exon plot not available.",
                cex = 1.8
              )
            }
            incProgress(1 / 8)

            # Transcript count
            if (sum(!is.na(x2$transcript_count)) >= minGenes && length(unique(x2$transcript_count)) > 2 && length(which(x2$Set == "List")) > minGenes) {
              freq <- table(x2$transcript_count, x2$Set)
              freq <- as.matrix(freq[which(freq[, 1] / colSums(freq)[1] > .02), ])
              if (dim(freq)[2] > 1 && dim(freq)[1] > 1) {
                Pval <- chisq.test(freq)$p.value
                sig <- paste("Number of transcript isoforms per coding gene \nChi-squared test P=", formatC(Pval, digits = 2, format = "G"))
                if (Pval < PvalGeneInfo2) {
                  sig <- paste(sig, "***")
                } else
                if (Pval < PvalGeneInfo1) {
                  sig <- paste(sig, "**")
                } else
                if (Pval < PvalGeneInfo) sig <- paste(sig, "*")
                freq <- freq[order(freq[, 1], decreasing = T), ]
                freq[, 1] <- freq[, 1] * colSums(freq)[2] / colSums(freq)[1]
                freq <- freq[, c(2, 1)] # reverse order
                barplot(t(freq),
                  beside = TRUE, las = 2, col = c("red", "lightgrey"), ylab = "Number of Genes",
                  main = sig, xlab = c("Number of transcripts per gene"), cex.lab = 1.5, cex.axis = 2, cex.names = 1.5, cex.main = 1.5
                )
                legend("topright", c("List", "Expected"), pch = 15, col = c("red", "lightgrey"), bty = "n", cex = 2)
              }
            } else { # Create empty plot
              plot(x = 0:1, y = 0:1, ann = F, bty = "n", type = "n", xaxt = "n", yaxt = "n")
              text(
                x = 0.5, y = 0.5, # Add text to empty plot
                "Transcript plot not available.",
                cex = 1.8
              )
            }
            incProgress(1 / 8)
          } # if minGenes
        incProgress(1 / 8, detail = paste("Done"))
        return(recordPlot())
      })
    }) # isolate
  })

  # density plots using ggplot2
  output$genePlot2 <- renderPlot(
    {
      req(input$ggplot2_theme)
      req(input$selectOrg)
      req(gene_density_plot())

      gene_density_plot()
    },
    width = 600,
    height = 2400
  )

  download_gene_plot_dist <- mod_download_images_server(
    "download_gene_plot_dist",
    filename = "gene_plot_dist",
    figure = reactive({
      ggpubr::as_ggplot(gene_density_plot())
    }),
    width = 6,
    height = 24
  )

  # density plots using ggplot2
  gene_density_plot <- reactive({
    if (input$goButton == 0) {
      return()
    }
    req(input$ggplot2_theme)
    req(input$selectOrg)
    isolate({
      withProgress(message = "Ploting gene characteristics", {
        x <- geneInfoLookup()
        x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses

        # background genes	--------------------------------------------------------------
        xB <- geneInfoLookup_background()
        convertedB <- converted_background()
        if (!is.null(xB) &&
          !is.null(convertedB)) { # if more than 30k genes, ignore background genes.

          x <- x[x$Set == "List", ] # remove background from selected genes
          xB <- xB[xB$Set == "List", ] # remove Genome genes from background
          xB$Set <- "Background"
          x <- rbind(x, xB)
          x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses
        }
        # end background genes


        if (dim(x)[1] >= minGenes) # only making plots if more than 20 genes
          { # only plot when there 10 genes or more   # some columns have too many missing values
            # par(mfrow=c(10,1))
            # par(mar=c(8,6,8,2))

            # increase fonts
            theme_set(theme_gray(base_size = 20))

            # Coding Sequence length
            if (sum(!is.na(x2$cds_length)) >= minGenes && length(unique(x2$cds_length)) > 2 &&
              length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(log(cds_length) ~ Set, data = x2)$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)


              p1 <- ggplot(x2, aes(cds_length, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                scale_x_log10() +
                labs(x = "Coding sequence length (bp)", y = "Density") +
                annotate("text", x = min(x2$cds_length) + 50, y = .5, label = sig, size = 6) +
                # annotate("text",x= max(x2$cds_length), y = densMode(x2$cds_length)$y, label=sig, size=8, hjust=1) +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p1 <- fake_plot("Coding Sequence length plot not available.")
            }

            incProgress(1 / 8)

            # Transcript length------------
            if (sum(!is.na(x2$transcript_length)) >= minGenes &&
              length(unique(x2$transcript_length)) > 2 &&
              length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(log(transcript_length) ~ Set, data = x2[which(!is.na(x2$transcript_length)), ])$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)

              p2 <- ggplot(x2, aes(transcript_length, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                scale_x_log10() +
                annotate("text", x = min(x2$transcript_length) + 100, y = .5, label = sig, size = 6) +
                # annotate("text",x= max(x2$transcript_length), y = densMode(x2$transcript_length)$y, label=sig, size=8, hjust=1) +
                labs(x = "Transcript length (bp)", y = "Density") +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p2 <- fake_plot("Transcript length plot not available.")
            }
            incProgress(2 / 8)

            # Genome span ------------

            if (sum(!is.na(x2$genomeSpan)) >= minGenes && length(unique(x2$genomeSpan)) > 2 && length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(log(genomeSpan) ~ Set, data = x2[which(!is.na(x2$genomeSpan)), ])$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)
              p3 <- ggplot(x2, aes(genomeSpan, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                scale_x_log10() +
                annotate("text", x = min(x2$genomeSpan) + 200, y = .5, label = sig, size = 6) +
                # annotate("text",x= max(x2$genomeSpan), y = densMode(x2$genomeSpan)$y, label=sig, size=8, hjust=1) +
                labs(x = "Genome span (bp)", y = "Density") +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p3 <- fake_plot("Genome span plot not available.")
            }

            incProgress(3 / 8)

            # 5' UTR ------------

            if (sum(!is.na(x2$FiveUTR)) >= minGenes && length(unique(x2$FiveUTR)) > 2 && length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(log(FiveUTR) ~ Set, data = x2[which(!is.na(x2$FiveUTR) & x2$FiveUTR > 0), ])$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)

              p4 <- ggplot(x2, aes(FiveUTR, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                scale_x_log10() +
                annotate("text",
                  x = min(x2[which(!is.na(x2$FiveUTR) & x2$FiveUTR > 0), "FiveUTR"]) + 5,
                  y = .5, label = sig, size = 6
                ) +
                # annotate("text",x= max(x2$FiveUTR), y = densMode(x2$FiveUTR)$y, label=sig, size=8, hjust=1) +
                labs(x = "5' UTR length (bp)", y = "Density") +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p4 <- fake_plot("5' UTR plot not available.")
            }

            incProgress(4 / 8)

            # 3' UTR ------------
            if (sum(!is.na(x2$ThreeUTR)) >= minGenes && length(unique(x2$ThreeUTR)) > 2 && length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(log(ThreeUTR) ~ Set, data = x2[which(!is.na(x2$ThreeUTR) & x2$ThreeUTR > 0), ])$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)

              p5 <- ggplot(x2, aes(ThreeUTR, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                scale_x_log10() +
                annotate("text", x = min(x2[which(!is.na(x2$ThreeUTR) & x2$ThreeUTR > 0), "ThreeUTR"]) + 5, y = .5, label = sig, size = 6) +
                # annotate("text",x= max(x2$ThreeUTR), y = densMode(x2$ThreeUTR)$y, label=sig, size=8, hjust=1) +
                labs(x = "3' UTR length (bp)", y = "Density") +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p5 <- fake_plot("3' UTR plot not available.")
            }
            incProgress(5 / 8)

            # GC content ------------
            # add this filter here to avoid, errors in t.test
            x2 <-  x2[which(!is.na(x2$percentage_gc_content) & x2$percentage_gc_content > 0), ]
            if (sum(!is.na(x2$percentage_gc_content)) >= minGenes &&
              length(unique(x2$percentage_gc_content)) > 2 &&
              length(which(x2$Set == "List")) > minGenes) {
              Pval <- t.test(percentage_gc_content ~ Set, data = x2)$p.value
              sig <- mark_significance(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo)

              p6 <- ggplot(x2, aes(percentage_gc_content, fill = Set, colour = Set)) +
                geom_density(alpha = 0.1) +
                # annotate("text",x= min(x2$percentage_gc_content)+5, y = .02, label=sig, size=8)+
                annotate("text", x = max(x2$percentage_gc_content), y = densMode(x2$percentage_gc_content)$y, label = sig, size = 6, hjust = 1) +
                labs(x = "GC content (%)", y = "Density") +
                guides(color = guide_legend(nrow = 2)) +
                theme(
                  legend.key = element_rect(color = NA, fill = NA),
                  legend.key.size = unit(1.2, "line")
                ) +
                theme(plot.margin = unit(c(0, 0, 1, 0), "cm"))
            } else {
              p6 <- fake_plot("GC content plot not available.")
            }

            incProgress(6 / 8)
            p1 <- refine_ggplot2(
              p = p1,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            p2 <- refine_ggplot2(
              p = p2,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            p3 <- refine_ggplot2(
              p = p3,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            p4 <- refine_ggplot2(
              p = p4,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            p5 <- refine_ggplot2(
              p = p5,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            p6 <- refine_ggplot2(
              p = p6,
              gridline = FALSE,
              ggplot2_theme = input$ggplot2_theme
            )
            incProgress(7 / 8, detail = paste("Done"))
            gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 1)
          }
      })
    }) # isolate
  })



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


  output$listSigPathways <- renderUI({
    tem <- input$selectOrg
    if (input$goButton == 0 | is.null(significantOverlaps())) {
      return(NULL)
    }

    tem <- significantOverlaps()

    if (dim(tem$x)[2] == 1) {
      return(NULL)
    }
    tem$x <- tem$x[tem$x[, 3] < 1000, ] # remove patways with more than 1000 genes.  Very slow.
    tem$x <- tem$x[order(-tem$x[, 4]), ] # sort by fold-enrichment
    choices <- tem$x[, 5]
    selectInput("sigPathways",
      label = "Select a significant KEGG pathway:",
      choices = choices
    )
  })


  output$KeggImage <- renderImage(
    {
      req(!is.null(input$sigPathways))

      # First generate a blank image. Otherwise return(NULL) gives us errors.
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 400, height = 300)

      frame()
      dev.off()
      blank <- list(
        src = outfile,
        contentType = "image/png",
        width = 400,
        height = 300,
        alt = " "
      )
      if (input$goButton == 0) {
        return(blank)
      }
      if (is.null(input$selectGO)) {
        return(blank)
      }
      if (input$selectGO != "KEGG") {
        return(blank)
      }
      if (is.null(significantOverlaps())) {
        return(blank)
      }

      isolate({
        withProgress(message = "Rendering KEGG pathway plot", {
          incProgress(1 / 5, "Downloading KEGG pathway data")


          Species <- converted()$species[1, 1]
          fold <- convertEnsembl2Entrez(converted()$IDs, Species)

          fold <- fold$entrezgene_id
          keggSpecies <- as.character(.app$keggSpeciesID[which(.app$keggSpeciesID[, 1] == Species), 3])

          if (nchar(keggSpecies) <= 2) {
            return(blank)
          } # not in KEGG

          # kegg pathway id
          incProgress(1 / 2, "Download pathway graph from KEGG.")

          # find pathway id
          # "Path:hsa04110 Cell cycle" --> "hsa04110"
          pathID <- gsub(" .*", "", input$sigPathways)
          pathID <- gsub("Path:", "", pathID)
          # cat("\nhere5  ",keggSpecies, " ",Species," ",input$sigPathways, "pathID:",pathID,"End", fold[1:5],names(fold)[1:5],"\n")
          # cat("\npathway:",is.na(input$sigPathways))
          # cat("\n",fold[1:5],"\n",keggSpecies,"\n",pathID)

          if (nchar(pathID) < 3) {
            return(blank)
          }
          randomString <- gsub(".*file", "", tempfile())
          tempFolder <- tempdir() # tempFolder = "temp";
          outfile <- paste(tempFolder, "/", pathID, ".", randomString, ".png", sep = "")

          pv.out <- mypathview(gene.data = fold, pathway.id = pathID, kegg.dir = tempFolder, out.suffix = randomString, species = keggSpecies, kegg.native = TRUE)


          # Return a list containing the filename
          list(
            src = outfile,
            contentType = "image/png",
            width = "100%",
            height = "100%",
            alt = "KEGG pathway image."
          )
        })
      })
    },
    deleteFile = TRUE
  )

  # Genome tab
  mod_genome_server(
    "genome",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    converted = converted,
    geneInfoLookup = geneInfoLookup,
    converted_background = converted_background,
    geneInfoLookup_background = geneInfoLookup_background
  )

  # Test-only hook (ignored outside shinytest2): the Chart, Tree and Plots tabs
  # render as images, so tests/testthat/test-app.R reads the plot objects (and
  # the tree's input) instead.
  exportTestValues(
    enrich_chart = enrichChartObject(),
    gene_density_plot = gene_density_plot(),
    tree_input = significantOverlaps2()
  )
}
