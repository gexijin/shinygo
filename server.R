# Main server
#
# Everything shared by the tabs: database setup, the sidebar (species picker,
# menus, messages), gene ID conversion and the enrichment results. Each tab's
# own code is in its module in R/mod_*.R; this file passes it the inputs and
# reactives it needs. Originally by Steven Ge (Xijin.Ge@sdstate.edu), Ge Lab.
#
# The shared reactives the modules use:
#   converted(), converted_background()   gene IDs from convertID()
#   geneInfoLookup(), geneInfoLookup_background()   gene annotation
#   conversionTableData()   the gene table (Genes and STRING tabs)
#   significantOverlapsAll()   FindOverlap() results, all pathways
#   significantOverlaps()      the same, filtered and sorted for display
#   significantOverlaps2()     the same, prepared for the Tree tab
# ----------------------------------------------------------------------------

server <- function(input, output, session) {
  # Setup ---------------------------------------------------------------------
  load_app_data() # database data shared by all sessions; only the first loads it

  options(warn = -1)

  # Sidebar: species, menus and messages --------------------------------------
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

  output$mapping_stats <- renderText({
    req(input$goButton)
    req(converted())
    n_genes <- length(converted()$originalIDs)
    n_mapped <- length(converted()$IDs)

    paste0(n_genes, " IDs mapped to ", n_mapped, " (", round(n_mapped / n_genes * 100, 0), "%) ", tolower(converted()$species$name2), " genes.")
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


  # Show or hide tabs ---------------------------------------------------------
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


  # Gene ID conversion and gene information -----------------------------------
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


  # Enrichment results, shared by the tabs ------------------------------------
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


  # Tabs (modules), in tab order ----------------------------------------------
  # Enrichment tab
  mod_enrichment_server(
    "enrichment",
    goButton = reactive(input$goButton),
    input_text_b = reactive(input$input_text_b),
    show_pathway_id = reactive(input$show_pathway_id),
    selectOrg = reactive(input$selectOrg),
    selectGO = reactive(input$selectGO),
    genesColumnID = reactive(input$genesColumnID),
    significantOverlaps = significantOverlaps,
    significantOverlapsAll = significantOverlapsAll
  )

  # Chart tab. Returns the chart, which the test hook below also reads.
  enrichChartObject <- mod_chart_server(
    "chart",
    goButton = reactive(input$goButton),
    maxTerms = reactive(input$maxTerms),
    selectGO = reactive(input$selectGO),
    selectOrg = reactive(input$selectOrg),
    show_pathway_id = reactive(input$show_pathway_id),
    abbreviatePathway = reactive(input$abbreviatePathway),
    ggplot2_theme = reactive(input$ggplot2_theme),
    significantOverlaps = significantOverlaps
  )

  # Tree tab
  mod_tree_server(
    "tree",
    goButton = reactive(input$goButton),
    maxTerms = reactive(input$maxTerms),
    significantOverlaps2 = significantOverlaps2
  )

  # Network tab
  mod_network_server(
    "network",
    goButton = reactive(input$goButton),
    input_text_b = reactive(input$input_text_b),
    show_pathway_id = reactive(input$show_pathway_id),
    selectOrg = reactive(input$selectOrg),
    selectGO = reactive(input$selectGO),
    significantOverlaps = significantOverlaps
  )

  # KEGG tab
  mod_kegg_server(
    "kegg",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    selectGO = reactive(input$selectGO),
    significantOverlaps = significantOverlaps,
    converted = converted
  )

  # Genes tab
  mod_genes_server(
    "genes",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    conversionTableData = conversionTableData
  )

  # Groups tab
  mod_groups_server(
    "groups",
    goButton = reactive(input$goButton),
    significantOverlaps = significantOverlaps
  )

  # Plots tab. Returns the density plots, which the test hook below also reads.
  gene_density_plot <- mod_plots_server(
    "plots",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    ggplot2_theme = reactive(input$ggplot2_theme),
    geneInfoLookup = geneInfoLookup,
    converted_background = converted_background,
    geneInfoLookup_background = geneInfoLookup_background
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

  # STRING tab
  mod_string_server(
    "string",
    goButton = reactive(input$goButton),
    selectOrg = reactive(input$selectOrg),
    minFDR = reactive(input$minFDR),
    conversionTableData = conversionTableData
  )


  # Test hook -----------------------------------------------------------------
  # Test-only hook (ignored outside shinytest2): the Chart, Tree and Plots tabs
  # render as images, so tests/testthat/test-app.R reads the plot objects (and
  # the tree's input) instead.
  exportTestValues(
    enrich_chart = enrichChartObject(),
    gene_density_plot = gene_density_plot(),
    tree_input = significantOverlaps2()
  )
}
