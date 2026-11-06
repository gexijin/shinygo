# KEGG tab module
#
# Draws a KEGG pathway diagram with the user's genes highlighted in red, for a
# significant KEGG pathway the user picks. The diagram is downloaded from KEGG
# (pathview), so it needs internet access and can take a few minutes.
#
#   mod_kegg_ui(id)      the tab
#   mod_kegg_server(id, goButton, selectOrg, selectGO, significantOverlaps,
#                   converted)
#
# Arguments of mod_kegg_server(), all reactives from the main server:
#   goButton             input$goButton (the Submit button)
#   selectOrg            input$selectOrg (the species ID)
#   selectGO             input$selectGO (the pathway database)
#   significantOverlaps  the enrichment results, for the pathway menu
#   converted            the user's genes, as Ensembl IDs
#
# The two conditionalPanel()s check the main app's input.selectGO, so their
# conditions are deliberately not namespaced.
# ----------------------------------------------------------------------------


mod_kegg_ui <- function(id) {
  ns <- NS(id)
  tabPanel("KEGG",
    value = 2,
    conditionalPanel(
      "input.selectGO != 'KEGG' ",
      br(), br(),
      h5("Please select KEGG from the pathway databases to conduct enrichment analysis first.
      Then you can visualize your genes on any of the significant pathways. Only for some species.")
    ),
    conditionalPanel(
      "input.selectGO == 'KEGG' ",
      br(),
      uiOutput(ns("listSigPathways")),
      br(), imageOutput(ns("KeggImage"), width = "100%", height = "100%"),
      h5("Your genes are highlighted in red. Downloading pathway diagram from KEGG can take 3 minutes. ")
    )
  )
}


mod_kegg_server <- function(id, goButton, selectOrg, selectGO, significantOverlaps, converted) {
  moduleServer(id, function(input, output, session) {
    output$listSigPathways <- renderUI({
      tem <- selectOrg()
      if (goButton() == 0 | is.null(significantOverlaps())) {
        return(NULL)
      }

      tem <- significantOverlaps()

      if (dim(tem$x)[2] == 1) {
        return(NULL)
      }
      tem$x <- tem$x[tem$x[, 3] < 1000, ] # remove patways with more than 1000 genes.  Very slow.
      tem$x <- tem$x[order(-tem$x[, 4]), ] # sort by fold-enrichment
      choices <- tem$x[, 5]
      selectInput(session$ns("sigPathways"),
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
        if (goButton() == 0) {
          return(blank)
        }
        if (is.null(selectGO())) {
          return(blank)
        }
        if (selectGO() != "KEGG") {
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
  })
}
