#' 05_kegg UI Function
#'
#' @description Renders the KEGG tab: once a significant KEGG pathway is
#'   available, lets the user pick one from a dropdown and displays that
#'   pathway's diagram, downloaded from KEGG, with the user's genes
#'   highlighted. Prompts the user to select KEGG as the pathway database
#'   until then.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_05_kegg_ui <- function(id) {
  ns <- NS(id)
  tagList(
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


#' 05_kegg Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param significantOverlaps A reactive (defined in the main server) that
#'   returns the filtered/sorted/ranked enrichment result list; used to
#'   populate the dropdown of significant pathways.
#' @param select_org A reactive returning the value of the selected organism
#'   input; read here only to trigger recalculation of the pathway list when
#'   the user changes species.
#' @param select_go A reactive returning the value of the selected pathway
#'   database input; used to confirm "KEGG" is selected before rendering a
#'   pathway image.
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#' @param converted A reactive (defined in the main server) returning the
#'   converted IDs and matched species for the user's gene list; used to look
#'   up Entrez IDs and the KEGG species code for the pathway image.
#'
#' @noRd
mod_05_kegg_server <- function(id, significantOverlaps, select_org, select_go, go_button, converted) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dropdown of significant KEGG pathways, sorted by fold enrichment.
    output$listSigPathways <- renderUI({
      tem <- select_org() # just to make it re-calculate if user changes species
      if (go_button() == 0 || is.null(significantOverlaps())) {
        return(NULL)
      }

      tem <- significantOverlaps()
      if (dim(tem$x)[2] == 1) {
        return(NULL)
      }
      tem$x <- tem$x[tem$x[, 3] < 1000, ] # remove pathways with more than 1000 genes. Very slow.
      tem$x <- tem$x[order(-tem$x[, 4]), ] # sort by fold-enrichment
      choices <- tem$x[, 5]
      selectInput(ns("sigPathways"),
        label = "Select a significant KEGG pathway:",
        choices = choices
      )
    })

    # KEGG pathway diagram for the selected pathway, with the user's genes
    # highlighted. Downloaded from KEGG on the fly via mypathview(), so a
    # blank placeholder is returned first and whenever a guard below fails.
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

        if (go_button() == 0) {
          return(blank)
        }
        if (is.null(select_go())) {
          return(blank)
        }
        if (select_go() != "KEGG") {
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
            keggSpecies <- as.character(keggSpeciesID[which(keggSpeciesID[, 1] == Species), 3])
            if (nchar(keggSpecies) <= 2) {
              return(blank) # not in KEGG
            }

            incProgress(1 / 2, "Download pathway graph from KEGG.")

            # "Path:hsa04110 Cell cycle" --> "hsa04110"
            pathID <- gsub(" .*", "", input$sigPathways)
            pathID <- gsub("Path:", "", pathID)
            if (nchar(pathID) < 3) {
              return(blank)
            }

            randomString <- gsub(".*file", "", tempfile())
            tempFolder <- tempdir()
            outfile <- paste(tempFolder, "/", pathID, ".", randomString, ".png", sep = "")
            pv.out <- mypathview(
              gene.data = fold,
              pathway.id = pathID,
              kegg.dir = tempFolder,
              out.suffix = randomString,
              species = keggSpecies,
              kegg.native = TRUE
            )

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
