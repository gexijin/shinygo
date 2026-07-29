#' 09_genome UI Function
#'
#' @description Renders the Genome tab: an interactive plotly plot showing
#'   the position of the user's genes across chromosomes, with controls for
#'   labeling gene symbols, restricting to coding genes/known chromosomes,
#'   and the sliding-window size/steps and FDR cutoff used to detect
#'   chromosomal regions enriched for query genes. A "Static plot" button
#'   opens a modal with a static, per-chromosome base R version of the same
#'   plot.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_09_genome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("genomePlotly"), height = "900px"),
    fluidRow(
      column(3, checkboxInput(ns("labelGeneSymbol"), "Label genes", value = FALSE)),
      column(3, checkboxInput(ns("ignoreNonCoding"), "Coding genes only", value = TRUE)),
      column(3, checkboxInput(ns("show_all_chr"), "All chr.", value = FALSE)),
      column(3, actionButton(ns("gPlotstatic"), "Static plot"))
    ),
    fluidRow(
      column(3, selectInput(
        inputId = ns("MAwindowSize"),
        label = h5("Window Size(Mb)"),
        selected = 6,
        choices = c(1, 2, 4, 6, 8, 10, 15, 20)
      )),
      column(3, selectInput(
        inputId = ns("MAwindowSteps"),
        label = h5("Steps in a window"),
        choices = unique(1:4),
        selected = c(2)
      )),
      column(3, selectInput(
        inputId = ns("chRegionPval"),
        label = h5("FDR cutoff for windows"),
        selected = 0.00001,
        choices = c(0.1, 0.05, 0.01, 0.001, 0.0001, 0.00001, 0.000001)
      ))
    ),
    h5("The genes are represented by red dots. The purple lines indicate regions where
                  these genes are statistically enriched, compared to the density of genes in the background.
                  We scanned the genome with a sliding window. Each window is further divided into several
                  equal-sized steps for sliding. Within each window we used the hypergeometric test to
                  determine if your genes are significantly overrepresented. Essentially, the genes in
                  each window define a gene set/pathway, and we carried out enrichment analysis. The
                  chromosomes may be only partly shown as we use the last gene's location to draw the line.
                  Mouse over to see gene symbols. Zoom in regions of interest."),
    bsModal(ns("genomePlotStaticButton"), "Static genome plot", ns("gPlotstatic"),
      size = "large",
      h5("Your genes are marked in each of the chromosomes.
              Note that the scale for each chromosomes are different."),
      plotOutput(ns("genomePlot"), width = "100%")
    ) # bsModal 6
  )
}


#' 09_genome Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param geneInfoLookup A reactive (defined in the main server) returning
#'   gene characteristics, including chromosome and position, for the user's
#'   query genes.
#' @param geneInfoLookup_background A reactive (defined in the main server)
#'   returning the same gene characteristics for the user's uploaded
#'   background genes, if any.
#' @param converted_background A reactive (defined in the main server)
#'   returning the converted IDs of the background genes; used here only to
#'   check whether background genes are available.
#' @param converted A reactive (defined in the main server) returning the
#'   converted IDs and gene ID mapping table for the user's gene list; used
#'   by the static plot to look up each gene's preferred display ID.
#' @param go_button A reactive returning the value of the main "Submit"
#'   action button.
#' @param select_org A reactive returning the value of the selected organism
#'   input; read here only to trigger recalculation when the user changes
#'   species.
#' @param quotes A character vector of quotes shown while the interactive
#'   plot loads.
#'
#' @noRd
mod_09_genome_server <- function(id, geneInfoLookup, geneInfoLookup_background,
                                  converted_background, converted,
                                  go_button, select_org, quotes) {
  moduleServer(id, function(input, output, session) {

    # Static, base R plot of gene positions on each chromosome, shown in the
    # modal opened by the "Static plot" button.
    output$genomePlot <- renderPlot(
      {
        if (go_button() == 0) {
          return()
        }
        tem <- select_org()
        isolate({
          x <- geneInfoLookup()
          converted1 <- converted()
          if (dim(x)[1] == 1) {
            return(NULL)
          } # no geneInfo found for STRING species
          # for STRING species, no gene location is available
          if (sum(!is.na(x$start_position)) < 5) {
            return(p)
          }
          # chromosomes
          if ((sum(!is.na(x$chromosome_name)) >= minGenes && length(unique(x$chromosome_name)) > 2) && length(which(x$Set == "List")) > minGenes) {
            freq <- table(x$chromosome_name, x$Set)
            freq <- as.matrix(freq[which(freq[, 1] / colSums(freq)[1] > .01), ])
            if (dim(freq)[2] > 1 && dim(freq)[1] > 1 && dim(freq)[1] < 100) { # some organisms do not have fully seuqence genome: chr. names: scaffold_99816
              freq <- freq[order(as.numeric(row.names(freq))), ]

              tem <- subset(x, select = c(chromosome_name, start_position))
              chrLengthTable <- aggregate(start_position ~ chromosome_name, data = tem, max)

              allUserGenes <- x[which(x$Set == "List"), ]
              allUserGenes <- merge(allUserGenes, converted1$conversionTable, by = "ensembl_gene_id")
              allUserGenes$preferedIDs <- allUserGenes$User_input
              if (length(unique(allUserGenes$symbol)) / dim(allUserGenes)[1] > .7) allUserGenes$preferedIDs <- allUserGenes$symbol
              par(mfrow = c(dim(freq)[1], 1))
              for (i in 1:dim(freq)[1]) {
                {
                  par(mar = c(0, 0, 0, 0))
                  plot(.1, .1, axes = F, col = "white", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
                  chr <- rownames(freq)[i]
                  ix <- match(chr, chrLengthTable$chromosome_name)
                  chrLength <- chrLengthTable[ix, 2]
                  a1 <- allUserGenes[which(allUserGenes$chromosome_name == chr), ]
                  # if most of the genes have gene symbol, show gene symbol
                  a1$start_position <- a1$start_position / chrLength
                  y1 <- .50 # vertical position, from 0 - 1, relative to bottom left.
                  text(0, y1, "I")
                  text(1, y1, "I") # start and end
                  text(0, y1 + .2, paste("Chr:", chr, sep = ""), cex = 2)
                  if (chrLength > 1e6) {
                    text(1, y1 + .2, paste(round(chrLength / 1e6, 1), "Mb", sep = ""), cex = 2)
                  } else {
                    text(1, y1 + .2, paste(round(chrLength / 1e3, 0), "Kb", sep = ""), cex = 2)
                  }

                  segments(0, y1 + .01, 1, y1 + .01, col = "blue")
                  sapply(1:dim(a1)[1], function(i) text(a1$start_position[i], y1 + .03, "|"))
                  if (dim(a1)[1] < 100 && freq[i, 2] > 0) { # if more genes, do not show symbol
                    sapply(1:dim(a1)[1], function(i) text(a1$start_position[i], y1, a1$preferedIDs[i], offset = 0, srt = 90, pos = 2, cex = 1.5))
                  }
                }
              }
            }
          }
        }) # isolate
      },
      height = 3000,
      width = 1000
    )

    # Interactive plotly plot of gene positions across chromosomes. Also
    # scans the genome with a sliding window, testing each window for
    # enrichment of query genes with the hypergeometric test, and marks
    # significant windows as purple error bars.
    output$genomePlotly <- renderPlotly({
      if (go_button() == 0) {
        return()
      }
      if (is.null(geneInfoLookup)) {
        return()
      }
      # Dependency triggers: re-run this reactive whenever any of these
      # inputs change, even though most are only read further down inside
      # the isolate() block below.
      tem <- select_org()
      tem <- input$MAwindowSize
      tem <- input$MAwindowSteps
      tem <- input$MAwindowCutoff
      tem <- input$ignoreNonCoding
      tem <- input$chRegionPval
      tem <- input$labelGeneSymbol
      tem <- input$show_all_chr
      library(dplyr)

      isolate({
        withProgress(message = sample(quotes, 1), detail = "Visualzing expression on the genome", {
          # default plot
          fake <- data.frame(a = 1:3, b = 1:3)
          p <- ggplot(fake, aes(x = a, y = b)) +
            geom_blank() +
            ggtitle("Position info not available.") +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank())

          x <- geneInfoLookup()
          if (dim(x)[1] == 1) {
            return(p)
          } # no geneInfo found for STRING species
          # for STRING species, no gene location is available
          if (sum(!is.na(x$start_position)) < 5) {
            return(p)
          }

          # Background genes: if uploaded, merge them in as a separate "Set"
          # so they can be compared against the query genes below.
          xB <- geneInfoLookup_background()
          convertedB <- converted_background()
          if (!is.null(xB) &&
            !is.null(convertedB)) { # if more than 30k genes, ignore background genes.
            x <- x[x$Set == "List", ] # remove background from selected genes
            xB <- xB[xB$Set == "List", ] # remove Genome genes from background
            xB$Set <- "Background"
            x <- rbind(x, xB)
          }

          # only coding genes?
          if (input$ignoreNonCoding) {
            x <- subset(x, gene_biotype == "protein_coding")
          }

          x$Fold <- 0
          ix <- which(x$Set == "List")
          x$Fold[ix] <- 1

          incProgress(0.1)
          # if no chromosomes found. For example if user do not convert gene IDs.
          if (dim(x)[1] > 5) {
            x <- x[order(x$chromosome_name, x$start_position), ]

            x$ensembl_gene_id <- as.character(x$ensembl_gene_id)

            # if symbol is missing use Ensembl id
            x$symbol <- as.character(x$symbol)
            ix <- which(is.na(x$symbol))
            ix2 <- which(nchar(as.character(x$symbol)) <= 2)
            ix3 <- which(duplicated(x$symbol))
            ix <- unique(c(ix, ix2, ix3))
            x$symbol[ix] <- x$ensembl_gene_id[ix]

            x <- x[!is.na(x$chromosome_name), ]
            x <- x[!is.na(x$start_position), ]

            # median # of characters in chr. names
            medean_nchars <- median(nchar(x$chromosome_name))

            tem <- sort(table(x$chromosome_name), decreasing = T)
            ch <- names(tem[tem >= 1]) # ch with less than 100 genes are excluded

            if (length(ch) > 50) ch <- ch[1:50] # at most 50 ch
            # hide chrs with extra long names:  "CHR_HSCHR19_5_CTG2"
            if (!input$show_all_chr) {
              ch <- ch[nchar(ch) <= 3 * medean_nchars + 1]
            }
            ch <- ch[order(as.numeric(ch))]
            tem <- ch
            ch <- 1:(length(ch)) # the numbers are continous from 1 to length(ch)
            names(ch) <- tem # the names are real chr. names

            x <- x[which(x$chromosome_name %in% names(ch)), ]
            x <- droplevels(x)

            x$chNum <- 1 # numeric encoding
            x$chNum <- ch[x$chromosome_name]

            # add chr. numer
            # use max position as chr. length   before filtering
            chLengthTable <- aggregate(start_position ~ chromosome_name, data = x, max)
            chLengthTable$chNum <- ch[chLengthTable$chromosome_name]
            chLengthTable <- chLengthTable[!is.na(chLengthTable$chNum), ]
            chLengthTable <- chLengthTable[order(chLengthTable$chNum), c(3, 2)]
            chLengthTable <- chLengthTable[order(chLengthTable$chNum), ]
            chLengthTable$start_position <- chLengthTable$start_position / 1e6

            chTotal <- dim(chLengthTable)[1]
            x0 <- x # keep a copy
            x <- subset(x, Set == "List")
            if (dim(x)[1] > 5) {

              # prepare coordinates
              x$start_position <- x$start_position / 1000000 # Mbp
              chD <- 30 # distance between chs.

              # y is scalled and also jittered with random number to avoid overlap
              x$y <- x$chNum * chD + 4 + runif(dim(x)[1]) * 6

              colnames(x)[which(colnames(x) == "start_position")] <- "x"

              incProgress(0.3)

              # plotting
              p <- ggplot() + # don't define x and y, so that we could plot use two datasets
                geom_point(
                  data = x, aes(x = x, y = y, text = symbol),
                  colour = "red", shape = 20, size = .3
                )
              if (input$labelGeneSymbol) {
                p <- p + geom_text(
                  data = x, aes(x = x, y = y, label = symbol),
                  check_overlap = FALSE, angle = 45, size = 2, vjust = 0, nudge_y = 4
                )
              }

              # label y with ch names
              p <- p + scale_y_continuous(
                labels = paste("chr", names(ch[chLengthTable$chNum]), sep = ""),
                breaks = chD * (1:chTotal),
                limits = c(0, chD * (chTotal + 1) + 5)
              )
              # draw horizontal lines for each ch.
              for (i in 1:dim(chLengthTable)[1]) {
                p <- p + annotate("segment",
                  x = 0, xend = chLengthTable$start_position[i],
                  y = chLengthTable$chNum[i] * chD, yend = chLengthTable$chNum[i] * chD
                )
              }

              p <- p + xlab("Position on chrs. (Mbp)") + theme(axis.title.y = element_blank())
              p <- p + theme(legend.position = "none")

              incProgress(0.5)

              # Enriched regions: slide a window (in `steps` overlapping
              # increments) across each chromosome and test each window for
              # enrichment of query genes with the hypergeometric test.
              x0 <- x0[x0$chromosome_name %in% unique(x$chromosome_name), ]
              x0$chNum <- 1 # numeric encoding
              x0$chNum <- ch[x0$chromosome_name]
              x0$start_position <- x0$start_position / 1e6 # Mbp

              windowSize <- as.numeric(input$MAwindowSize) # Mb
              steps <- as.numeric(input$MAwindowSteps) # step size is then windowSize / steps
              cutoff <- as.numeric(input$MAwindowCutoff)

              totalN <- dim(x0)[1] # total genes
              listN <- dim(subset(x0, Set == "List"))[1] # genes in list

              for (i in 0:(steps - 1)) {
                # step size is  windowSize/steps
                # If windowSize=10 and steps = 2; then step size is 5Mb
                # 1.3 becomes 5, 11.2 -> 15 for step 1
                # 1.3 -> -5
                x0$x <- (floor((x0$start_position - i * windowSize / steps) / windowSize) +
                  0.5 + i / steps) * windowSize

                movingAverage1 <- x0 %>%
                  select(chNum, x, Fold) %>%
                  filter(x >= 0) %>% # beginning bin can be negative for first bin in the 2nd step
                  group_by(chNum, x) %>%
                  summarize(n = n(), k = sum(Fold)) %>%
                  filter(k > 0) %>%
                  filter(k / n > listN / totalN) %>%
                  mutate(pval = phyper(k - 1,
                    n,
                    totalN - n,
                    listN,
                    lower.tail = FALSE
                  ))

                if (i == 0) {
                  movingAverage <- movingAverage1
                } else {
                  movingAverage <- rbind(movingAverage, movingAverage1)
                }
              }

              # translate fold to y coordinates
              movingAverage <- movingAverage %>%
                filter(n >= 3) %>%
                mutate(pval = p.adjust(pval, method = "fdr")) %>%
                filter(pval < as.numeric(input$chRegionPval)) %>%
                mutate(y = chNum * chD - 4)

              # significant regions are marked as horizontal error bars
              if (dim(movingAverage)[1] > 0) {
                p <- p +
                  geom_errorbarh(
                    data = movingAverage, aes(
                      x = x,
                      y = y,
                      xmin = x - windowSize / 2,
                      xmax = x + windowSize / 2
                    ),
                    size = 2,
                    height = 15,
                    colour = "purple"
                  )

                # label significant regions
                sigCh <- sort(table(movingAverage$chNum), decreasing = TRUE)
                sigCh <- names(ch)[as.numeric(names(sigCh))]
                if (length(sigCh) <= 5) { # more than 5 just show 5
                  sigCh <- paste0("chr", sigCh, collapse = ", ")
                } else {
                  sigCh <- sigCh[1:5]
                  sigCh <- paste0("chr", sigCh, collapse = ", ")
                  sigCh <- paste0(sigCh, ", ...")
                }

                sigCh <- paste(
                  dim(movingAverage)[1],
                  " enriched regions \n(",
                  round(sum(chLengthTable$start_position) / windowSize * steps * as.numeric(input$chRegionPval), 2),
                  " expected)  detected on:\n ", sigCh
                )

                p <- p + annotate(
                  geom = "text",
                  x = max(x$x) * 0.70,
                  y = max(x$y) * 0.90,
                  label = sigCh
                )
              }
            } # have genes after filter
          } # have 5+ genes to begin with
          incProgress(1)
          ggplotly(p)
        }) # progress
      }) # isolate
    })
  })
}
