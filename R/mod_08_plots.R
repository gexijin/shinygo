# Plots tab module
#
# Compares the user's genes with the rest of the genome (or the background
# genes): density plots of gene length, transcript length, genome span, UTR
# lengths and GC content (t-tests), and bar plots of chromosomes, gene types
# and other categories (chi-squared tests), each with a download.
#
#   mod_plots_ui(id)      the tab
#   mod_plots_server(id, goButton, selectOrg, ggplot2_theme, geneInfoLookup,
#                    converted_background, geneInfoLookup_background)
#                         returns the density plots (a reactive gtable)
#
# Arguments of mod_plots_server(), all reactives from the main server:
#   goButton, selectOrg   input$goButton and input$selectOrg
#   ggplot2_theme         input$ggplot2_theme, the "Plot theme" menu on the
#                         Chart tab
#   geneInfoLookup        annotation of the user's genes
#   converted_background, geneInfoLookup_background
#                         the same for the background genes, if any
#
# The bar plots are drawn with base graphics inside gene_barplot_object(), and
# their chi-squared P values exist only in the plot titles.
# ----------------------------------------------------------------------------


mod_plots_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Plots",
    value = 8,
    h5("The characteristics of your genes are compared with the rest in the genome. Chi-squared and Student's
        t-tests are run to see if your genes have special characteristics when compared with all the other genes or, if uploaded, a customized background."),
    fluidRow(
      column(
        width = 4,
        mod_download_images_ui(ns("download_gene_plot_dist"), "Download density plots")
      ),
      column(
        width = 4,
        mod_download_images_ui(ns("download_gene_barplot"), "Download barplots")
      )
    ),
    br(),
    plotOutput(ns("genePlot2"), inline = TRUE, width = "auto", height = "auto"),
    plotOutput(ns("gene_barplot"), inline = TRUE, width = "auto", height = "auto")
  )
}


mod_plots_server <- function(id, goButton, selectOrg, ggplot2_theme, geneInfoLookup,
                             converted_background, geneInfoLookup_background) {
  moduleServer(id, function(input, output, session) {
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
      if (goButton() == 0) {
        return()
      }
      tem <- selectOrg()
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
        req(ggplot2_theme())
        req(selectOrg())
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
      if (goButton() == 0) {
        return()
      }
      req(ggplot2_theme())
      req(selectOrg())
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

              # Five log-scale charts, drawn the same way (density_panel() in
              # fct_plots.R). The UTR charts leave out zero lengths.
              panels <- list(
                list("cds_length", "Coding sequence length (bp)", "Coding Sequence length plot not available.", 50, FALSE),
                list("transcript_length", "Transcript length (bp)", "Transcript length plot not available.", 100, FALSE),
                list("genomeSpan", "Genome span (bp)", "Genome span plot not available.", 200, FALSE),
                list("FiveUTR", "5' UTR length (bp)", "5' UTR plot not available.", 5, TRUE),
                list("ThreeUTR", "3' UTR length (bp)", "3' UTR plot not available.", 5, TRUE)
              )
              plots <- list()
              for (k in seq_along(panels)) {
                a <- panels[[k]]
                plots[[k]] <- density_panel(x2, col = a[[1]], xlab = a[[2]], missing_text = a[[3]],
                  label_offset = a[[4]], positive_only = a[[5]])
                incProgress(k / 8)
              }

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
              plots[[6]] <- p6
              plots <- lapply(plots, refine_ggplot2, gridline = FALSE, ggplot2_theme = ggplot2_theme())
              incProgress(7 / 8, detail = paste("Done"))
              do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
            }
        })
      }) # isolate
    })

    gene_density_plot
  })
}
