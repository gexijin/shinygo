####################################################
# Author: Steven Ge Xijin.Ge@sdstate.edu
# Lab: Ge Lab
# R version 4.0.5
# Project: ShinyGO
# File: mod_plots.R
# Purpose of file: Module for the Plots tab functionality
# Start data: NA (mm-dd-yyyy)
# Data last modified: 04-26-2025
#######################################################
library(shiny)
library(ggplot2)
library(gridExtra)

#' Plots UI Function
#'
#' @description A shiny Module for the Plots tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plots_ui <- function(id) {
  ns <- NS(id)
  tagList(
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

#' Plots Server Function
#'
#' @description Server logic for the Plots tab.
#'
#' @param id Module ID
#' @param geneInfoLookup Reactive function that returns gene info data
#' @param converted Reactive function that returns converted gene IDs
#' @param geneInfoLookup_background Reactive function that returns background gene info
#' @param converted_background Reactive function that returns converted background genes
#' @param input_goButton Reactive function that returns the value of the goButton
#' @param input_ggplot2_theme Reactive function that returns the ggplot2 theme choice
#' @param PvalGeneInfo P-value threshold for significance (*)
#' @param PvalGeneInfo1 P-value threshold for significance (**)
#' @param PvalGeneInfo2 P-value threshold for significance (***)
#' @param minGenes Minimum number of genes for plotting
#'
#' @noRd
mod_plots_server <- function(id, 
                            geneInfoLookup, 
                            converted, 
                            geneInfoLookup_background, 
                            converted_background,
                            input_goButton,
                            input_ggplot2_theme,
                            PvalGeneInfo = 0.05,
                            PvalGeneInfo1 = 0.01,
                            PvalGeneInfo2 = 0.001,
                            minGenes = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # density plots using ggplot2
    output$genePlot2 <- renderPlot(
      {
        req(input_ggplot2_theme())
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

    output$gene_barplot <- renderPlot(
      {
        gene_barplot_object()
      },
      width = 600,
      height = 1500
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

    # barplots using R base graphics
    gene_barplot_object <- reactive({
      if (input_goButton() == 0) {
        return()
      }
      
      isolate({
        withProgress(message = "Plotting gene characteristics", {
          x <- geneInfoLookup()
          x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses

          # background genes --------------------------------------------------------------
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

          if (dim(x)[1] >= minGenes) # only making plots if more than minimum genes
            { # only plot when there are enough genes   # some columns have too many missing values
              par(mfrow = c(4, 1))
              par(mar = c(8, 6, 8, 2))
              # chromosomes
              if (sum(!is.na(x$chromosome_name)) >= minGenes && length(unique(x$chromosome_name)) > 2 && length(which(x$Set == "List")) > minGenes) {
                freq <- table(x$chromosome_name, x$Set)
                freq <- as.matrix(freq[which(nchar(row.names(freq)) < 50), ]) # remove unmapped chromosomes
                if (dim(freq)[2] > 1 && dim(freq)[1] > 1) { # some organisms do not have fully sequenced genome: chr. names: scaffold_99816
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
    gene_density_plot <- reactive({
      if (input_goButton() == 0) {
        return()
      }
      req(input_ggplot2_theme())
      
      isolate({
        withProgress(message = "Plotting gene characteristics", {
          x <- geneInfoLookup()
          x2 <- x[which(x$gene_biotype == "protein_coding"), ] # only coding for some analyses

          # background genes --------------------------------------------------------------
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
              if (sum(!is.na(x2$percentage_gc_content)) >= minGenes &&
                length(unique(x2$percentage_gc_content)) > 2 &&
                length(which(x2$Set == "List")) > minGenes) {
                Pval <- t.test(percentage_gc_content ~ Set,
                  data = x2[which(!is.na(x2$percentage_gc_content) & x2$percentage_gc_content > 0), ]
                )$p.value
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
                ggplot2_theme = input_ggplot2_theme()
              )
              p2 <- refine_ggplot2(
                p = p2,
                gridline = FALSE,
                ggplot2_theme = input_ggplot2_theme()
              )
              p3 <- refine_ggplot2(
                p = p3,
                gridline = FALSE,
                ggplot2_theme = input_ggplot2_theme()
              )
              p4 <- refine_ggplot2(
                p = p4,
                gridline = FALSE,
                ggplot2_theme = input_ggplot2_theme()
              )
              p5 <- refine_ggplot2(
                p = p5,
                gridline = FALSE,
                ggplot2_theme = input_ggplot2_theme()
              )
              p6 <- refine_ggplot2(
                p = p6,
                gridline = FALSE,
                ggplot2_theme = input_ggplot2_theme()
              )
              incProgress(7 / 8, detail = paste("Done"))
              gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 1)
            }
        })
      }) # isolate
    })
  })
}

# find peak values in density plots
# for adding annotation texts
# http://ianmadd.github.io/pages/PeakDensityDistribution.html
densMode <- function(x) {
  td <- density(x, na.rm = TRUE)
  maxDens <- which.max(td$y)
  list(x = td$x[maxDens], y = td$y[maxDens])
}

# 0.000234   <- 2.3E-4 *
mark_significance <- function(Pval, PvalGeneInfo2, PvalGeneInfo1, PvalGeneInfo) {
  sig <- paste("P=", formatC(Pval, digits = 2, format = "G"), sep = "")
  if (Pval < PvalGeneInfo2) {
    sig <- paste(sig, "***")
  } else
  if (Pval < PvalGeneInfo1) {
    sig <- paste(sig, "**")
  } else
  if (Pval < PvalGeneInfo) sig <- paste(sig, "*")
  return(sig)
}

# generates a fake ggplot2, with some message like: "Not available."
fake_plot <- function(some_text) {
  p <- ggplot2::ggplot() +
    geom_point() +
    xlim(-10, 10) +
    ylim(-10, 10) +
    annotate("text",
      x = 0,
      y = 0,
      label = some_text
    ) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  return(p)
}

#' Change ggplot2 plots
#'
#' @param p ggplot2 object
#' @param gridline TRUE of FALSE
#' @param ggplot2_theme Theme name
#'
#' @return ggplot2 object
refine_ggplot2 <- function(p, gridline, ggplot2_theme = "light") {

  # apply theme based on selection
  p <- switch(ggplot2_theme,
    "linedraw" = p + ggplot2::theme_linedraw(),
    "classic" = p + ggplot2::theme_classic(),
    "gray" = p + ggplot2::theme_gray(),
    "light" = p + ggplot2::theme_light(),
    "dark" = p + ggplot2::theme_dark(),
    "bw" = p + ggplot2::theme_bw(),
    p # default, no change
  )

  if (ggplot2_theme != "Add grid") { # keep grid
    if (!gridline) { # by default it has gridlines
      p <- p +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }
  }

  return(p)
}