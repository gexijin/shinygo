###################################################
# Author: Steven Ge Xijin.Ge@sdstate.edu
# Lab: Ge Lab
# R version 4.1
# Project: ShinyGO
# File: ui.R
# Purpose of file:ui logic of app
# Start data: NA (mm-dd-yyyy)
# Data last modified: 04-8-2022
#######################################################
library(shiny, verbose = FALSE)
library(shinyBS, verbose = FALSE) # for popup figures
library(plotly) # interactive network plot
library(visNetwork)
library("reactable", verbose = FALSE)
columnSelection <- list(
  "-log10(FDR)" = "EnrichmentFDR",
  "Fold Enrichment" = "FoldEnrichment",
  "Genes" = "nGenes",
  "Category Name" = "Pathway"
)

ui <- fluidPage(
  # reduce the space between label and widgets, globally
  tags$head(
    tags$style(HTML(
      "label { font-size:100%; font-family:Times New Roman; margin-bottom:-15px; }"
    ))
  ),
  shinybusy::add_busy_spinner(spin = "fading-circle"), # add spinner
  sidebarLayout(
    sidebarPanel(
      titlePanel("ShinyGO 0.82",
        tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
             tags$title("ShinyGO 0.82"))
      ),
      # use conditional panel to hide the selectOrg input
      conditionalPanel(
        condition = "0", # hide the selectOrg input, always
        selectInput(
          inputId = "selectOrg",
          label = NULL,
          selectize = TRUE,
          choices = setNames(99, "Human"), # Human is selected by default
          selected = setNames(99, "Human")
        )
      ),
      fluidRow(
        column(
          width = 6,
          textOutput("selected_species")
        ),
        column(
          width = 6,
          align = "left",
          # hide the change species button, once submit button is clicked. 
          # this avoids errors where some analyses are not updated when user changes species in the middle of an analysis
          conditionalPanel(
            condition = "input.goButton == 0", 
          # Species list and genome assemblies ----------
            actionButton(
              inputId = "genome_assembl_button",
              label = strong("Change species")
            ),
            tippy::tippy_this(
              "genome_assembl_button",
              "Search and click a row to select a species. ",
              theme = "light-border"
            )
          )
        )
      ),
      tags$head(tags$style("#selected_species{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }")),
      br(),
      fluidRow(
        column(8, 
          conditionalPanel(
            condition = "input.goButton == 0", 
            actionButton("useDemo1", "Demo genes"), 
          )
        ),
        # column(4,   actionButton("useDemo2", "Demo 2"),	  	  ),
        column(4, p(HTML("<div align=\"right\"> <A HREF=\"javascript:history.go(0)\">Reset</A></div>")))
      ),
      tags$style(type = "text/css", "textarea {width:100%}"),
      tags$textarea(
        id = "input_text", placeholder = "Change the species if it is not human. Then just paste a list of genes and click Submit. Gene IDs can be NCBI, Ensembl, symbol, or other common types.",
        rows = 8, ""
      ),
      conditionalPanel(
        condition = "input.goButton != 0", 
        textOutput("mapping_stats") 
      ),
      fluidRow(
        column(8, actionButton("backgroundGenes", "Background (recommended)")),
        column(4, actionButton("goButton", strong("Submit")))
      ),
      br(),
      htmlOutput("selectGO1"),
      fluidRow(
        column(
          6,
          numericInput(
            inputId = "minFDR",
            label = h5("FDR cutoff"),
            value = 0.05, step = 0.01
          ),
          tippy::tippy_this(
            "minFDR",
            "Minimum  P-value, ajusted using the FDR (false discovery rate)
          method. P-value is derived from hypergeometric distribution.
           Really significant FDR are between 1E-5 to 1E-20. Be cautious
           when you get an FDR of 1E-2 or 1E-3, as thousands of
           gene sets are tested.",
            theme = "light-border"
          )
        ),
        column(
          6,
          selectInput("maxTerms", h5("# pathways to show"),
            choices = list(
              "10" = 10,
              "15" = 15,
              "20" = 20,
              "25" = 25,
              "30" = 30,
              "40" = 40,
              "50" = 50,
              "60" = 60,
              "80" = 80,
              "100" = 100,
              "200" = 200,
              "500" = 500
            ),
            selected = "20",
            selectize = FALSE
          ),
          tippy::tippy_this(
            "maxTerms",
            "How many top pathways to show.
            You can download nearly all significant ones.
            We typically recommend focusing on the top 10 to 20 pathways.
            If you go down the list,
            you can always find the one that help you tell the story you
            want to tell.",
            theme = "light-border"
          )
        )
      ),
      # tags$style(type='text/css', "#minFDR { width:100%;   margin-top:-15px}"),
      # selectInput("selectOrg", label = NULL,"Best matching species",width='100%'),


      fluidRow(
        column(
          width = 6,
          numericInput("minSetSize",
            label = h5("Pathway size: Min."),
            min   = 2,
            max   = 30,
            value = 2,
            step  = 1
          ),
          tippy::tippy_this(
            "minSetSize",
            "Smaller pathways can introduce noise. Generally safe to incrase to 10 or 15.
            It is automatically raised to 10 when \"Sort by Fold Enrichment \" is selected.",
            theme = "light-border"
          )
        ),
        column(
          width = 6,
          numericInput("maxSetSize",
            label = h5("Max."),
            min   = 1000,
            max   = 20000,
            value = 5000,
            step  = 200
          ),
          tippy::tippy_this(
            "maxSetSize",
            "Big gene sets, such as those associated with top-level GO term
            \"Cellular Process\", are less informative, but
            tend to have small P values due to increased power.",
            theme = "light-border"
          )
        )
      ), # fluidRow
      # tags$style(type='text/css', "#minSetSize { width:100%;   margin-top:-12px}"),
      # tags$style(type='text/css', "#maxSetSize { width:100%;   margin-top:-12px}"),
      fluidRow(
        column(
          width = 6,
          checkboxInput(
            "removeRedudantSets",
            "Remove redundancy",
            value = TRUE
          ),
          tippy::tippy_this(
            "removeRedudantSets",
            "Similar pathways sharing 95% of genes are represented by the most significant pathway.",
            theme = "light-border"
          )
        ),
        column(
          width = 6,
          checkboxInput(
            "abbreviatePathway",
            "Abbreviate pathways",
            value = TRUE
          ),
          tippy::tippy_this(
            "abbreviatePathway",
            "Positive regulation --> Pos. reg.",
            theme = "light-border"
          )
        )
      ), # fluidRow

      fluidRow(
        column(
          width = 6,
          checkboxInput(
            "gene_count_pathwaydb",
            "Use pathway DB for gene counts",
            value = FALSE
          ),
          tippy::tippy_this(
            "gene_count_pathwaydb",
            "If turned on, a gene must match at least one pathway in the selected pathway database.
            Otherwise, this gene is ignored when calculating enrichment. Be cautious
            when the selected pathway database is small, such as KEGG. ",
            theme = "light-border"
          )
        ),
        column(
          width = 6,
          checkboxInput(
            inputId = "show_pathway_id",
            label = "Show pathway IDs",
            value = FALSE
          ),
          tippy::tippy_this(
            "show_pathway_id",
            "If selected, pathway IDs, such as Path:mmu04115 and GO:0042770,  will be appended to pathway name.",
            theme = "light-border"
          )
        )
      ), # fluidRow

      actionButton("MGeneIDexamples", "Gene IDs examples"),
      tippy::tippy_this(
        "MGeneIDexamples",
        "Show some example gene IDs in our database for a specific species.",
        theme = "light-border"
      ),
      h5("Try ", a(" iDEP", href = "https://bioinformatics.sdstate.edu/idep/", target = "_blank"), "for RNA-Seq data analysis"),
      #tableOutput("species")
    ), # sidebarPanel

    mainPanel(
      tabsetPanel(
        id = "tabs", type = "tabs",
        tabPanel("Enrichment",
          value = 1,
          conditionalPanel(
            "input.goButton == 0 ", # welcome screen
            br(),
            fluidRow(
              column(
                width = 9,
                h4("ShinyGO: a graphical gene-set enrichment tool for animals and plants")
              ),
              column(
                width = 3,
                img(
                  src = "shinygo_logo.png",
                  width = "43",
                  height = "50"
                )
              )
            ),
            p("2/3/25: v.0.82. Fix issues caused by multiple ENSEMBL IDs for the same gene on patched chromosomes, causing inaccurate enrichment results. Duplicated ENSEMBL IDs are now ignored. "),
            p("You can still use the old versions using links on the About tab.", 
              "To support this effort, please cite our paper, like ",
                a("over 2000 users did.", href = "https://scholar.google.com/scholar?oi=bibs&hl=en&cites=4205886424733220184&as_sdt=5"),
                "Just including URL is not enough.",
              a("Email Jenny ", href = "mailto:gelabinfo@gmail.com?Subject=ShinyGO"),
              "(gelabinfo@gmail.com) for questions, suggestions or data contributions.",
              "Follow Dr Ge on ", a("Twitter", href = "https://twitter.com/StevenXGe"), " and ",
              a("LinkedIn", href = "https://www.linkedin.com/in/steven-ge-ab016947/", target = "_blank"),
              " for updates. "
            ),
            p("Feb. 11, 2022: Like ShinyGO but your genome is not covered?",
              a("Customized ShinyGO", href = "http://bioinformatics.sdstate.edu/goc/"), " is now available.
                    Its database includes several custom genomes requested by users. To request to add a new species/genome, fill in this ",
              a("Form.", href = "https://forms.gle/zLtLnqxkW187AgT76")
            ),            
            h3("For-profit organizations: contact us for licensing, local installation, or customization services.", style="color: red;"),
            br(),

            h3("GO Enrichment analysis, plus a lot more!"),
            p("Just paste your gene list to get enriched GO terms and othe pathways for over 14,000 species.
				    based on annotation from Ensembl and STRING-db. Produce
				    KEGG pathway diagrams with your genes highlighted, hierarchical clustering trees and networks summarizing
				    overlapping terms/pathways, protein-protein interaction networks, gene characterristics plots, and enriched promoter motifs.
            "),
            br(), img(src = "enrich.png", align = "center", width = "660", height = "339"),
            br(), img(src = "enrichmentChart.png", align = "center", width = "700", height = "400"),
            br(), br(), img(src = "KEGG2.png", align = "center", width = "541", height = "360"),
            br(), br(), img(src = "GOtree3.png", align = "center", width = "500", height = "258"),
            br(), br(), img(src = "GOnetwork2.png", align = "center", width = "500", height = "248"),
            br(), br(), img(src = "PPInetwork2.png", align = "center", width = "500", height = "391"),
            br(), br(), img(src = "chr.png", align = "center", width = "444", height = "338"),
            br(), br(), img(src = "downSyndrome.png", align = "center", width = "371", height = "276"),
            #br(), br(), img(src = "promoter.png", align = "center", width = "717", height = "288")
          ),
          br(),
          conditionalPanel(
            "input.goButton != 0",
            div(
              style = "display:inline-block",
              selectInput(
                inputId = "SortPathways",
                label = NULL,
                choices = c(
                  "Sort by FDR" = "Sort by FDR",
                  "Sort by Fold Enrichment" = "Sort by Fold Enrichment",
                  "Sort by average ranks(FDR & Fold)" = "Sort by FDR & Fold Enrichment",
                  "Select by FDR, sort by Fold Enrichment" = "Select by FDR, sort by Fold Enrichment",
                  "Sort by Genes" = "Sort by Genes",
                  "Sort by Category Name" = "Sort by Category Name"
                ),
                selected = "Select by FDR, sort by Fold Enrichment"
              ),
              style = "algn:right"
            )
          ),
          tableOutput("EnrichmentTable"),
          conditionalPanel(
            "input.goButton != 0",
            downloadButton("downloadEnrichment", "Top Pathways shown above"),
            downloadButton("downloadEnrichmentAll", "Results on all Pathways"),
            br(), br(),
            h3("Methods"),
            p("All query genes are converted to ENSEMBL gene IDs or STRING-db protein IDs,
              as our gene ID mapping and pathway data primarily come from these sources. 
              For model organisms, we manually compile extensive pathway lists from various
              public databases."),

            p("P-values are calculated using the hypergeometric test, and false discovery 
              rates (FDRs) are computed via the Benjamini-Hochberg method to correct for 
              multiple testing. Fold enrichment is defined as the percentage of genes in 
              your list that are in a pathway divided by the corresponding percentage in 
              the background genes. While FDR measures statistical significance, fold 
              enrichment indicates effect size."),

            p("We recommend that users provide their own list of background genes, which 
              could include all genes detected in an experiment, such as genes with probes 
              on a DNA microarray, passed a minimal filter in RNA-seq analysis, or detected 
              in a proteomics experiment. If no background genes are uploaded, the default
              is to use all protein-coding genes. Alternatively, you may select the option
              'Use pathway DB for gene counts,' which calculates the background based on
              the total number of unique genes in the chosen pathway database, limited
              between 5,000 and 30,000 genes. When this option is selected, any genes in 
              the user's original list that are not in the pathway database are excluded."),

            p("Only pathways within specified size limits, as defined by the 
            'Pathway size: (Min, Max)' settings, are considered. Results for smaller
            pathways can be noisy, but some pathways or GO terms have only a few genes. 
            After analysis, pathways are filtered by a user-defined FDR cutoff. Significant 
            pathways are then sorted in different ways, and only the top-ranked are shown 
            in the table above.

            By default, 'Select by FDR, then by Fold Enrichment' is used, where pathways
            are first filtered and sorted by FDR, and then the top 20 are sorted by fold 
            enrichment. In other words, the default setting shows the top 20 most 
            significant pathways ranked by fold enrichment. When the 'Sort by average 
            ranks (FDR & fold enrichment)' option is selected, pathways are sorted by 
            the average of their ranks based on both FDR and fold enrichment. When 
            'Sort by FDR' is selected, pathways are ranked by FDR and only the top 20 are shown.

            The 'Remove redundancy' option eliminates similar pathways that share 95% of 
              their genes and 50% of the words in their names, representing them with the
              pathway that has the highest significance."),

            h3("Interpreting GO Enrichment Results"),

            p("The Gene Ontology (GO) includes tens of thousands of terms (functional 
              categories), each tested individually for enrichment. Hundreds or even 
              thousands of GO terms can be statistically significant. These terms are 
              filtered, ranked, and only the top ones are displayed. Understanding this
              process is crucial for interpreting GO enrichment results."),

            tags$ul(
              tags$li("P-value: Reflects the statistical significance of the enrichment.
                      Lower values suggest a lower likelihood of the result occurring by
                      chance under the null hypothesis. FDR q-values adjust P-values for
                      multiple testing to control the proportion of type I errors."),
              tags$li("Fold Enrichment: Measures the magnitude of enrichment. Higher values
                      indicate stronger enrichment and are an important metric of effect size."),
              tags$li("Pathway Genes: The total number of genes in a pathway or GO term."),
              tags$li("nGenes: The number of genes in the pathway that overlap with your gene list.")
            ),

            tags$p("Exercise caution when interpreting FDR values of 0.01 or 0.001 for GO 
                  terms, as these levels often represent noise due to the vast number of 
                  terms tested. For a gene list of reasonable size, more significant results 
                  (FDR < 1E-5) are expected."),

            tags$p("Large pathways, such as the cell cycle, often show smaller FDRs due to 
                  increased statistical power, while smaller pathways might have higher 
                  FDRs despite their biological relevance. Enrichment analysis tends to 
                  favor larger pathways."),

            tags$p("With a default cutoff of FDR < 0.05, thousands of significant GO terms
                  may be detected, though only a subset is shown. Therefore, the method of
                  filtering and ranking these terms is crucial."),

            tags$p("With large sample sizes, small differences can appear extremely 
                  significant. In addition to FDR, fold enrichment should also be 
                  considered when prioritizing pathways, as it reflects the strength 
                  of the enrichment. We offer several methods that consider both FDR 
                  q-values and fold enrichment."),

            tags$p("Many GO terms are closely related (e.g., 'Cell Cycle', 'Regulation of 
                  Cell Cycle') and can dominate the top 20, obscuring other pathways. To 
                  avoid this, consider examining the top 50 terms. Additionally, use tree
                  plots and network plots to identify clusters of related GO terms and 
                  uncover overarching themes."),

            tags$p("Discuss the most significant pathways first, even if they do not fit 
                  your initial expectations."),
          )
        ), # enrichment tab

        #---Enrichment Chart-----------------------------------------------------------
        tabPanel("Chart",
          value = 3,
          plotOutput("enrichChart", width = "100%", height = "100%"),
          fluidRow(
            column(3, selectInput(
              inputId = "SortPathwaysPlot",
              label = h5("Sort Pathway by"),
              choices = columnSelection,
              selected = columnSelection[2]
            )),
            column(3, selectInput(
              inputId = "SortPathwaysPlotX",
              label = h5("x-axis"),
              choices = columnSelection[1:3],
              selected = columnSelection[2]
            )),
            column(3, selectInput(
              inputId = "SortPathwaysPlotColor",
              label = h5("Color"),
              choices = columnSelection[1:3],
              selected = columnSelection[1]
            )),
            column(3, selectInput(
              inputId = "SortPathwaysPlotSize",
              label = h5("Size"),
              choices = columnSelection[1:3],
              selected = columnSelection[3]
            ))
          ), # first row

          fluidRow(
            column(3, numericInput(
              inputId = "SortPathwaysPlotFontSize",
              label = h5("Font Size"),
              value = 12,
              min = 3,
              max = 18,
              step = 1
            )),
            column(3, numericInput(
              inputId = "SortPathwaysPlotMarkerSize",
              label = h5("Circle Size"),
              value = 4,
              min = 0,
              max = 10,
              step = 1
            )),
            column(3, selectInput(
              inputId = "SortPathwaysPlotHighColor",
              label = h5("Color:High"),
              choices = c("red", "orange", "yellow", "green", "blue", "purple"),
              selected = "red"
            )),
            column(3, selectInput(
              inputId = "SortPathwaysPlotLowColor",
              label = h5("Color:Low"),
              choices = c("red", "orange", "yellow", "green", "blue", "purple"),
              selected = "blue"
            ))
          ), # 2nd row

          fluidRow(
            column(width = 3, selectInput(
              inputId = "enrichChartType",
              label = h5("Chart type"),
              choices = c("lollipop", "dotplot", "barplot", "barplot_inside"),
              selected = "lollipop"
            )),
            column(3, selectInput(
              inputId = "enrichChartAspectRatio",
              label = h5("Aspect Ratio"),
              choices = .1 * (5:30),
              selected = 2
            )),
            column(
              width = 3,
              selectInput(
                inputId = "ggplot2_theme",
                label = h5("Plot theme:"),
                choices = c(
                  "default", # no change
                  "gray",
                  "bw",
                  "light",
                  "dark",
                  "classic",
                  "minimal",
                  "linedraw",
                  "Add grid"
                ),
                selected = "default",
                selectize = FALSE
              ),
              tippy::tippy_this(
                "ggplot2_theme",
                "Changes the ggplot2 theme for all plots, including those in the Plots tab.",
                theme = "light-border"
              )
            ),
            column(3, style = "margin-top: 25px;", mod_download_images_ui("download_barplot"))
          ) # 3rd row
        ),

        #---Tree-----------------------------------------------------------
        tabPanel("Tree",
          value = 4,
          h5("A hierarchical clustering tree summarizes the correlation among significant pathways
                      listed in the Enrichment tab. Pathways with many shared genes are clustered together.
                        Bigger dots indicate more significant P-values. The width of the plot can be
                        changed by adjusting the width of your browser window."),
          fluidRow(
            column(width = 3, selectInput(
              inputId = "treeChartAspectRatio",
              label = h5("Aspect Ratio"),
              choices = .1 * (5:40),
              selected = 2
            )),
            column(3, style = "margin-top: 25px;", mod_download_images_ui("download_tree", label = "Download"))
          ),
          plotOutput("GOTermsTree")
        ),

        #---Enrichment network-------------------------------------------------------
        tabPanel("Network",
          value = 5,
          fluidRow(
            column(2, actionButton("layoutButton", "Change layout")),
            column(2, actionButton("GONetwork", "Static plot")),
            column(2, h5("Edge cutoff:"), align = "left"),
            column(2, numericInput("edgeCutoff", label = NULL, value = 0.30, min = 0, max = 1, step = .1), align = "right"),
            column(2, checkboxInput("wrapTextNetwork", "Wrap text", value = TRUE))
          ),
          visNetworkOutput("enrichmentNetworkPlotInteractive", height = "800px", width = "800px"),
          downloadButton("enrichmentNetworkPlotInteractiveDownload", "Download HTML"),
          downloadButton("downloadEdges", "Edges"),
          downloadButton("downloadNodes", "Nodes"),
          h5("Similar to the Tree tab, this interactive plot also shows the relationship between enriched pathways.
       Two pathways (nodes) are connected if they share 20% (default) or more genes.
       You can move the nodes by dragging them, zoom in and out by scrolling,
       and shift the entire network by click on an empty point and drag.
       Darker nodes are more significantly enriched gene sets.
       Bigger nodes represent larger gene sets.
       Thicker edges represent more overlapped genes.")
        ),

        #---KEGG-----------------------------------------------------------
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
            uiOutput("listSigPathways"),
            br(), imageOutput("KeggImage", width = "100%", height = "100%"),
            h5("Your genes are highlighted in red. Downloading pathway diagram from KEGG can take 3 minutes. ")
          )
        ),

        #---Genes-----------------------------------------------------------
        tabPanel("Genes",
          value = 6,
          fluidRow(
            column(3, downloadButton("downloadGeneInfo", "More info")),
            column(4, checkboxInput("showDetailedGeneInfo", "Detailed Description", value = FALSE))
          ),
          tableOutput("conversionTable")
        ),
        #---Groups-----------------------------------------------------------
        tabPanel("Groups",
          value = 7,
          downloadButton("downloadGrouping", "Download"),
          h5("Your genes are grouped by functional categories defined by high-level GO terms. "),
          tableOutput("grouping")
        ),
        #---Plots-----------------------------------------------------------
        tabPanel("Plots",
          value = 8,
          h5("The characteristics of your genes are compared with the rest in the genome. Chi-squared and Student's
              t-tests are run to see if your genes have special characteristics when compared with all the other genes or, if uploaded, a customized background."),
          fluidRow(
            column(
              width = 4,
              mod_download_images_ui("download_gene_plot_dist", "Download density plots")
            ),
            column(
              width = 4,
              mod_download_images_ui("download_gene_barplot", "Download barplots")
            )
          ),
          br(),
          plotOutput("genePlot2", inline = TRUE, width = "auto", height = "auto"),
          plotOutput("gene_barplot", inline = TRUE, width = "auto", height = "auto")
        ),

        #---Genome-----------------------------------------------------------
        tabPanel("Genome",
          value = 9,
          mod_genome_ui("genome")
        ),

        #---STRING-----------------------------------------------------------
        tabPanel("STRING",
          value = 11,
          mod_string_ui("string")
        ),

        #--------------------------------------------------------------
        tabPanel("About",
          value = 12,
          mod_about_ui("about")
        )
        
      ), # tabsetPanel
      bsModal("ModalExamplePPI", "Protein-protein interaction networks ", "ModalPPI",
        size = "large",
        h5("By sending your genes to the STRING website,
			shinyGO is retrieving a sub-network, calculating PPI enrichment,
		  and generating custom URLs to the STRING website containing your genes. This can take 5 minutes. Patience will pay off! "),
        sliderInput("nGenesPPI", label = h5("Genes to include:"), min = 0, max = 400, value = 20, step = 10),
        # ,htmlOutput("stringDB_network_link")
        # ,tags$head(tags$style("#stringDB_network_link{color: blue; font-size: 15px;}"))

        plotOutput("stringDB_network1")
      ), # bsModal 1

      bsModal("InteractiveNetwork", "Interactive enrichment networks ", "GONetwork",
        size = "large",
        fluidRow(
          column(2, actionButton("layoutButtonStatic", "Change layout")),
          column(2, downloadButton("enrichmentNetworkPlotDownload", "Download")),
          column(2, checkboxInput("wrapTextNetworkStatic", "Wrap text", value = FALSE))
        ),
        plotOutput("enrichmentNetworkPlot")
      ), # bsModal 2

      bsModal("BackgroundGenes", "Customized background genes (recommended)", "backgroundGenes",
        size = "large",
        tags$textarea(
          id = "input_text_b",
          placeholder = "
Paste all genes from which the gene list is derived. These are all
genes whose expression or other activity that you measured.
This could be all the genes on a DNA microarray or all the genes
detected by a proteomics experiment.

By default, we compare your gene list with a background of all
protein-coding genes in the genome. When your genes are not selected
from genome-wide data, customized background genes might yield more
accurate results for enrichment analysis. For gene lists derived from
a typical RNA-seq dataset, many  use the subset of genes with detectable
expression, typically the genes passed a minimum filter.
We can also customize background genes to overcome bias in selection.
Currently only less than 30,000 genes are accepted.",
          rows = 20,
          ""
        )
      ), # bsModal 3

      bsModal("geneIDexamples", "What the gene IDs in our database look like?", "MGeneIDexamples",
        size = "large",
        selectizeInput(
          inputId = "userSpecieIDexample",
          label = "Select or search for species", choices = NULL
        ),
        tableOutput("showGeneIDs4Species")
      ) # bsModal 4
    ) # mainPanel
  ), # sidebarLayout
  tags$head(includeScript("google_analytics.js")), # tracking usage
  tags$head(includeHTML(("google_analytics_GA4.html")))
  #  ,tags$head(includeHTML(("../google_analytics_golem.html")))
) # fluidPage
