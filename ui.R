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
      titlePanel("ShinyGO 0.85.1",
        tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
             tags$title("ShinyGO 0.85"))
      ),
      # use conditional panel to hide the selectOrg input
      conditionalPanel(
        condition = "0", # hide the selectOrg input, always
        selectInput(
          inputId = "selectOrg",
          label = NULL,
          selectize = TRUE,
          choices = setNames(96, "Human"), # Human is selected by default
          selected = setNames(96, "Human")
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

        #---Enrichment-----------------------------------------------------------
        tabPanel("Enrichment",
          value = 1,
          mod_01_enrichment_ui("enrichment")
        ), 

        #---Enrichment Chart-----------------------------------------------------------
        tabPanel("Chart",
          value = 3,
          mod_02_chart_ui("chart")
        ),

        #---Tree-----------------------------------------------------------
        tabPanel("Tree",
          value = 4,
          mod_03_tree_ui("tree")
        ),

        #---Enrichment network-------------------------------------------------------
        tabPanel("Network",
          value = 5,
          mod_04_network_ui("network")
        ),

        #---KEGG-----------------------------------------------------------
        tabPanel("KEGG",
          value = 2,
          mod_05_kegg_ui("kegg")
        ),

        #---Genes-----------------------------------------------------------
        tabPanel("Genes",
          value = 6,
          mod_06_genes_ui("genes")
        ),
        #---Groups-----------------------------------------------------------
        tabPanel("Groups",
          value = 7,
          mod_07_groups_ui("groups")
        ),
        #---Plots-----------------------------------------------------------
        tabPanel("Plots",
          value = 8,
          mod_08_plots_ui("plots")
          ),

        #---Genome-----------------------------------------------------------
        tabPanel("Genome",
          value = 9,
          mod_09_genome_ui("genome")
        ),
        #---Genome-----------------------------------------------------------
#        tabPanel("Promoter",
#          value = 10,
#          radioButtons("radio", label = NULL, choices = list(
#            "Upstream 300bp as promoter" = 300,
#            "Upstream 600bp as promoter" = 600
#          ), selected = 300),
#          tableOutput("promoter"),
#          downloadButton("downloadPromoter", "Download"),
#          h5("The promoter sequences of your genes are compared with those of the
#              other genes in the genome in terms of transcription factor (TF) binding motifs.
#              \"*Query gene\" indicates a transcription factor coded by a gene included in
#              your list.")
#        ),
        #---STRING-----------------------------------------------------------
        tabPanel("STRING",
          value = 11,
          mod_10_string_ui("string")
        ),

        #---?-----------------------------------------------------------
        tabPanel("About",
          value = 12,
          mod_11_about_ui("about")
        )
        
      ), # tabsetPanel

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
      ), # bsModal 4

    ) # mainPanel
  ) # sidebarLayout
  #, tags$head(includeScript("google_analytics.js")), # tracking usage
  #tags$head(includeHTML(("google_analytics_GA4.html")))
) # fluidPage
