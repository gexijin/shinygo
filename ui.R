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

ui <- fluidPage(
  # reduce the space between label and widgets, globally
  tags$head(
    tags$style(HTML(
      "label { font-size:100%; font-family:Times New Roman; margin-bottom:-15px; }"
    ))
  ),
  shinybusy::add_busy_spinner(spin = "fading-circle"), # add spinner
  sidebarLayout(
    app_sidebar_ui(), # sidebarPanel

    mainPanel(
      tabsetPanel(
        id = "tabs", type = "tabs",
        mod_enrichment_ui("enrichment"), # enrichment tab
        mod_chart_ui("chart"), # enrichment chart tab
        mod_tree_ui("tree"), # tree tab
        mod_network_ui("network"), # network tab
        mod_kegg_ui("kegg"), # KEGG tab
        mod_genes_ui("genes"), # genes tab
        mod_groups_ui("groups"), # groups tab
        mod_plots_ui("plots"), # plots tab
        mod_genome_ui("genome"), # genome tab

        mod_string_ui("string"), # STRING tab
        mod_about_ui() # about tab
        
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
  #, tags$head(includeScript("google_analytics.js")), # tracking usage
  , tags$head(includeHTML(("google_analytics_GA4.html")))
) # fluidPage
