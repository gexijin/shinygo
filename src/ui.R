####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: ui.R
# Purpose of file: ui have shiny
# Start data: 01-03-2021 (mm-dd-yyyy)
# Data last modified: 01-10-2021 (mm-dd-yyyy)
#######################################################
if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)} 
pacman::p_load(shiny, reactable)

#################################################################
# FUNCTION : 
# DESCRIPTION : 
# INPUT ARGS : 
# OUTPUT ARGS : 
# IN/OUT ARGS :
# RETURN : 
#################################################################
ui <- fluidPage(
    titlePanel("View data from Ge Lab Tools"),
    sidebarLayout(
        fluid = TRUE,
        sidebarPanel(#Side panel
            #see server lines 29 and 31 
            selectizeInput(inputId = "userSpecie",
                           label = "What's your specie name?", choices = NULL),
            shiny::tags$h5("Can erase and type in box"),
            selectizeInput(inputId = "userIDtype",
                           label = "What's your ID type? (Optional)", choices = NULL),
            shiny::tags$h5("Can erase and type in box"),
            textAreaInput(inputId = "geneList", label = "Gene List (Optional)",
                          value = "", resize = "both",
                          placeholder = "Hus1 Rad1 Tp63 Tp73 Usp28 Rad9b Fanci Hus1b"),
            shiny::tags$h5("Shows example of input above"),
            
            fileInput(inputId = "geneListFile", label = "Upload Gene List (CSV or text)",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values",
                          "text/tab-separated-values",
                          "text/plain",
                          ".csv",
                          ".tsv"          
                      ), buttonLabel = "Browse...", placeholder = "No file selected" ),
            actionButton(inputId = "submit", label = "submit") 
        ),##End of side panel
        mainPanel(
            textOutput(outputId = "textResult"),
            reactable::reactableOutput(outputId = "tableResult"),
            ##Instructions for the user
            shiny::tags$div(
                shiny::tags$h1("Instructions for Usage"),
                shiny::tags$h4("This page purpose is to give the user some interactive tools to look at our database IDs.
                               There are four different uses to this page depending on how you input the data, see explanation of the four below:"),
                shiny::tags$ul(
                    shiny::tags$li(#Bullet point 1
                        shiny::tags$h4("If you only pick a species,
                                       you are receiving table with all the different IDs
                                       related to that species. ")
                    ),
                    shiny::tags$li(#Bullet point 2
                        shiny::tags$h4("If you pick a species and an ID type,
                                       a table with all the IDs of the ID type you pick and how they map to ensembl IDs(our preferred ID database).")
                    ),
                    shiny::tags$li(#Bullet point 3
                        shiny::tags$h4("If you pick a species and enter a list of genes,
                                       then all possible genes that match your query will be returned.
                                       Each column tells you the database that your gene matched to,
                                       and then in the entry it shows it being converted to ensembl IDs. 
                                       The first row of the table shows how many genes match to that ID type.")
                    ),
                    shiny::tags$li(#Bullet point 4
                        shiny::tags$h4("If you enter all three fields,
                                       you are receiver table of all the IDs that match to the species and ID type you picked.")
                    )
                )
            ) ##End of instructions
        )
    )
)

