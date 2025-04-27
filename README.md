# ShinyGO: a graphical tool for enrichment analysis

This was previously hosted alongside iDEP at the [iDEP](https://github.com/iDEP-SDSU/idep) repository. 
The Shiny app is hosted at [South Dakota State University](http://bioinformatics.sdstate.edu/go). 

Contact Dr. Ge on [Twitter](https://twitter.com/StevenXGe).



# ShinyGO Workflow Overview

## Overview of ShinyGO
ShinyGO is a gene-set enrichment analysis tool built in R Shiny that helps researchers analyze lists of genes to identify enriched biological pathways, functions, and other relevant patterns.

## User Input Section
The workflow begins with users providing several inputs:
- Species/genome selection (with over 14,000 species available)
- Gene list input (in various ID formats - NCBI, Ensembl, symbols, etc.)
- Optional background gene list (for more accurate enrichment analysis)
- Configuration parameters like FDR cutoff, pathway size limits, and sorting options

## Database Connection
The app connects to several databases:
- Species information in orgInfo database
- Gene ID conversion databases
- Gene information databases
- Pathway databases (GO, KEGG, etc.)
- STRING database for protein interactions

## Core Processing
The main computational workflow consists of:
1. Gene ID conversion - maps user's gene IDs to standardized IDs for the selected species
2. Gene info lookup - retrieves detailed information about each gene
3. Enrichment analysis - identifies statistically overrepresented pathways
4. Pathway filtering & sorting - removes redundancies and arranges results

## Reactive Objects
The app uses several reactive objects to store intermediate results and enable dynamic updates:
- `converted()` - holds gene ID conversion results
- `geneInfoLookup()` - contains gene annotation data
- `significantOverlapsAll()` - stores all enrichment results
- `significantOverlaps()` - filtered enrichment results based on user parameters

## Visualization Modules
Results are presented through modular tabs:
- **Enrichment** - tabular view of enriched pathways
- **Chart** - bar chart visualization of enrichment results
- **Tree** - hierarchical clustering of pathways
- **Network** - interactive network of pathway relationships
- **KEGG** - KEGG pathway diagrams with highlighted genes
- **Genes** - gene ID conversion and annotation tables
- **Plots** - gene characteristic visualizations
- **Genome** - chromosome distribution plots
- **STRING** - protein-protein interaction networks

## User Interaction Flow
The typical user journey involves:
1. Selecting a species
2. Pasting a gene list
3. Configuring analysis parameters
4. Submitting the analysis
5. Exploring results across different visualization tabs
6. Downloading tables, images, or other outputs
   
![shinygo_workflow_corrected](https://github.com/user-attachments/assets/1bc677d5-50b2-48b0-910f-613dc2035c9c)

![image](https://github.com/user-attachments/assets/32d8dbb3-4278-4d1a-bb4d-84b7441aaeb3)



