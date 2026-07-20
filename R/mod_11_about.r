#' 11_about UI Function
#'
#' @description Renders the static About tab: project background, citation
#'   info, version changelog, and links.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny tagList
mod_11_about_ui <- function(id) {
  tagList(
    # Intro: team, contact, citation, and related links (SDSU credits, GitHub, paper, iDEP)
    p(
      "ShinyGO is developed and maintained by a small team at ",
      a(
        "South Dakota State University (SDSU). ",
        href = "https://www.sdstate.edu/"
      ),
      "Our team consists of ",
      a(" Xijin Ge (PI), ", href = "https://www.sdstate.edu/directory/xijin-ge", target = "_blank"),
      "Jianli Qi (research associate), and
        a few graduate students."
    ),
    " For feedbacks, ",
    a("email us, ", href = "mailto:gelabinfo@gmail.com?Subject=ShinyGO"),
    "or file a bug report or feature request on our",
    a(" GitHub repository, ", href = "https://github.com/gexijin/shinygo", target = "_blank"),
    " where you can also find the source code.",
    "For details, please see our", a("paper", href = "https://doi.org/10.1093/bioinformatics/btz931", target = "_blank"),
    "and a detailed", a("demo.", href = "https://www.biorxiv.org/content/biorxiv/suppl/2018/05/04/315150.DC1/315150-1.pdf", target = "_blank"),
    "ShinyGO shares many functionalities and databases with ", a("iDEP.", href = "http://bioinformatics.sdstate.edu/idep/", target = "_blank"),
    br(), br(),
    strong("Citation (Just including URL is not enough!):"),
    br(),
    "Ge SX, Jung D & Yao R,", a(" Bioinformatics 36:2628–2629, 2020.", href = "https://doi.org/10.1093/bioinformatics/btz931", target = "_blank"),
    " If you use the KEGG diagram, please also cite ",
    a("KEGG.", href = "https://doi.org/10.1093/nar/gkaa970"),
    br(), br(),
    # Archived version history: links to previously released ShinyGO builds by Ensembl release
    strong("Previous versions are still functional:"),
    br(),
    a("ShinyGO V0.82, ",
      href = "http://bioinformatics.sdstate.edu/go82/"
    ),
    "based on Ensembl Release 104, archived on Sept 5, 2025",
    br(),
    a("ShinyGO V0.81, ",
      href = "http://bioinformatics.sdstate.edu/go81/"
    ),
    "based on Ensembl Release 104, archived on Feb. 3, 2025",
    br(),
    a("ShinyGO V0.80, ",
      href = "http://bioinformatics.sdstate.edu/go80/"
    ),
    "based on Ensembl Release 104, archived on Oct 25, 2024",
    br(),
    a("ShinyGO V0.77, ",
      href = "http://bioinformatics.sdstate.edu/go77/"
    ),
    "based on Ensembl Release 104, archived on Jan. 5, 2024",
    br(),
    a("ShinyGO V0.76, ",
      href = "http://bioinformatics.sdstate.edu/go76/"
    ),
    "based on Ensembl Release 104 with revision, archived on September 2, 2022",
    br(),
    a("ShinyGO V0.75, ",
      href = "http://bioinformatics.sdstate.edu/go75/"
    ),
    "based on Ensembl Release 104 with revision, archived on April 4, 2022",
    br(),
    a("ShinyGO V0.74, ",
      href = "http://bioinformatics.sdstate.edu/go74/"
    ),
    "based on Ensembl Release 104, archived on Feb. 8, 2022",
    br(),
    a("ShinyGO V0.65, ",
      href = "http://bioinformatics.sdstate.edu/go65/"
    ),
    "based on Ensembl Release 103, archived on Oct. 15, 2021",
    br(),
    a("ShinyGO V0.61, ",
      href = "http://bioinformatics.sdstate.edu/go61/"
    ),
    "based on Ensembl Release 96, archived on May 23, 2020",
    br(),
    a("ShinyGO V0.60, ",
      href = "http://bioinformatics.sdstate.edu/go60/"
    ),
    "based on Ensembl Release version 96, archived on Nov 6, 2019",
    br(),
    a("ShinyGO V0.51, ",
      href = "http://bioinformatics.sdstate.edu/go51/"
    ),
    "based on Ensembl Release version 95, archived on May 20, 2019",
    br(),
    a("ShinyGO V0.50, ",
      href = "http://bioinformatics.sdstate.edu/go50/"
    ),
    "based on Ensembl Release version 92, archived on March 29, 2019",
    br(),
    a("ShinyGO V0.41, ",
      href = "http://bioinformatics.sdstate.edu/go41/"
    ),
    "based on Ensembl Release version 91, archived on July 11, 2018",
    br(),
    h5("Genomes based on STRING-db is marked as STRING-db. If the same genome is included in both Ensembl and STRING-db, users should
            use Ensembl annotation, as it is more updated and is supported in more functional modules. "),
    # ,includeHTML("help.htm")
    # User-facing documentation: input format, output types, and example plots for each result panel
    h4("Input:"),
    "A list of gene ids, separated by tab, space, comma or the newline characters.
		   Ensembl gene IDs are used internally to identify genes. Other types of IDs will be mapped to Ensembl
		   gene IDs using ID mapping information available in Ensembl BioMart. ",
    h4("Output:"),
    "Enriched GO terms and pathways:",
    br(),
    img(src = "enrich.png", align = "center", width = "660", height = "339"),
    br(), br(),
    "In addition to the enrichment table, a set of plots are produced. If KEGG database is choosen, then enriched pathway diagrams are shown, with user's genes highlighted, like this one below:",
    br(),
    img(src = "KEGG.png", align = "center", width = "696", height = "494"),
    br(), br(),
    "Many GO terms are related. Some are even redundant, like \"cell cycle\" and \"cell cycle process\".
		 To visualize such relatedness in enrichment results, we use a hierarchical clustering tree and network.
		 In this hierarchical clustering tree, related GO terms are grouped together based on how many genes they share. The size of the solid circle corresponds to the enrichment FDR.",
    br(),
    img(src = "GOtree.png", align = "center", width = "700", height = "361"),
    br(), br(),
    "In this network below, each node represents an enriched GO term. Related GO terms are connected by a line, whose thickness reflects percent of overlapping genes. The size of the node corresponds to number of genes.",
    br(), img(src = "GOnetwork.png", align = "center", width = "500", height = "248"),
    br(), br(),
    "Through API access to STRING-db, we also retrieve a protein-protein interaction (PPI) network. In addition to a static network image, users can also get access to interactive graphics at the www.string-db.org web server.",
    br(), img(src = "PPInetwork.png", align = "center", width = "700", height = "547"),
    br(), br(),
    "ShinyGO also detects transcription factor (TF) binding motifs enriched in the promoters of user's genes.",
    br(), br(), img(src = "promoter.png", align = "center", width = "717", height = "288"),
    includeHTML("human_mouse_source.html"),
    br(),
    # Changelog: reverse-chronological release notes (oldest changes at bottom, newest near top)
    h4("Changes:"),
    p("2/3/25: v.0.82. Fix issues caused by multiple ENSEMBL IDs for the same gene on patched chromosomes, causing inaccurate enrichment results. Duplicated ENSEMBL IDs are now ignored. "),
    p("10/26/24: v0.81. Disabled the switch of species during analysis. Fixed errors with STRING tab when STRINGdb species are used.
            If a gene ID maps multiple Ensembl genes, all are kept for enrichment. "),
    p("10/25/24: Migrated to new server. Upgraded R to 4.4.0."),
    p("4/21/2024: New barchart with GO terms on the bars."),
    p("4/12/2024: Max set size is increased to 5000 from 2000. Some meaningful GO terms (RNA biosynthetic proc.) contains 4000+ genes. "),
    p("4/21/2024: New barchart with GO terms on the bars."),
    p("4/20/2024: UI adjustment"),
    p("4/12/2024: Max set size is increased to 5000 from 2000. Some meaningful GO terms (RNA biosynthetic proc.) contains 4000+ genes. "),
    p("1/5/2024: ShinyGO 0.80 becomes default. You have to select your species first. Database is updated to Ensembl release
             107 which includes 620 species: 215 main, 177 metazoa, 124 plants, 33 protists and 1 bacteria.
            We also included 14,094 species from STRING-DB 11.5."
    ),
    p("5/1/2023: ShinyGO 0.80 release in testing mode. Thanks to Jenny's hardwork, we update to Ensembl release
             107 which includes 620 species: 215 main, 177 metazoa, 124 plants, 33 protists and 1 bacteria.
            We also included 14,094 species from STRING-DB 11.5.",
      style = "color:red"
    ),
    p(
      "Jan. 19, 2023: Thanks to a user's feedback, we found a serious bug
              in ShinyGO 0.76.
              As some genes are represented by multiple gene IDs in Ensebml, they are counted more
              than once in calculating enrichment. We believe this is fixed. If you
              pasted Ensembl gene IDs to ShinyGO 0.76 between April 4, 2022 and Jan. 19, 2023,
              please rerun your analysis. ShinyGO has not been throughly tested.
              Please always double check your results with other tools
              such as G:profiler, Enrichr, STRING-db, and DAVID."
    ),
    p("Oct 26, 2022: V. 0.76.3 Add hover text. Change plot styles.
             When users select \"Sort by Fold Enrichment\",
             the minimum pathway size is raised to 10 to
             filter out noise from tiny gene sets."),
    p("Sept 28, 2022: In ShinyGO 0.76.2, KEGG is now the default pathway database. More importantly,
                    we reverted to 0.76 for default gene counting method, namely
                    all protein-coding genes are used as the background by default.
                    The new feature introduced in 0.76.1, which uses the pathway database
                    to determine total number of genes in the background, can be turned on as an option
                    ('Use pathway database for gene counts').
                    This is based on feedback from some users that
                     when using smaller pathway databases, such as KEGG, the new method changes
                     the P values substantially."),
    p("Sept 3, 2022: ShinyGO 0.76.1. In this small improvement, we improved how we count the number of genes for
                    calculating P value. A gene must match at least one pathway in the selected pathway database. Otherwise this gene
                    is ignored in the calculation of P values based on hypergeometric distribution. This applies to both query and background genes. "),
    p("April 19, 2022: ShinyGO 0.76 released. Improved pathway filtering, pathway sorting, figure downloading.
                       Version 0.75 is available", a("here.", href = "http://bioinformatics.sdstate.edu/go75")),
    p("April 17, 2022: Add more flexiblity for download figures in PDF, SVG and high-res PNG."),
    p("April 8, 2022: Add features to remove redundant pathways. Add filter to remove extrmely large or small pathways. Changed interface to always show KEGG tab."),
    p("Mar. 7, 2022: Fixed an R library issue affected KEGG diagrams for some organisms."),
    p("Feb. 26, 2022: Fixed a bug regarding the Plot tab when background genes are used. Background genes were not correctly
                    used to calculate the distributions of various gene characteristics. If these plots are important in your study, please re-analyze your genes."),
    p("Feb. 19, 2022: R upgraded from 4.05 to 4.1.2. This solved the STRING API issues. Some Bioconductor packages are also upgraded.", style = "color:red"),
    p("Feb. 8, 2022: ShinyGO v0.75 officially released. Old versions are still available. See the last tab.", style = "color:red"),
    p("Nov. 15, 2021: Database update. ShinyGO v0.75 available in testing mode. It includes Ensembl database update, new species from Ensembl Fungi and Ensembl Protists, and STRINGdb (5090 species) update to 11.5."),
    p("Oct25, 2021: Interactive genome plot. Identificantion of genomic regions signficantly enriched with user genes."),
    p("Oct.23, 2021: Version 0.741 A fully customizable enrichment chart! Switch between bar, dot or lollipop plots.  Detailed gene informations with links on the Genes tab."),
    p("Oct. 15, 2021: Version 0.74. Database updated to Ensembl Release 104 and STRING v11. We now recommends the use of background genes in enrichment analysis. V.0.74 is much faster with even large set of background genes."),
    h5("6/6/2021: V0.66 Adjusted interface. "),
    h5("6/2/2021: V0.66 add customized background genes."),
    h5("5/23/2021: V0.65 Database update to Ensembl 103 and STRING-db v11."),
    h5("11/3/2019: V 0.61 Improved visualization based on suggestions from reviewers. Interactive networks."),
    h5("5/20/2019: V0.60 Upgraded to Ensembl Biomart 96. Add annotation from STRING-db v10"),
    h5("3/29/2019: V0.51 Update annotation to Ensembl release 95. Interface change. Demo gene lists. Error messages."),
    h5("9/10/2018: V0.5 Upgraded to Ensembl Biomart 92"),
    h5("4/30/2018: V0.42 changed figure configurations for tree."),
    h5("4/27/2018: V0.41 Change to ggplot2, add grid and gridExtra packages"),
    h5("4/24/2018: V0.4 Add STRING API, KEGG diagram, tree display and network.")
  )
}
