# R/mod_kegg.R

#' KEGG UI Function
#'
#' @description A shiny Module to display KEGG pathway diagrams.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kegg_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "input.selectGO != 'KEGG'",
      br(), br(),
      h5("Please select KEGG from the pathway databases to conduct enrichment analysis first.
          Then you can visualize your genes on any of the significant pathways. Only for some species.")
    ),
    conditionalPanel(
      "input.selectGO == 'KEGG'",
      br(),
      uiOutput(ns("listSigPathways")),
      br(), 
      imageOutput(ns("KeggImage"), width = "100%", height = "100%"),
      h5("Your genes are highlighted in red. Downloading pathway diagram from KEGG can take 3 minutes.")
    )
  )
}



#' KEGG Server Functions
#'
#' @noRd
mod_kegg_server <- function(id, converted, significantOverlaps, input_selectGO, input_goButton, input_selectOrg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$listSigPathways <- renderUI({
      tem <- input_selectOrg()
      if (input_goButton() == 0 || is.null(significantOverlaps())) {
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
        if (input_goButton() == 0) {
            return(blank)
        }
        if (is.null(input_selectGO())) {
            return(blank)
        }
        if (input_selectGO() != "KEGG") {
            return(blank)
        }
        if (is.null(significantOverlaps())) {
            return(blank)
        }

        library(pathview, verbose = FALSE)

        # these two functions are from the pathview package, modified to write to a designated folder: temp.
        mypathview <- function(gene.data = NULL, cpd.data = NULL, pathway.id, species = "hsa",
                                kegg.dir = ".", cpd.idtype = "kegg", gene.idtype = "entrez",
                                gene.annotpkg = NULL, min.nnodes = 3, kegg.native = TRUE,
                                map.null = TRUE, expand.node = FALSE, split.group = FALSE,
                                map.symbol = TRUE, map.cpdname = TRUE, node.sum = "sum",
                                discrete = list(gene = FALSE, cpd = FALSE), limit = list(
                                gene = 1,
                                cpd = 1
                                ), bins = list(gene = 10, cpd = 10), both.dirs = list(
                                gene = T,
                                cpd = T
                                ), trans.fun = list(gene = NULL, cpd = NULL),
                                low = list(gene = "green", cpd = "blue"), mid = list(
                                gene = "gray",
                                cpd = "gray"
                                ), high = list(gene = "red", cpd = "yellow"),
                                na.col = "transparent", ...) {
            dtypes <- !is.null(gene.data) + (!is.null(cpd.data))
            cond0 <- dtypes == 1 & is.numeric(limit) & length(limit) >
            1
            if (cond0) {
            if (limit[1] != limit[2] & is.null(names(limit))) {
                limit <- list(gene = limit[1:2], cpd = limit[1:2])
            }
            }
            if (is.null(trans.fun)) {
            trans.fun <- list(gene = NULL, cpd = NULL)
            }
            arg.len2 <- c(
            "discrete", "limit", "bins", "both.dirs", "trans.fun",
            "low", "mid", "high"
            )
            for (arg in arg.len2) {
            obj1 <- eval(as.name(arg))
            if (length(obj1) == 1) {
                obj1 <- rep(obj1, 2)
            }
            if (length(obj1) > 2) {
                obj1 <- obj1[1:2]
            }
            obj1 <- as.list(obj1)
            ns <- names(obj1)
            if (length(ns) == 0 | !all(c("gene", "cpd") %in% ns)) {
                names(obj1) <- c("gene", "cpd")
            }
            assign(arg, obj1)
            }
            if (is.character(gene.data)) {
            gd.names <- gene.data
            gene.data <- rep(1, length(gene.data))
            names(gene.data) <- gd.names
            both.dirs$gene <- FALSE
            ng <- length(gene.data)
            nsamp.g <- 1
            } else if (!is.null(gene.data)) {
            if (length(dim(gene.data)) == 2) {
                gd.names <- rownames(gene.data)
                ng <- nrow(gene.data)
                nsamp.g <- 2
            } else if (is.numeric(gene.data) & is.null(dim(gene.data))) {
                gd.names <- names(gene.data)
                ng <- length(gene.data)
                nsamp.g <- 1
            } else {
                stop("wrong gene.data format!")
            }
            } else if (is.null(cpd.data)) {
            stop("gene.data and cpd.data are both NULL!")
            }
            gene.idtype <- toupper(gene.idtype)
            data(bods)
            if (species != "ko") {
            species.data <- kegg.species.code(species,
                na.rm = T,
                code.only = FALSE
            )
            } else {
            species.data <- c(
                kegg.code = "ko", entrez.gnodes = "0",
                kegg.geneid = "K01488", ncbi.geneid = NA, ncbi.proteinid = NA,
                uniprot = NA
            )
            gene.idtype <- "KEGG"
            msg.fmt <- "Only KEGG ortholog gene ID is supported, make sure it looks like \"%s\"!"
            msg <- sprintf(msg.fmt, species.data["kegg.geneid"])
            message("Note: ", msg)
            }
            if (length(dim(species.data)) == 2) {
            message("Note: ", "More than two valide species!")
            species.data <- species.data[1, ]
            }
            species <- species.data["kegg.code"]
            entrez.gnodes <- species.data["entrez.gnodes"] == 1
            if (is.na(species.data["ncbi.geneid"])) {
            if (!is.na(species.data["kegg.geneid"])) {
                msg.fmt <- "Mapping via KEGG gene ID (not Entrez) is supported for this species,\nit looks like \"%s\"!"
                msg <- sprintf(msg.fmt, species.data["kegg.geneid"])
                message("Note: ", msg)
            } else {
                stop("This species is not annotated in KEGG!")
            }
            }
            if (is.null(gene.annotpkg)) {
            gene.annotpkg <- bods[match(species, bods[, 3]), 1]
            }
            if (length(grep("ENTREZ|KEGG|NCBIPROT|UNIPROT", gene.idtype)) <
            1 & !is.null(gene.data)) {
            if (is.na(gene.annotpkg)) {
                stop("No proper gene annotation package available!")
            }
            if (!gene.idtype %in% gene.idtype.bods[[species]]) {
                stop("Wrong input gene ID type!")
            }
            gene.idmap <- id2eg(gd.names,
                category = gene.idtype,
                pkg.name = gene.annotpkg, unique.map = F
            )
            gene.data <- mol.sum(gene.data, gene.idmap)
            gene.idtype <- "ENTREZ"
            }
            if (gene.idtype != "KEGG" & !entrez.gnodes & !is.null(gene.data)) {
            id.type <- gene.idtype
            if (id.type == "ENTREZ") {
                id.type <- "ENTREZID"
            }
            kid.map <- names(species.data)[-c(1:2)]
            kid.types <- names(kid.map) <- c(
                "KEGG", "ENTREZID", "NCBIPROT",
                "UNIPROT"
            )
            kid.map2 <- gsub("[.]", "-", kid.map)
            kid.map2["UNIPROT"] <- "up"
            if (is.na(kid.map[id.type])) {
                stop("Wrong input gene ID type for the species!")
            }
            message("Info: Getting gene ID data from KEGG...")
            gene.idmap <- KEGGREST::keggConv(kid.map2[id.type], species)
            message("Info: Done with data retrieval!")
            kegg.ids <- gsub(paste(species, ":", sep = ""), "", names(gene.idmap))
            in.ids <- gsub(paste0(kid.map2[id.type], ":"), "", gene.idmap)
            gene.idmap <- cbind(in.ids, kegg.ids)
            gene.data <- mol.sum(gene.data, gene.idmap)
            gene.idtype <- "KEGG"
            }
            if (is.character(cpd.data)) {
            cpdd.names <- cpd.data
            cpd.data <- rep(1, length(cpd.data))
            names(cpd.data) <- cpdd.names
            both.dirs$cpd <- FALSE
            ncpd <- length(cpd.data)
            } else if (!is.null(cpd.data)) {
            if (length(dim(cpd.data)) == 2) {
                cpdd.names <- rownames(cpd.data)
                ncpd <- nrow(cpd.data)
            } else if (is.numeric(cpd.data) & is.null(dim(cpd.data))) {
                cpdd.names <- names(cpd.data)
                ncpd <- length(cpd.data)
            } else {
                stop("wrong cpd.data format!")
            }
            }
            if (length(grep("kegg", cpd.idtype)) < 1 & !is.null(cpd.data)) {
            data(rn.list)
            cpd.types <- c(names(rn.list), "name")
            cpd.types <- tolower(cpd.types)
            cpd.types <- cpd.types[-grep("kegg", cpd.types)]
            if (!tolower(cpd.idtype) %in% cpd.types) {
                stop("Wrong input cpd ID type!")
            }
            cpd.idmap <- cpd2kegg(cpdd.names, in.type = cpd.idtype)
            cpd.data <- mol.sum(cpd.data, cpd.idmap)
            }
            warn.fmt <- "Parsing %s file failed, please check the file!"
            if (length(grep(species, pathway.id)) > 0) {
            pathway.name <- pathway.id
            pathway.id <- gsub(species, "", pathway.id)
            } else {
            pathway.name <- paste(species, pathway.id, sep = "")
            }
            kfiles <- list.files(path = kegg.dir, pattern = "[.]xml|[.]png")
            npath <- length(pathway.id)
            out.list <- list()
            tfiles.xml <- paste(pathway.name, "xml", sep = ".")
            tfiles.png <- paste(pathway.name, "png", sep = ".")
            if (kegg.native) {
            ttype <- c("xml", "png")
            } else {
            ttype <- "xml"
            }
            xml.file <- paste(kegg.dir, "/", tfiles.xml, sep = "")
            for (i in 1:npath) {
            if (kegg.native) {
                tfiles <- c(tfiles.xml[i], tfiles.png[i])
            } else {
                tfiles <- tfiles.xml[i]
            }
            if (!all(tfiles %in% kfiles)) {
                dstatus <- download.kegg(
                pathway.id = pathway.id[i],
                species = species, kegg.dir = kegg.dir, file.type = ttype
                )
                if (dstatus == "failed") {
                warn.fmt <- "Failed to download KEGG xml/png files, %s skipped!"
                warn.msg <- sprintf(warn.fmt, pathway.name[i])
                message("Warning: ", warn.msg)
                return(invisible(0))
                }
            }
            if (kegg.native) {
                node.data <- try(node.info(xml.file[i]), silent = T)
                if (class(node.data) == "try-error") {
                warn.msg <- sprintf(warn.fmt, xml.file[i])
                message("Warning: ", warn.msg)
                return(invisible(0))
                }
                node.type <- c("gene", "enzyme", "compound", "ortholog")
                sel.idx <- node.data$type %in% node.type
                nna.idx <- !is.na(node.data$x + node.data$y + node.data$width +
                node.data$height)
                sel.idx <- sel.idx & nna.idx
                if (sum(sel.idx) < min.nnodes) {
                warn.fmt <- "Number of mappable nodes is below %d, %s skipped!"
                warn.msg <- sprintf(warn.fmt, min.nnodes, pathway.name[i])
                message("Warning: ", warn.msg)
                return(invisible(0))
                }
                node.data <- lapply(node.data, "[", sel.idx)
            } else {
                gR1 <- try(parseKGML2Graph2(xml.file[i],
                genes = F,
                expand = expand.node, split.group = split.group
                ),
                silent = T
                )
                node.data <- try(node.info(gR1), silent = T)
                if (class(node.data) == "try-error") {
                warn.msg <- sprintf(warn.fmt, xml.file[i])
                message("Warning: ", warn.msg)
                return(invisible(0))
                }
            }
            if (species == "ko") {
                gene.node.type <- "ortholog"
            } else {
                gene.node.type <- "gene"
            }
            if ((!is.null(gene.data) | map.null) & sum(node.data$type ==
                gene.node.type) > 1) {
                plot.data.gene <- node.map(gene.data, node.data,
                node.types = gene.node.type,
                node.sum = node.sum, entrez.gnodes = entrez.gnodes
                )
                kng <- plot.data.gene$kegg.names
                kng.char <- gsub("[0-9]", "", unlist(kng))
                if (any(kng.char > "")) {
                entrez.gnodes <- FALSE
                }
                if (map.symbol & species != "ko" & entrez.gnodes) {
                if (is.na(gene.annotpkg)) {
                    warn.fmt <- "No annotation package for the species %s, gene symbols not mapped!"
                    warn.msg <- sprintf(warn.fmt, species)
                    message("Warning: ", warn.msg)
                } else {
                    plot.data.gene$labels <- NA # Try to fix this error: Error in $<-.data.frame: replacement has 97 rows, data has 103
                    plot.data.gene$labels <- eg2id(as.character(plot.data.gene$kegg.names),
                    category = "SYMBOL", pkg.name = gene.annotpkg
                    )[
                    ,
                    2
                    ]
                    mapped.gnodes <- rownames(plot.data.gene)
                    node.data$labels[mapped.gnodes] <- plot.data.gene$labels
                }
                }
                cols.ts.gene <- node.color(plot.data.gene, limit$gene,
                bins$gene,
                both.dirs = both.dirs$gene, trans.fun = trans.fun$gene,
                discrete = discrete$gene, low = low$gene, mid = mid$gene,
                high = high$gene, na.col = na.col
                )
            } else {
                plot.data.gene <- cols.ts.gene <- NULL
            }
            if ((!is.null(cpd.data) | map.null) & sum(node.data$type ==
                "compound") > 1) {
                plot.data.cpd <- node.map(cpd.data, node.data,
                node.types = "compound",
                node.sum = node.sum
                )
                if (map.cpdname & !kegg.native) {
                plot.data.cpd$labels <- cpdkegg2name(plot.data.cpd$labels)[
                    ,
                    2
                ]
                mapped.cnodes <- rownames(plot.data.cpd)
                node.data$labels[mapped.cnodes] <- plot.data.cpd$labels
                }
                cols.ts.cpd <- node.color(plot.data.cpd, limit$cpd,
                bins$cpd,
                both.dirs = both.dirs$cpd, trans.fun = trans.fun$cpd,
                discrete = discrete$cpd, low = low$cpd, mid = mid$cpd,
                high = high$cpd, na.col = na.col
                )
            } else {
                plot.data.cpd <- cols.ts.cpd <- NULL
            }
            if (kegg.native) {
                pv.pars <- my.keggview.native(
                plot.data.gene = plot.data.gene,
                cols.ts.gene = cols.ts.gene, plot.data.cpd = plot.data.cpd,
                cols.ts.cpd = cols.ts.cpd, node.data = node.data,
                pathway.name = pathway.name[i], kegg.dir = kegg.dir,
                limit = limit, bins = bins, both.dirs = both.dirs,
                discrete = discrete, low = low, mid = mid, high = high,
                na.col = na.col, ...
                )
            } else {
                pv.pars <- keggview.graph(
                plot.data.gene = plot.data.gene,
                cols.ts.gene = cols.ts.gene, plot.data.cpd = plot.data.cpd,
                cols.ts.cpd = cols.ts.cpd, node.data = node.data,
                path.graph = gR1, pathway.name = pathway.name[i],
                map.cpdname = map.cpdname, split.group = split.group,
                limit = limit, bins = bins, both.dirs = both.dirs,
                discrete = discrete, low = low, mid = mid, high = high,
                na.col = na.col, ...
                )
            }
            plot.data.gene <- cbind(plot.data.gene, cols.ts.gene)
            if (!is.null(plot.data.gene)) {
                cnames <- colnames(plot.data.gene)[-(1:8)]
                nsamp <- length(cnames) / 2
                if (nsamp > 1) {
                cnames[(nsamp + 1):(2 * nsamp)] <- paste(cnames[(nsamp +
                    1):(2 * nsamp)], "col", sep = ".")
                } else {
                cnames[2] <- "mol.col"
                }
                colnames(plot.data.gene)[-(1:8)] <- cnames
            }
            plot.data.cpd <- cbind(plot.data.cpd, cols.ts.cpd)
            if (!is.null(plot.data.cpd)) {
                cnames <- colnames(plot.data.cpd)[-(1:8)]
                nsamp <- length(cnames) / 2
                if (nsamp > 1) {
                cnames[(nsamp + 1):(2 * nsamp)] <- paste(cnames[(nsamp +
                    1):(2 * nsamp)], "col", sep = ".")
                } else {
                cnames[2] <- "mol.col"
                }
                colnames(plot.data.cpd)[-(1:8)] <- cnames
            }
            out.list[[i]] <- list(
                plot.data.gene = plot.data.gene,
                plot.data.cpd = plot.data.cpd
            )
            }
            if (npath == 1) {
            out.list <- out.list[[1]]
            } else {
            names(out.list) <- pathway.name
            }
            return(invisible(out.list))
        } # <environment: namespace:pathview>
        my.keggview.native <- function(plot.data.gene = NULL, plot.data.cpd = NULL, cols.ts.gene = NULL,
                                        cols.ts.cpd = NULL, node.data, pathway.name, out.suffix = "pathview",
                                        kegg.dir = ".", multi.state = TRUE, match.data = TRUE, same.layer = TRUE,
                                        res = 400, cex = 0.25, discrete = list(gene = FALSE, cpd = FALSE),
                                        limit = list(gene = 1, cpd = 1), bins = list(gene = 10, cpd = 10),
                                        both.dirs = list(gene = T, cpd = T), low = list(
                                        gene = "green",
                                        cpd = "blue"
                                        ), mid = list(gene = "gray", cpd = "gray"),
                                        high = list(gene = "red", cpd = "yellow"), na.col = "transparent",
                                        new.signature = TRUE, plot.col.key = FALSE, key.align = "x",
                                        key.pos = "topright", ...) {
            img <- readPNG(paste(kegg.dir, "/", pathway.name, ".png",
            sep = ""
            ))
            width <- ncol(img)
            height <- nrow(img)
            cols.ts.gene <- cbind(cols.ts.gene)
            cols.ts.cpd <- cbind(cols.ts.cpd)
            nc.gene <- max(ncol(cols.ts.gene), 0)
            nc.cpd <- max(ncol(cols.ts.cpd), 0)
            nplots <- max(nc.gene, nc.cpd)
            pn.suffix <- colnames(cols.ts.gene)
            if (length(pn.suffix) < nc.cpd) {
            pn.suffix <- colnames(cols.ts.cpd)
            }
            if (length(pn.suffix) < nplots) {
            pn.suffix <- 1:nplots
            }
            if (length(pn.suffix) == 1) {
            pn.suffix <- out.suffix
            } else {
            pn.suffix <- paste(out.suffix, pn.suffix, sep = ".")
            }
            na.col <- colorpanel2(1, low = na.col, high = na.col)
            if ((match.data | !multi.state) & nc.gene != nc.cpd) {
            if (nc.gene > nc.cpd & !is.null(cols.ts.cpd)) {
                na.mat <- matrix(na.col, ncol = nplots - nc.cpd, nrow = nrow(cols.ts.cpd))
                cols.ts.cpd <- cbind(cols.ts.cpd, na.mat)
            }
            if (nc.gene < nc.cpd & !is.null(cols.ts.gene)) {
                na.mat <- matrix(na.col,
                ncol = nplots - nc.gene,
                nrow = nrow(cols.ts.gene)
                )
                cols.ts.gene <- cbind(cols.ts.gene, na.mat)
            }
            nc.gene <- nc.cpd <- nplots
            }
            out.fmt <- "Working in directory %s"
            wdir <- getwd()
            out.msg <- sprintf(out.fmt, wdir)
            message("Info: ", out.msg)
            out.fmt <- "Writing image file %s"
            multi.state <- multi.state & nplots > 1
            if (multi.state) {
            nplots <- 1
            pn.suffix <- paste(out.suffix, "multi", sep = ".")
            if (nc.gene > 0) {
                cols.gene.plot <- cols.ts.gene
            }
            if (nc.cpd > 0) {
                cols.cpd.plot <- cols.ts.cpd
            }
            }
            for (np in 1:nplots) {
            # img.file = paste(pathway.name, pn.suffix[np], "png",
            #    sep = ".")
            img.file <- paste(kegg.dir, "/", pathway.name, ".", pn.suffix[np], ".png",
                sep = ""
            )
            out.msg <- sprintf(out.fmt, img.file)
            message("Info: ", out.msg)
            png(img.file, width = width, height = height, res = res)
            op <- par(mar = c(0, 0, 0, 0))
            plot(c(0, width), c(0, height),
                type = "n", xlab = "",
                ylab = "", xaxs = "i", yaxs = "i"
            )
            if (new.signature) {
                img[height - 4:25, 17:137, 1:3] <- 1
            }
            if (same.layer != T) {
                rasterImage(img, 0, 0, width, height, interpolate = F)
            }
            if (!is.null(cols.ts.gene) & nc.gene >= np) {
                if (!multi.state) {
                cols.gene.plot <- cols.ts.gene[, np]
                }
                if (same.layer != T) {
                render.kegg.node(plot.data.gene, cols.gene.plot,
                    img,
                    same.layer = same.layer, type = "gene",
                    cex = cex
                )
                } else {
                img <- render.kegg.node(plot.data.gene, cols.gene.plot,
                    img,
                    same.layer = same.layer, type = "gene"
                )
                }
            }
            if (!is.null(cols.ts.cpd) & nc.cpd >= np) {
                if (!multi.state) {
                cols.cpd.plot <- cols.ts.cpd[, np]
                }
                if (same.layer != T) {
                render.kegg.node(plot.data.cpd, cols.cpd.plot,
                    img,
                    same.layer = same.layer, type = "compound",
                    cex = cex
                )
                } else {
                img <- render.kegg.node(plot.data.cpd, cols.cpd.plot,
                    img,
                    same.layer = same.layer, type = "compound"
                )
                }
            }
            if (same.layer == T) {
                rasterImage(img, 0, 0, width, height, interpolate = F)
            }
            pv.pars <- list()
            pv.pars$gsizes <- c(width = width, height = height)
            pv.pars$nsizes <- c(46, 17)
            pv.pars$op <- op
            pv.pars$key.cex <- 2 * 72 / res
            pv.pars$key.lwd <- 1.2 * 72 / res
            pv.pars$sign.cex <- cex
            off.sets <- c(x = 0, y = 0)
            align <- "n"
            ucol.gene <- unique(as.vector(cols.ts.gene))
            na.col.gene <- ucol.gene %in% c(na.col, NA)
            if (plot.col.key & !is.null(cols.ts.gene) & !all(na.col.gene)) {
                off.sets <- col.key(
                limit = limit$gene, bins = bins$gene,
                both.dirs = both.dirs$gene, discrete = discrete$gene,
                graph.size = pv.pars$gsizes, node.size = pv.pars$nsizes,
                key.pos = key.pos, cex = pv.pars$key.cex, lwd = pv.pars$key.lwd,
                low = low$gene, mid = mid$gene, high = high$gene,
                align = "n"
                )
                align <- key.align
            }
            ucol.cpd <- unique(as.vector(cols.ts.cpd))
            na.col.cpd <- ucol.cpd %in% c(na.col, NA)
            if (plot.col.key & !is.null(cols.ts.cpd) & !all(na.col.cpd)) {
                off.sets <- col.key(
                limit = limit$cpd, bins = bins$cpd,
                both.dirs = both.dirs$cpd, discrete = discrete$cpd,
                graph.size = pv.pars$gsizes, node.size = pv.pars$nsizes,
                key.pos = key.pos, off.sets = off.sets, cex = pv.pars$key.cex,
                lwd = pv.pars$key.lwd, low = low$cpd, mid = mid$cpd,
                high = high$cpd, align = align
                )
            }
            if (new.signature) {
                pathview.stamp(x = 17, y = 20, on.kegg = T, cex = pv.pars$sign.cex)
            }
            par(pv.pars$op)
            dev.off()
            }
            return(invisible(pv.pars))
        }

        # modify function in a package, change namespace
        # http://stackoverflow.com/questions/23279904/modifying-an-r-package-function-for-current-r-session-assigninnamespace-not-beh
        tmpfun <- get("keggview.native", envir = asNamespace("pathview"))
        environment(my.keggview.native) <- environment(tmpfun)
        attributes(my.keggview.native) <- attributes(tmpfun) # don't know if this is really needed

        isolate({
            withProgress(message = "Rendering KEGG pathway plot", {
            incProgress(1 / 5, "Loading the pathview package")


            Species <- converted()$species[1, 1]
            fold <- convertEnsembl2Entrez(converted()$IDs, Species)

            fold <- fold$entrezgene_id
            keggSpecies <- as.character(keggSpeciesID[which(keggSpeciesID[, 1] == Species), 3])

            if (nchar(keggSpecies) <= 2) {
                return(blank)
            } # not in KEGG

            # kegg pathway id
            incProgress(1 / 2, "Download pathway graph from KEGG.")

            # find pathway id
            # "Path:hsa04110 Cell cycle" --> "hsa04110"
            pathID <- gsub(" .*", "", input$sigPathways)
            pathID <- gsub("Path:", "", pathID)
            # pathID <- keggPathwayID(input$sigPathways, Species, "KEGG", input$selectOrg)
            # cat("\nhere5  ",keggSpecies, " ",Species," ",input$sigPathways, "pathID:",pathID,"End", fold[1:5],names(fold)[1:5],"\n")
            # cat("\npathway:",is.na(input$sigPathways))
            # cat("\n",fold[1:5],"\n",keggSpecies,"\n",pathID)

            if (nchar(pathID) < 3) {
                return(blank)
            }
            randomString <- gsub(".*file", "", tempfile())
            tempFolder <- tempdir() # tempFolder = "temp";
            outfile <- paste(tempFolder, "/", pathID, ".", randomString, ".png", sep = "")

            pv.out <- mypathview(gene.data = fold, pathway.id = pathID, kegg.dir = tempFolder, out.suffix = randomString, species = keggSpecies, kegg.native = TRUE)


            # Return a list containing the filename
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