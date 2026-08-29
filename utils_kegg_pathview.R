# Functions adapted from the pathview R/Bioconductor package (GPL-3).
# Original author: Weijun Luo
# Citation: Luo W, Brouwer C (2013). Pathview: an R/Bioconductor package for
#   pathway-based data integration and visualization. Bioinformatics, 29(14):1830-2.
#   https://doi.org/10.1093/bioinformatics/btt285
# Sources:
# https://rdrr.io/bioc/pathview/f/
# https://code.bioconductor.org/browse/pathview/tree/RELEASE_3_22/R/
#

# ---- colorpanel2 ------------------------------------------------------------

colorpanel2 <- function(n,
                        low,
                        mid,
                        high) {
  if (missing(mid) || missing(high)) {
    low <- grDevices::col2rgb(low)
    if (missing(high)) {
      high <- grDevices::col2rgb(mid)
    } else {
      high <- grDevices::col2rgb(high)
    }
    red <- seq(low[1, 1], high[1, 1], length = n) / 255
    green <- seq(low[3, 1], high[3, 1], length = n) / 255
    blue <- seq(low[2, 1], high[2, 1], length = n) / 255
  } else {
    isodd <- n %% 2 == 1
    if (isodd) {
      n <- n + 1
    }
    low <- grDevices::col2rgb(low)
    mid <- grDevices::col2rgb(mid)
    high <- grDevices::col2rgb(high)
    lower <- floor(n / 2)
    upper <- n - lower
    red <- c(
      seq(low[1, 1], mid[1, 1], length = lower),
      seq(mid[1, 1], high[1, 1], length = upper)
    ) / 255
    green <- c(
      seq(low[3, 1], mid[3, 1], length = lower),
      seq(mid[3, 1], high[3, 1], length = upper)
    ) / 255
    blue <- c(
      seq(low[2, 1], mid[2, 1], length = lower),
      seq(mid[2, 1], high[2, 1], length = upper)
    ) / 255
    if (isodd) {
      red <- red[-(lower + 1)]
      green <- green[-(lower + 1)]
      blue <- blue[-(lower + 1)]
    }
  }
  grDevices::rgb(red, blue, green)
}


# ---- render.kegg.node -------------------------------------------------------

render.kegg.node <- function(plot.data,
                             cols.ts,
                             img,
                             same.layer = TRUE,
                             type = c("gene", "compound")[1],
                             text.col = "black",
                             cex = 0.25) {
  width <- ncol(img)
  height <- nrow(img)
  nn <- nrow(plot.data)
  pwids <- plot.data$width
  if (!all(pwids == max(pwids))) {
    message("Info: ", "some node width is different from others, and hence adjusted!")
    wc <- table(pwids)
    pwids <- plot.data$width <- as.numeric(names(wc)[which.max(wc)])
  }

  if (type == "gene") {
    if (!same.layer) {
      rect.out <- sliced.shapes(plot.data$x + 0.5, height - plot.data$y, plot.data$width / 2 - 0.5, plot.data$height / 2 - 0.25, cols = cols.ts, draw.border = F, shape = "rectangle")
      text(plot.data$x + 0.5, height - plot.data$y,
        labels = as.character(plot.data$labels),
        cex = cex, col = text.col
      )
      return(invisible(1))
    } else {
      img2 <- img
      pidx <- cbind(
        ceiling(plot.data$x - plot.data$width / 2) + 1,
        floor(plot.data$x + plot.data$width / 2) + 1,
        ceiling(plot.data$y - plot.data$height / 2) + 1,
        floor(plot.data$y + plot.data$height / 2) + 1
      )
      cols.ts <- cbind(cols.ts)
      ns <- ncol(cols.ts)
      brk.x <- sapply(plot.data$width / 2, function(wi) seq(-wi, wi, length = ns + 1))
      for (k in 1:ns) {
        col.rgb <- col2rgb(cols.ts[, k]) / 255
        pxr <- t(apply(pidx[, 1:2], 1, function(x) x[1]:x[2])) - plot.data$x - 1
        sel <- pxr >= ceiling(brk.x[k, ]) & pxr <= floor(brk.x[k + 1, ])
        for (i in 1:nn) {
          sel.px <- (pidx[i, 1]:pidx[i, 2])[sel[i, ]]
          node.rgb <- img[pidx[i, 3]:pidx[i, 4], sel.px, 1:3]
          node.rgb.sum <- apply(node.rgb, c(1, 2), sum)
          blk.ind <- which(node.rgb.sum == 0 | node.rgb.sum == 1, arr.ind = TRUE)
          node.rgb <- array(col.rgb[, i], dim(node.rgb)[3:1])
          node.rgb <- aperm(node.rgb, 3:1)
          for (j in 1:3) node.rgb[cbind(blk.ind, j)] <- 0
          img2[pidx[i, 3]:pidx[i, 4], sel.px, 1:3] <- node.rgb
        }
      }
      return(img2)
    }
  } else if (type == "compound") {
    if (!same.layer) {
      nc.cols <- ncol(cbind(cols.ts))
      if (nc.cols > 2) {
        na.cols <- rep("#FFFFFF", nrow(plot.data))
        cir.out <- sliced.shapes(plot.data$x, height - plot.data$y, plot.data$width[1], plot.data$width[1], cols = na.cols, draw.border = F, shape = "ellipse", lwd = 0.2)
      }
      cir.out <- sliced.shapes(plot.data$x, height - plot.data$y, plot.data$width[1], plot.data$width[1], cols = cols.ts, shape = "ellipse", blwd = 0.2)
      return(invisible(1))
    } else {
      blk <- c(0, 0, 0)
      img2 <- img
      w <- ncol(img)
      h <- nrow(img)
      cidx <- rep(1:w, each = h)
      ridx <- rep(1:h, w)
      pidx <- lapply(1:nn, function(i) {
        ii <- which(
          (cidx - plot.data$x[i])^2 + (ridx - plot.data$y[i])^2 < (plot.data$width[i])^2
        )
        imat <- cbind(cbind(ridx, cidx)[rep(ii, each = 3), ], 1:3)
        imat[, 1:2] <- imat[, 1:2] + 1
        ib <- which(
          abs((cidx - plot.data$x[i])^2 + (ridx - plot.data$y[i])^2 - (plot.data$width[i])^2) <= 8
        )
        ibmat <- cbind(cbind(ridx, cidx)[rep(ib, each = 3), ], 1:3)
        ibmat[, 1:2] <- ibmat[, 1:2] + 1
        return(list(fill = imat, border = ibmat))
      })

      cols.ts <- cbind(cols.ts)
      ns <- ncol(cols.ts)
      brk.x <- sapply(plot.data$width, function(wi) seq(-wi, wi, length = ns + 1))
      for (i in 1:nn) {
        pxr <- pidx[[i]]$fill[, 2] - 1 - plot.data$x[i]
        col.rgb <- col2rgb(cols.ts[i, ]) / 255
        for (k in 1:ns) {
          sel <- pxr >= brk.x[k, i] & pxr <= brk.x[k + 1, i]
          img2[pidx[[i]]$fill[sel, ]] <- col.rgb[, k]
        }
        img2[pidx[[i]]$border] <- blk
      }
      return(img2)
    }
  } else {
    stop("unrecognized node type!")
  }
}


# ---- pathview.stamp ---------------------------------------------------------

pathview.stamp <- function(x = NULL,
                           y = NULL,
                           position = "bottomright",
                           graph.sizes,
                           on.kegg = TRUE,
                           cex = 1) {
  labels <- "Data on KEGG graph\nRendered by ShinyGO"
  if (is.null(x) || is.null(y)) {
    x <- graph.sizes[1] * .80
    y <- graph.sizes[2] / 40
    if (length(grep("left", position)) == 1) x <- graph.sizes[1] / 40
    if (length(grep("top", position)) == 1) y <- graph.sizes[2] - y
  }
  text(x = x, y = y, labels = labels, adj = 0, cex = cex, font = 2)
}

# ---- col.key ----------------------------------------------------------------

col.key <- function(discrete = FALSE,
                    limit = 1.0,
                    bins = 10,
                    cols = NULL,
                    both.dirs = TRUE,
                    low = "green",
                    mid = "gray",
                    high = "red",
                    graph.size,
                    node.size,
                    size.by.graph = TRUE,
                    key.pos = "topright",
                    off.sets = c(x = 0, y = 0),
                    align = "n",
                    cex = 1,
                    lwd = 1) {

  if (both.dirs && length(limit) == 1) {
    limit <- c(-abs(limit), abs(limit))
  } else if (length(limit) == 1) {
    limit <- c(0, limit)
  }
  disc.cond1 <- all(as.integer(limit) == limit)
  disc.cond2 <- (limit[2] - limit[1]) %% bins == 0
  discrete <- discrete & disc.cond1 & disc.cond2
  if (discrete) {
    limit[2] <- limit[2] + 1
    bins <- bins + 1
  }

  width <- graph.size[1]
  height <- graph.size[2]
  if (size.by.graph) {
    xs <- width / 80
    ys <- height / 40
  } else if (!missing(node.size)) {
    xs <- node.size[1] * 3 / bins
    ys <- node.size[2]
  } else {
    message("Note: ", "color key not plotted, node.size is needed\n when size.by.graph=FALSE!")
    return(off.sets)
  }

  if (align == "x") {
    off.sets["x"] <- 2 * xs
    off.sets["y"] <- off.sets["y"] + 3 * ys
  }
  if (align == "y") {
    off.sets <- off.sets + c(x = 3 * xs, y = 0)
  }
  if (align == "n") {
    off.sets <- off.sets + c(x = 2 * xs, y = 2 * ys)
  }
  if (length(grep("right", key.pos)) == 1) {
    off.sets["x"] <- off.sets["x"] + bins * xs
    x <- width - off.sets["x"]
  } else {
    x <- off.sets["x"]
    off.sets["x"] <- off.sets["x"] + bins * xs
  }
  if (length(grep("top", key.pos)) == 1) {
    y <- height - off.sets["y"]
  } else {
    y <- off.sets["y"]
  }

  ckx <- seq(x, x + bins * xs, length = bins + 1)
  cky <- c(y, y + ys)

  if (is.null(cols)) {
    if (both.dirs) {
      cols <- colorpanel2(bins, low = low, mid = mid, high = high)
    } else {
      cols <- colorpanel2(bins, low = mid, high = high)
    }
  }

  data.cuts <- seq(from = limit[1], to = limit[2], length = bins + 1)
  image(x = ckx, y = cky, z = cbind(data.cuts[-1]), col = cols, axes = FALSE, add = TRUE)
  if (!discrete) {
    label <- format(data.cuts[c(1, bins / 2 + 1, bins + 1)], digits = 2)
    text(x = seq(x, x + bins * xs, length = length(label)), y = rep(y - ys, length(label)), label = label, cex = cex)
  } else {
    label <- paste(as.integer(data.cuts[c(1, bins)]))
    text(x = seq(x, x + bins * xs, length = length(label)) + c(xs, -xs) / 2, y = rep(y - ys, length(label)), label = label, cex = cex)
  }
  cky <- c(y - 0.25 * ys, y + ys)
  for (i in 1:(bins + 1)) lines(rep(ckx[i], 2), cky, lwd = lwd)

  return(off.sets)
}

# ---- download.kegg ----------------------------------------------------------

download.kegg <- function(pathway.id = "00010",
                          species = "hsa",
                          kegg.dir = ".",
                          file.type = c("xml", "png")) {
  npath <- length(pathway.id)
  if (species != "ko") {
    species <- kegg.species.code(species, na.rm = TRUE)
  }
  nspec <- length(species)
  if (npath != 1 || nspec != 1) {
    species <- rep(species, npath)
    pathway.id <- rep(pathway.id, each = nspec)
  }
  pathway.id <- paste(species, pathway.id, sep = "")
  uidx <- !duplicated(pathway.id)
  pathway.id <- pathway.id[uidx]
  species <- species[uidx]
  npath <- length(pathway.id)
  xml.fnames <- paste(pathway.id, ".xml", sep = "")
  png.fnames <- paste(pathway.id, ".png", sep = "")
  xml.fmt <- "https://rest.kegg.jp/get/%s/kgml"
  png.fmt <- "https://rest.kegg.jp/get/%s/image"
  all.status <- rep("succeed", npath)
  names(all.status) <- pathway.id
  warn.fmt.xml <- "Download of %s xml file failed!\nThis pathway may not exist!"
  warn.fmt.png <- "Download of %s png file failed!\nThis pathway may not exist!"
  if ("xml" %in% file.type) {
    for (i in 1:npath) {
      msg <- sprintf("Downloading xml files for %s, %d/%d pathways..", pathway.id[i], i, length(pathway.id))
      message("Info: ", msg)
      xml.url <- sprintf(xml.fmt, pathway.id[i])
      xml.target <- sprintf("%s/%s", kegg.dir, xml.fnames[i])
      xml.status <- try(download.file(xml.url, xml.target, quiet = TRUE), silent = TRUE)
      if (xml.status != 0) {
        all.status[i] <- "failed"
      }
      if (class(xml.status)[1] == "try-error") {
        warn.msg <- sprintf(warn.fmt.xml, pathway.id[i])
        message("Warning: ", warn.msg)
        unlink(xml.target)
      }
    }
  }
  if ("png" %in% file.type) {
    for (i in 1:npath) {
      msg <- sprintf("Downloading png files for %s, %d/%d pathways..", pathway.id[i], i, length(pathway.id))
      message("Info: ", msg)
      png.url <- sprintf(png.fmt, pathway.id[i])
      png.target <- sprintf("%s/%s", kegg.dir, png.fnames[i])
      png.status <- suppressWarnings(try(download.file(png.url, png.target, quiet = TRUE, mode = "wb"), silent = TRUE))
      if (png.status != 0) {
        all.status[i] <- "failed"
      }
      if (class(png.status)[1] == "try-error") {
        warn.msg <- sprintf(warn.fmt.png, pathway.id[i])
        message("Warning: ", warn.msg)
        unlink(png.target)
      }
    }
  }
  return(all.status)
}

# ---- kegg.species.code ------------------------------------------------------

kegg.species.code <- function(species = "hsa",
                              na.rm = FALSE,
                              code.only = TRUE) {
  # Returns species metadata vector(s) for use by mypathview().
  # Validation is intentionally skipped — an invalid code will produce a clear
  # HTTP error when mypathview() tries to download the KEGG XML/PNG files.
  nspec <- length(species)

  results <- lapply(species, function(sp) {
    c(
      kegg.code      = sp,
      entrez.gnodes  = "1",
      kegg.geneid    = NA_character_,
      ncbi.geneid    = "1",
      ncbi.proteinid = NA_character_,
      uniprot        = NA_character_
    )
  })

  if (length(results) == 1) {
    info <- results[[1]]
  } else {
    info <- do.call(rbind, results)
  }

  if (code.only) return(info["kegg.code"])
  return(info)
}

# ---- node.color -------------------------------------------------------------

node.color <- function(plot.data = NULL,
                       discrete = FALSE,
                       limit = 1,
                       bins = 10,
                       both.dirs = TRUE,
                       low = "green",
                       mid = "gray",
                       high = "red",
                       na.col = "transparent",
                       trans.fun = NULL) {
  if (is.null(plot.data)) return(NULL)
  node.summary <- plot.data[, -c(1:8)]
  if (length(dim(node.summary)) == 2) {
    node.summary <- as.matrix(node.summary)
  } else {
    names(node.summary) <- rownames(plot.data)
  }
  if (!is.null(trans.fun)) node.summary <- trans.fun(node.summary)
  if (both.dirs && length(limit) == 1) {
    limit <- c(-abs(limit), abs(limit))
  } else if (length(limit) == 1) {
    limit <- c(0, limit)
  }
  disc.cond1 <- all(as.integer(limit) == limit)
  disc.cond2 <- (limit[2] - limit[1]) %% bins == 0
  if (discrete && disc.cond1 && disc.cond2) {
    node.summary[] <- as.integer(node.summary)
    limit[2] <- limit[2] + 1
    bins <- bins + 1
  } else if (discrete) {
    message("Note: ", "limit or bins not proper, data not treated as discrete!")
  }
  node.summary[node.summary > limit[2]] <- limit[2]
  node.summary[node.summary < limit[1]] <- limit[1]
  if (both.dirs) {
    cols <- colorpanel2(bins, low = low, mid = mid, high = high)
  } else {
    cols <- colorpanel2(bins, low = mid, high = high)
  }
  na.col <- colorpanel2(1, low = na.col, high = na.col)
  data.cuts <- seq(from = limit[1], to = limit[2], length = bins + 1)
  index.ts <- cols.ts <- node.summary
  index.ts[] <- cut(node.summary, data.cuts, include.lowest = TRUE, right = FALSE)
  cols.ts[] <- cols[index.ts]
  cols.ts[is.na(cols.ts)] <- na.col
  return(cols.ts)
}

# ---- node.info --------------------------------------------------------------

node.info <- function(object, short.name = TRUE) {
  cobj <- class(object)[1]
  if (cobj == "character") {
    object <- KEGGgraph::parseKGML(object)
    ndata <- KEGGgraph::nodes(object)
  } else if (cobj == "KEGGPathway") {
    ndata <- KEGGgraph::nodes(object)
  } else if (cobj == "graphNEL") {
    ndata <- KEGGgraph::getKEGGnodeData(object)
  } else {
    stop("object should be either a filename, KEGGPathway, or graphNEL!")
  }
  nodeNames <- sapply(ndata, KEGGgraph::getName)
  nodeType <- sapply(ndata, KEGGgraph::getType)
  nodeComp <- sapply(ndata, KEGGgraph::getComponent)
  node.size <- sapply(nodeComp, length)
  grs1 <- sapply(ndata, function(x) {
    grs <- x@graphics
    c(labels = grs@name, shape = grs@type)
  })
  grs2 <- sapply(ndata, function(x) {
    grs <- x@graphics
    c(x = grs@x, y = grs@y, width = grs@width, height = grs@height)
  })
  grs1 <- t(grs1)
  grs2 <- t(grs2)
  graphic.data <- as.list(cbind(data.frame(grs1, stringsAsFactors = FALSE), data.frame(grs2)))
  nd.list <- list(kegg.names = nodeNames, type = nodeType, component = nodeComp, size = node.size)
  nd.list <- c(nd.list, graphic.data)
  if (short.name) {
    gnames <- sapply(strsplit(nd.list$labels, ", "), "[[", 1)
    map.idx <- nd.list$type == "map"
    gnames[map.idx] <- nd.list$labels[map.idx]
    gnames[is.na(gnames)] <- ""
    gnames <- gsub("[.][.][.]", "", gnames)
    nd.list$labels <- gnames
    nd.list$kegg.names <- lapply(nd.list$kegg.names, function(x) gsub("^.*:", "", x))
  }
  nn <- names(nodeNames)
  nd.list <- lapply(nd.list, function(x) {
    names(x) <- nn
    return(x)
  })
  return(nd.list)
}

# ---- node.map ---------------------------------------------------------------

node.map <- function(mol.data = NULL,
                     node.data,
                     node.types = c("gene", "ortholog", "compound")[1],
                     node.sum = c("sum", "mean", "median", "max", "max.abs", "random")[1],
                     entrez.gnodes = TRUE) {
  type.sel <- node.data$type %in% node.types
  if (sum(type.sel) < 1) {
    message("Note: ", "No specified node types in the pathway!")
    plot.data <- NULL
    return(plot.data)
  }
  node.data <- lapply(node.data, "[", type.sel)
  n.nodes <- length(node.data$kegg.names)
  spacials <- as.matrix(as.data.frame(node.data[c("type", "x", "y", "width", "height")]))
  if (node.types[1] == "gene") {
    kng <- node.data$kegg.names[node.data$type == "gene"]
    kng.char <- gsub("[0-9]", "", unlist(kng))
    if (any(kng.char > "")) entrez.gnodes <- FALSE
  }
  na.plot.data <- function() {
    sapply(1:n.nodes, function(i) {
      kns <- node.data$kegg.names[[i]]
      if (node.types[1] == "gene" && entrez.gnodes) {
        items <- as.numeric(kns)
      } else {
        items <- kns
      }
      ord <- order(items)
      items <- items[ord]
      kns <- kns[ord]
      return(c(kns[1], "", spacials[i, ], NA))
    })
  }
  if (is.null(mol.data)) {
    plot.data <- na.plot.data()
  } else {
    if (is.character(mol.data)) {
      gd.names <- mol.data
      mol.data <- rep(1, length(mol.data))
      names(mol.data) <- gd.names
    }
    mol.data <- cbind(mol.data)
    if (is.null(colnames(mol.data))) colnames(mol.data) <- paste("ge", 1:ncol(mol.data), sep = "")
    mapped.mols <- intersect(unlist(node.data$kegg.names), row.names(mol.data))
    if (length(mapped.mols) == 0) {
      message("Warning: ", paste("None of the genes or compounds mapped to the pathway!",
        "Argument gene.idtype or cpd.idtype may be wrong.", sep = "\n"))
      plot.data <- na.plot.data()
    } else {
      if (node.types[1] == "gene" && entrez.gnodes) mapped.mols <- as.numeric(mapped.mols)
      plot.data <- sapply(1:n.nodes, function(i) {
        kns <- node.data$kegg.names[[i]]
        if (node.types[1] == "gene" && entrez.gnodes) {
          items <- as.numeric(kns)
        } else {
          items <- kns
        }
        ord <- order(items)
        items <- items[ord]
        kns <- kns[ord]
        hit <- items %in% mapped.mols
        if (sum(hit) == 0) {
          return(c(kns[1], "", spacials[i, ], rep(NA, ncol(mol.data))))
        } else if (sum(hit) == 1) {
          edata <- mol.data[as.character(items[hit]), ]
          return(c(kns[hit], kns[hit], spacials[i, ], edata))
        } else {
          node.sum <- eval(as.name(node.sum))
          edata <- apply(mol.data[as.character(items[hit]), , drop = FALSE], 2, function(x) {
            x <- x[!is.na(x)]
            if (length(x) < 1) return(NA)
            else return(node.sum(x, na.rm = FALSE))
          })
          return(c(kns[hit][1], paste(kns[hit], collapse = ","), spacials[i, ], edata))
        }
      })
    }
  }
  colnames(plot.data) <- names(node.data$kegg.names)
  plot.data <- as.data.frame(t(plot.data), stringsAsFactors = FALSE)
  plot.data$labels <- node.data$labels
  ncs <- ncol(plot.data)
  plot.data <- plot.data[, c(1, ncs, 2:(ncs - 1))]
  if (is.null(mol.data)) cns <- "mol.data" else cns <- colnames(mol.data)
  colnames(plot.data)[c(1, 3, 9:ncs)] <- c("kegg.names", "all.mapped", cns)
  for (ic in (1:ncol(plot.data))[-c(1:4)]) plot.data[, ic] <- as.numeric(plot.data[, ic])
  return(plot.data)
}

# ---- eg2id ------------------------------------------------------------------

eg2id <- function(eg,
                  category = "symbol",
                  org = "Hs",
                  pkg.name = NULL, ...) {
  category <- toupper(category)
  if (category %in% c("ENTREZ", "EG", "ENTREZID")) {
    stop("output ID or category cannot be Entrez Gene ID!")
  }
  if (is.null(pkg.name)) {
    stop("pkg.name (annotation package) must be provided!")
  }
  pkg <- tryCatch(
    get(pkg.name, envir = asNamespace(pkg.name)),
    error = function(e) NULL
  )
  if (is.null(pkg)) {
    message("Warning: Annotation package '", pkg.name, "' is not installed — gene symbols not mapped, Entrez IDs used for node labels.")
    return(cbind(eg, as.character(eg)))
  }
  mapped <- AnnotationDbi::mapIds(
    pkg,
    keys    = as.character(eg),
    column  = category,
    keytype = "ENTREZID",
    multiVals = "first"
  )
  cbind(eg, as.character(mapped))
}

# ---- keggview.graph ---------------------------------------------------------

keggview.graph <- function(plot.data.gene = NULL,
                           plot.data.cpd = NULL,
                           cols.ts.gene = NULL,
                           cols.ts.cpd = NULL,
                           node.data,
                           path.graph,
                           pathway.name,
                           out.suffix = "pathview",
                           pdf.size = c(7, 7),
                           multi.state = TRUE,
                           same.layer = TRUE,
                           match.data = TRUE,
                           rankdir = c("LR", "TB")[1],
                           is.signal = TRUE,
                           split.group = FALSE,
                           afactor = 1,
                           text.width = 15,
                           cex = 0.5,
                           map.cpdname = FALSE,
                           cpd.lab.offset = 1.0,
                           discrete = list(gene = FALSE, cpd = FALSE),
                           limit = list(gene = 1, cpd = 1),
                           bins = list(gene = 10, cpd = 10),
                           both.dirs = list(gene = TRUE, cpd = TRUE),
                           low = list(gene = "green", cpd = "blue"),
                           mid = list(gene = "gray", cpd = "gray"),
                           high = list(gene = "red", cpd = "yellow"),
                           na.col = "transparent",
                           new.signature = TRUE,
                           plot.col.key = TRUE,
                           key.align = "x",
                           key.pos = "topright",
                           sign.pos = "bottomright",
                           ...) {
  gR1 <- path.graph
  grp.idx <- node.data$size > 1
  if (sum(grp.idx) > 0 && !split.group) {
    sub2grp <- cbind(
      unlist(node.data$component[grp.idx], use.names = FALSE),
      rep(names(grp.idx)[grp.idx], node.data$size[grp.idx])
    )
    du.idx <- duplicated(sub2grp[, 1])
    if (sum(du.idx) > 0) {
      du.rn <- sub2grp[, 1] %in% sub2grp[du.idx, 1]
      message("Warning: reconcile groups sharing member nodes!")
      print(sub2grp[du.rn, ])
      du.grps <- sub2grp[du.idx, ]
      rn <- which(du.idx)
      for (r in rn) {
        comps <- node.data$component[[sub2grp[r, 2]]]
        comps <- comps[comps != sub2grp[r, 1]]
        node.data$component[[sub2grp[r, 2]]] <- comps
        node.data$size[sub2grp[r, 2]] <- node.data$size[sub2grp[r, 2]] - 1
      }
      sub2grp <- sub2grp[!du.idx, ]
    }
    rownames(sub2grp) <- sub2grp[, 1]
  } else {
    sub2grp <- NULL
  }
  if (sum(grp.idx) > 0 && !split.group) {
    for (gn in names(grp.idx)[grp.idx]) {
      gR1 <- combineKEGGnodes(node.data$component[[gn]], gR1, gn)
    }
  } else if (split.group) {
    gR1 <- subGraph(nodes(gR1)[node.data$size == 1], gR1)
  }
  nNames <- nodes(gR1)
  nSizes <- node.data$size[nNames]
  deg <- degree(gR1)
  deg <- deg$inDegree + deg$outDegree
  if (is.signal && sum(deg < 1) > 0) {
    gR2 <- subKEGGgraph(nNames[deg > 0], gR1)
    nNames <- nNames[deg > 0]
    nSizes <- nSizes[deg > 0]
    if (!is.null(sub2grp)) {
      sub.idx <- sub2grp[, 1] %in% nNames | sub2grp[, 2] %in% nNames
    } else {
      sub.idx <- 0
    }
  } else {
    gR2 <- gR1
    if (!is.null(sub2grp)) {
      sub.idx <- rep(TRUE, nrow(sub2grp))
    } else {
      sub.idx <- 0
    }
  }
  if (length(nNames) < 2) {
    msg <- sprintf("%s not rendered, 0 or 1 connected nodes!\nTry \"kegg.native=T\" instead!", pathway.name)
    message("Note: ", msg)
    return(list())
  }
  attrs <- list()
  attrs$graph$rankdir <- "LR"
  attrs$node <- list(fixedsize = FALSE)
  ntype <- node.data$type[nNames]
  cpd.idx <- ntype == "compound"
  map.idx <- ntype == "map"
  rect.idx <- !(cpd.idx | map.idx)
  nAttr <- list()
  nAttr$label <- rep("", length(nNames))
  shapes <- node.data$shape[nNames]
  if (any(cpd.idx)) shapes[cpd.idx] <- "ellipse"
  if (any(map.idx)) shapes[map.idx] <- "plaintext"
  nAttr$shape <- shapes
  nAttr$height <- .75 * 17 / 46 * nSizes * afactor
  nAttr$width <- rep(.75, length(nNames)) * afactor
  if (any(cpd.idx)) {
    nAttr$height[cpd.idx] <- nAttr$height[cpd.idx] * 1.5
    nAttr$width[cpd.idx] <- nAttr$width[cpd.idx] * 1.5
  }
  if (any(map.idx)) {
    nAttr$height[map.idx] <- nAttr$height[map.idx] * 1.5
    nAttr$width[map.idx] <- nAttr$width[map.idx] * 2
  }
  nAttr <- lapply(nAttr, function(x) {
    names(x) <- nNames
    x
  })
  na.col <- colorpanel2(1, low = na.col, high = na.col)
  fillcol <- rep(na.col, length(nNames))
  names(fillcol) <- nNames
  subdisplay <- subtypeDisplay(gR2)
  if (length(subdisplay) < 1) {
    eAttrs <- list()
  } else {
    na.rn <- apply(subdisplay, 2, function(x) sum(is.na(x)) == 7)
    if (sum(na.rn) > 0) {
      subdisplay[, na.rn] <- KEGGEdgeSubtype[KEGGEdgeSubtype[, 1] == "others", rownames(subdisplay)]
    }
    eLabel <- subdisplay["label", ]
    eCol <- subdisplay["color", ]
    eTextCol <- subdisplay["fontcolor", ]
    eLty <- subdisplay["style", ]
    eArrowhead <- subdisplay["arrowhead", ]
    if (ncol(subdisplay) == 1) {
      tmp <- colnames(subdisplay)[1]
      names(eLabel) <- names(eCol) <- names(eTextCol) <- tmp
      names(eLty) <- names(eArrowhead) <- tmp
    }
    eAttrs <- list(lty = eLty, col = eCol, textCol = eTextCol, label = eLabel, arrowhead = eArrowhead)
  }
  gR2.layout <- gR2
  edgeRenderInfo(gR2.layout) <- eAttrs
  layoutType <- ifelse(is.signal, "dot", "neato")
  gR2.layout <- layoutGraph(gR2.layout, attrs = attrs, nodeAttrs = nAttr, layoutType = layoutType)
  edgeRenderInfo(gR2.layout) <- eAttrs
  nri <- nodeRenderInfo(gR2.layout)
  loc <- list(x = nri$nodeX, y = nri$nodeY)
  if (sum(rect.idx) > 0) {
    w.unit <- min(nri$lWidth[rect.idx])
    h.unit <- min(nri$height[rect.idx])
  }
  cni <- nSizes > 1
  if (sum(cni) > 0) {
    xloc <- rep(loc[[1]][cni], nSizes[cni])
    sn.y <- unlist(sapply(nSizes[cni], function(x) seq(-(x - 1) / 2, (x - 1) / 2, 1)), use.names = FALSE)
    yloc <- rep(loc[[2]][cni], nSizes[cni]) + h.unit * sn.y
  } else {
    xloc <- yloc <- NULL
  }
  xloc.nd <- c(xloc, loc[[1]][nSizes == 1 & rect.idx])
  yloc.nd <- c(yloc, loc[[2]][nSizes == 1 & rect.idx])
  labs <- node.data$labels
  labs[nNames[map.idx]] <- sapply(labs[nNames[map.idx]], wordwrap, width = text.width, break.word = FALSE)
  labs[nNames[cpd.idx]] <- sapply(labs[nNames[cpd.idx]], wordwrap, width = text.width, break.word = TRUE)
  cols.ts.gene <- cbind(cols.ts.gene)
  cols.ts.cpd <- cbind(cols.ts.cpd)
  nc.gene <- max(ncol(cols.ts.gene), 0)
  nc.cpd <- max(ncol(cols.ts.cpd), 0)
  nplots <- max(nc.gene, nc.cpd)
  pn.suffix <- colnames(cols.ts.gene)
  if (length(pn.suffix) < nc.cpd) pn.suffix <- colnames(cols.ts.cpd)
  if (length(pn.suffix) < nplots) pn.suffix <- 1:nplots
  if (length(pn.suffix) == 1) {
    pn.suffix <- out.suffix
  } else {
    pn.suffix <- paste(out.suffix, pn.suffix, sep = ".")
  }
  if ((match.data || !multi.state) && nc.gene != nc.cpd) {
    if (nc.gene > nc.cpd && !is.null(cols.ts.cpd)) {
      na.mat <- matrix(na.col, ncol = nplots - nc.cpd, nrow = nrow(cols.ts.cpd))
      cols.ts.cpd <- cbind(cols.ts.cpd, na.mat)
    }
    if (nc.gene < nc.cpd && !is.null(cols.ts.gene)) {
      na.mat <- matrix(na.col, ncol = nplots - nc.gene, nrow = nrow(cols.ts.gene))
      cols.ts.gene <- cbind(cols.ts.gene, na.mat)
    }
    nc.gene <- nc.cpd <- nplots
  }
  if (!is.null(cols.ts.gene)) {
    nidx.gene <- which(nNames %in% rownames(cols.ts.gene))
    cidx.gene <- match(nNames[nidx.gene], rownames(cols.ts.gene))
    sci.gene <- match(sub2grp[sub.idx, 1], rownames(cols.ts.gene))
    sci.node <- match(sub2grp[sub.idx, 1], nNames)
  }
  if (!is.null(cols.ts.cpd)) {
    nidx.cpd <- which(nNames %in% rownames(cols.ts.cpd))
    cidx.cpd <- match(nNames[nidx.cpd], rownames(cols.ts.cpd))
  }
  out.fmt <- "Working in directory %s"
  wdir <- getwd()
  out.msg <- sprintf(out.fmt, wdir)
  message("Info: ", out.msg)
  out.fmt <- "Writing image file %s"
  if (sum(rect.idx) > 0) {
    cn.col <- rep(NA, sum(sub.idx))
    cn.col <- fillcol[sci.node]
    names(cn.col) <- sub2grp[sub.idx, 1]
    rect.col <- c(cn.col, fillcol[nSizes == 1 & rect.idx])
    rect.col[rect.col == na.col] <- NA
    rect.col <- matrix(rep(rect.col, nplots), ncol = nplots)
  }
  if (sum(cpd.idx) > 0) {
    ell.col <- fillcol[cpd.idx]
    ell.col[ell.col == na.col] <- NA
    ell.col <- matrix(rep(ell.col, nplots), ncol = nplots)
    w.e <- min(nri$lWidth[cpd.idx])
    h.e <- min(nri$height[cpd.idx])
    xloc.e <- loc[[1]][cpd.idx]
    yloc.e <- loc[[2]][cpd.idx]
  }
  fillcol <- matrix(na.col, nrow = length(nNames), ncol = nplots)
  rownames(fillcol) <- nNames
  if (!is.null(cols.ts.gene) && sum(rect.idx) > 0) {
    fillcol[nidx.gene, 1:nc.gene] <- cols.ts.gene[cidx.gene, ]
    cn.col <- matrix(NA, nrow = sum(sub.idx), ncol = nc.gene)
    cn.col[] <- cols.ts.gene[sci.gene, ]
    rownames(cn.col) <- sub2grp[sub.idx, 1]
    if (nc.gene > 1) {
      rect.col <- rbind(cn.col, fillcol[nSizes == 1 & rect.idx, 1:nc.gene])
    } else {
      rect.col <- c(cn.col, fillcol[nSizes == 1 & rect.idx, 1])
    }
    rect.col[rect.col == na.col] <- NA
  }
  if (!is.null(cols.ts.cpd) && sum(cpd.idx) > 0) {
    fillcol[nidx.cpd, 1:nc.cpd] <- cols.ts.cpd[cidx.cpd, ]
    if (sum(cpd.idx) > 0) {
      ell.col <- fillcol[cpd.idx, 1:nc.cpd]
      ell.col[ell.col == na.col] <- NA
    }
  }
  multi.state <- multi.state & nplots > 1
  if (multi.state) {
    nplots <- 1
    pn.suffix <- paste(out.suffix, "multi", sep = ".")
    if (sum(rect.idx > 0)) rect.col.plot <- rect.col
    if (sum(cpd.idx) > 0) ell.col.plot <- ell.col
  }
  for (np in 1:nplots) {
    gfile <- paste(pathway.name, pn.suffix[np], "pdf", sep = ".")
    out.msg <- sprintf(out.fmt, gfile)
    message("Info: ", out.msg)
    ntypes <- length(unique(node.data$type[nNames]))
    etypes <- nrow(unique(t(subdisplay)))
    if (!same.layer) {
      kl.type <- "both"
    } else {
      if (ntypes < 3 && sum(cpd.idx) < 3) {
        kl.type <- "edge"
      } else if (etypes < 3) {
        kl.type <- "node"
      } else {
        kl.type <- "both"
        same.layer <- FALSE
      }
    }
    pdf.width <- ifelse(same.layer, 1.5, 1) * pdf.size[1]
    pdf(gfile, width = pdf.width, height = pdf.size[2])
    op <- par(no.readonly = TRUE)
    if (same.layer) nf <- layout(cbind(1, 2), c(2, 1))
    rg <- renderGraph(gR2.layout)
    gri <- graphRenderInfo(rg)
    par(mai = gri$mai, usr = gri$usr)
    if (sum(rect.idx) > 0) {
      if (!multi.state) rect.col.plot <- cbind(rect.col)[, np]
      rect.out <- sliced.shapes(xloc.nd, yloc.nd, w.unit, h.unit / 2, cols = rect.col.plot, shape = "rectangle")
    }
    if (sum(cpd.idx) > 0) {
      if (!multi.state) ell.col.plot <- cbind(ell.col)[, np]
      ell.out <- sliced.shapes(xloc.e, yloc.e, w.e, h.e / 2, cols = ell.col.plot, shape = "ellipse")
    }
    if (sum(cni) > 0) {
      if (sum(sub.idx) > 0) text(xloc, yloc, label = labs[sub2grp[sub.idx, 1]], cex = cex)
    }
    if (sum(!cpd.idx) > 0) {
      text(loc[[1]][!cpd.idx], loc[[2]][!cpd.idx], label = labs[nNames[!cpd.idx]], cex = cex)
    }
    if (sum(cpd.idx) > 0) {
      if (map.cpdname && !is.null(cols.ts.cpd)) {
        yloc.et <- yloc.e + h.e * cpd.lab.offset
      } else {
        yloc.et <- yloc.e
      }
      text(xloc.e, yloc.et, label = labs[nNames[cpd.idx]], cex = cex)
    }
    pv.pars <- list()
    pv.pars$gsizes <- c(gri$bbox[2, 1], gri$bbox[2, 2])
    if (sum(rect.idx) > 0) {
      pv.pars$nsizes <- c(w.unit, h.unit)
    } else {
      pv.pars$nsizes <- c(w.e, h.e)
    }
    pv.pars$op <- op
    pv.pars$key.cex <- cex * 1.5
    pv.pars$key.lwd <- 1
    pv.pars$sign.cex <- 1.2 * cex
    off.sets <- c(x = 0, y = 0)
    align <- "n"
    ucol.gene <- unique(as.vector(cols.ts.gene))
    na.col.gene <- ucol.gene %in% c(na.col, NA)
    if (plot.col.key && !is.null(cols.ts.gene) && !all(na.col.gene)) {
      off.sets <- col.key(
        limit = limit$gene, bins = bins$gene, both.dirs = both.dirs$gene,
        discrete = discrete$gene, graph.size = pv.pars$gsizes, node.size = pv.pars$nsizes,
        key.pos = key.pos, cex = pv.pars$key.cex, lwd = pv.pars$key.lwd,
        low = low$gene, mid = mid$gene, high = high$gene, align = "n"
      )
      align <- key.align
    }
    ucol.cpd <- unique(as.vector(cols.ts.cpd))
    na.col.cpd <- ucol.cpd %in% c(na.col, NA)
    if (plot.col.key && !is.null(cols.ts.cpd) && !all(na.col.cpd)) {
      off.sets <- col.key(
        limit = limit$cpd, bins = bins$cpd, both.dirs = both.dirs$cpd,
        discrete = discrete$cpd, graph.size = pv.pars$gsizes,
        node.size = pv.pars$nsizes, key.pos = key.pos, off.sets = off.sets,
        cex = pv.pars$key.cex, lwd = pv.pars$key.lwd, low = low$cpd,
        mid = mid$cpd, high = high$cpd, align = align
      )
    }
    if (new.signature) {
      pathview.stamp(position = sign.pos, graph.sizes = pv.pars$gsizes, on.kegg = FALSE, cex = pv.pars$sign.cex)
    }
    kegg.legend(type = kl.type)
    par(pv.pars$op)
    dev.off()
  }
  return(invisible(pv.pars))
}

# ---- mol.sum ----------------------------------------------------------------

mol.sum <- function(mol.data,
                    id.map,
                    gene.annotpkg = "org.Hs.eg.db",
                    sum.method = c("sum", "mean", "median", "max", "max.abs", "random")[1]) {
  if (is.character(mol.data)) {
    gd.names <- mol.data
    mol.data <- rep(1, length(mol.data))
    names(mol.data) <- gd.names
    ng <- length(mol.data)
  } else if (!is.null(mol.data)) {
    if (length(dim(mol.data)) == 2) {
      gd.names <- rownames(mol.data)
      ng <- nrow(mol.data)
    } else if (is.numeric(mol.data) && is.null(dim(mol.data))) {
      gd.names <- names(mol.data)
      ng <- length(mol.data)
    } else {
      stop("wrong mol.data format!")
    }
  } else {
    stop("NULL mol.data!")
  }
  if (is.character(id.map) && length(id.map) == 1) {
    id.map <- id2eg(gd.names, category = id.map, pkg.name = gene.annotpkg)
  }
  sel.idx <- id.map[, 2] > "" & !is.na(id.map[, 2])
  id.map <- id.map[sel.idx, , drop = FALSE]
  eff.idx <- gd.names %in% id.map[, 1]
  mapped.ids <- id.map[match(gd.names[eff.idx], id.map[, 1]), 2]
  if (sum(eff.idx) < 1) {
    stop("no ID can be mapped!")
  } else if (sum(eff.idx) == 1) {
    mapped.data <- cbind(mol.data)[eff.idx, , drop = FALSE]
    rownames(mapped.data) <- mapped.ids[1]
  } else {
    if (sum.method %in% c("sum", "mean")) {
      sum.method <- eval(as.name(sum.method))
      mapped.data <- apply(cbind(mol.data)[eff.idx, , drop = FALSE], 2, function(x) {
        sum.res <- tapply(x, mapped.ids, sum.method, na.rm = TRUE)
        return(sum.res)
      })
      if (length(unique(mapped.ids)) == 1) {
        if (length(mapped.data) > 1) {
          mapped.data <- rbind(mapped.data)
          rownames(mapped.data) <- mapped.ids[1]
        } else {
          names(mapped.data) <- mapped.ids[1]
        }
      }
    } else {
      sum.method <- eval(as.name(sum.method))
      mol.data <- cbind(mol.data)[eff.idx, , drop = FALSE]
      if (all(mol.data >= 0) || all(mol.data <= 0)) {
        vars <- apply(cbind(mol.data), 1, IQR)
      } else {
        vars <- apply(cbind(mol.data), 1, sum, na.rm = TRUE)
      }
      sel.rn <- tapply(1:sum(eff.idx), mapped.ids, function(x) {
        if (length(x) == 1) return(x)
        else return(x[which.min(abs(vars[x] - sum.method(vars[x], na.rm = TRUE)))])
      })
      mapped.data <- mol.data[sel.rn, , drop = FALSE]
      rownames(mapped.data) <- names(sel.rn)
    }
  }
  return(mapped.data)
}

# ---- mypathview -------------------------------------------------------------

mypathview <- function(gene.data = NULL,
                       cpd.data = NULL,
                       pathway.id,
                       species = "hsa",
                       kegg.dir = ".",
                       cpd.idtype = "kegg",
                       gene.idtype = "entrez",
                       gene.annotpkg = NULL,
                       min.nnodes = 3,
                       kegg.native = TRUE,
                       map.null = TRUE,
                       expand.node = FALSE,
                       split.group = FALSE,
                       map.symbol = TRUE,
                       map.cpdname = TRUE,
                       node.sum = "sum",
                       discrete = list(gene = FALSE, cpd = FALSE),
                       limit = list(gene = 1, cpd = 1),
                       bins = list(gene = 10, cpd = 10),
                       both.dirs = list(gene = TRUE, cpd = TRUE),
                       trans.fun = list(gene = NULL, cpd = NULL),
                       low = list(gene = "green", cpd = "blue"),
                       mid = list(gene = "gray", cpd = "gray"),
                       high = list(gene = "red", cpd = "yellow"),
                       na.col = "transparent",
                       ...) {
  dtypes <- !is.null(gene.data) + (!is.null(cpd.data))
  cond0 <- dtypes == 1 & is.numeric(limit) & length(limit) > 1
  if (cond0) {
    if (limit[1] != limit[2] && is.null(names(limit))) {
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
    if (length(ns) == 0 || !all(c("gene", "cpd") %in% ns)) {
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
    } else if (is.numeric(gene.data) && is.null(dim(gene.data))) {
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

  # bods: organism -> annotation package mapping
  # Source: pathview R package (Bioconductor version 3.22)
  # Note: static internal copy for use without pathview
  bods <- data.frame(
    species = c(
      "Anopheles", "Arabidopsis", "Bovine", "Worm", "Canine",
      "Fly", "Zebrafish", "E coli strain K12", "E coli strain Sakai",
      "Chicken", "Human", "Mouse", "Rhesus", "Malaria",
      "Chimp", "Rat", "Yeast", "Pig", "Xenopus"
    ),
    kegg.code = c(
      "aga", "ath", "bta", "cel", "cfa",
      "dme", "dre", "eco", "ecs",
      "gga", "hsa", "mmu", "mcc", "pfa",
      "ptr", "rno", "sce", "ssc", "xla"
    ),
    pkg = c(
      "org.Ag.eg.db", "org.At.tair.db", "org.Bt.eg.db", "org.Ce.eg.db", "org.Cf.eg.db",
      "org.Dm.eg.db", "org.Dr.eg.db", "org.EcK12.eg.db", "org.EcSakai.eg.db",
      "org.Gg.eg.db", "org.Hs.eg.db", "org.Mm.eg.db", "org.Mmu.eg.db", "org.Pf.plasmo.db",
      "org.Pt.eg.db", "org.Rn.eg.db", "org.Sc.sgd.db", "org.Ss.eg.db", "org.Xl.eg.db"
    ),
    id.type = c(
      "eg", "tair", "eg", "eg", "eg",
      "eg", "eg", "eg", "eg",
      "eg", "eg", "eg", "eg", "orf",
      "eg", "eg", "orf", "eg", "eg"
    ),
    stringsAsFactors = FALSE
  )

  if (species != "ko") {
    species.data <- kegg.species.code(
      species,
      na.rm = TRUE,
      code.only = FALSE
    )
  } else {
    species.data <- c(
      kegg.code = "ko",
      entrez.gnodes = "0",
      kegg.geneid = "K01488",
      ncbi.geneid = NA,
      ncbi.proteinid = NA,
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
    idx <- match(species, bods$kegg.code)
    gene.annotpkg <- if (!is.na(idx)) bods$pkg[idx] else NA_character_
  }
  if (
    length(grep("ENTREZ|KEGG|NCBIPROT|UNIPROT", gene.idtype)) < 1 && !is.null(gene.data)
  ) {
    if (is.na(gene.annotpkg)) {
      stop("No proper gene annotation package available!")
    }
    if (!gene.idtype %in% gene.idtype.bods[[species]]) {
      stop("Wrong input gene ID type!")
    }
    gene.idmap <- id2eg(
      gd.names,
      category = gene.idtype,
      pkg.name = gene.annotpkg,
      unique.map = F
    )
    gene.data <- mol.sum(gene.data, gene.idmap)
    gene.idtype <- "ENTREZ"
  }
  if (gene.idtype != "KEGG" && !entrez.gnodes && !is.null(gene.data)) {
    id.type <- gene.idtype
    if (id.type == "ENTREZ") {
      id.type <- "ENTREZID"
    }
    kid.map <- names(species.data)[-c(1:2)]
    kid.types <- names(kid.map) <- c(
      "KEGG", "ENTREZID", "NCBIPROT", "UNIPROT"
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
    } else if (is.numeric(cpd.data) && is.null(dim(cpd.data))) {
      cpdd.names <- names(cpd.data)
      ncpd <- length(cpd.data)
    } else {
      stop("wrong cpd.data format!")
    }
  }
  if (length(grep("kegg", cpd.idtype)) < 1 && !is.null(cpd.data)) {
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
        species = species,
        kegg.dir = kegg.dir,
        file.type = ttype
      )
      if (dstatus == "failed") {
        warn.fmt <- "Failed to download KEGG xml/png files, %s skipped!"
        warn.msg <- sprintf(warn.fmt, pathway.name[i])
        message("Warning: ", warn.msg)
        return(invisible(0))
      }
    }
    if (kegg.native) {
      node.data <- try(node.info(xml.file[i]), silent = TRUE)
      if (class(node.data) == "try-error") {
        warn.msg <- sprintf(warn.fmt, xml.file[i])
        message("Warning: ", warn.msg)
        return(invisible(0))
      }
      node.type <- c("gene", "enzyme", "compound", "ortholog")
      sel.idx <- node.data$type %in% node.type
      nna.idx <- !is.na(
        node.data$x + node.data$y + node.data$width + node.data$height
      )
      sel.idx <- sel.idx & nna.idx
      if (sum(sel.idx) < min.nnodes) {
        warn.fmt <- "Number of mappable nodes is below %d, %s skipped!"
        warn.msg <- sprintf(warn.fmt, min.nnodes, pathway.name[i])
        message("Warning: ", warn.msg)
        return(invisible(0))
      }
      node.data <- lapply(node.data, "[", sel.idx)
    } else {
      gR1 <- try(
        parseKGML2Graph2(
          xml.file[i],
          genes = FALSE,
          expand = expand.node,
          split.group = split.group
        ),
        silent = TRUE
      )
      node.data <- try(
        node.info(gR1),
        silent = TRUE
      )
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
    if ((
      !is.null(gene.data) || map.null) && sum(node.data$type == gene.node.type) > 1
    ) {
      plot.data.gene <- node.map(
        gene.data,
        node.data,
        node.types = gene.node.type,
        node.sum = node.sum,
        entrez.gnodes = entrez.gnodes
      )
      kng <- plot.data.gene$kegg.names
      kng.char <- gsub("[0-9]", "", unlist(kng))
      if (any(kng.char > "")) {
        entrez.gnodes <- FALSE
      }
      if (!is.null(gene.data) && nrow(plot.data.gene) > 0) {
        matched <- sum(!is.na(plot.data.gene[, 9]))
        if (matched > 0 && matched / nrow(plot.data.gene) < 0.05) {
          message(
            "Warning: Very few genes mapped to pathway ", pathway.name[i],
            " (", matched, " of ", nrow(plot.data.gene), " nodes). ",
            "If gene nodes appear mostly gray, this may indicate a gene ID ",
            "mismatch for species '", species, "'."
          )
        }
      }
      if (map.symbol && species != "ko" && entrez.gnodes) {
        if (is.na(gene.annotpkg)) {
          warn.fmt <- "No annotation package for the species %s, gene symbols not mapped!"
          warn.msg <- sprintf(warn.fmt, species)
          message("Warning: ", warn.msg)
        } else {
          plot.data.gene$labels <- NA
          plot.data.gene$labels <- eg2id(
            as.character(plot.data.gene$kegg.names),
            category = "SYMBOL",
            pkg.name = gene.annotpkg
          )[, 2]
          mapped.gnodes <- rownames(plot.data.gene)
          node.data$labels[mapped.gnodes] <- plot.data.gene$labels
        }
      }
      cols.ts.gene <- node.color(
        plot.data.gene,
        limit$gene,
        bins$gene,
        both.dirs = both.dirs$gene,
        trans.fun = trans.fun$gene,
        discrete = discrete$gene,
        low = low$gene,
        mid = mid$gene,
        high = high$gene,
        na.col = na.col
      )
    } else {
      plot.data.gene <- cols.ts.gene <- NULL
    }
    if ((
      !is.null(cpd.data) || map.null) && sum(node.data$type == "compound") > 1
    ) {
      plot.data.cpd <- node.map(
        cpd.data,
        node.data,
        node.types = "compound",
        node.sum = node.sum
      )
      if (map.cpdname && !kegg.native) {
        plot.data.cpd$labels <- cpdkegg2name(plot.data.cpd$labels)[, 2]
        mapped.cnodes <- rownames(plot.data.cpd)
        node.data$labels[mapped.cnodes] <- plot.data.cpd$labels
      }
      cols.ts.cpd <- node.color(
        plot.data.cpd,
        limit$cpd,
        bins$cpd,
        both.dirs = both.dirs$cpd,
        trans.fun = trans.fun$cpd,
        discrete = discrete$cpd,
        low = low$cpd,
        mid = mid$cpd,
        high = high$cpd,
        na.col = na.col
      )
    } else {
      plot.data.cpd <- cols.ts.cpd <- NULL
    }
    if (kegg.native) {
      pv.pars <- my.keggview.native(
        plot.data.gene = plot.data.gene,
        cols.ts.gene = cols.ts.gene,
        plot.data.cpd = plot.data.cpd,
        cols.ts.cpd = cols.ts.cpd,
        node.data = node.data,
        pathway.name = pathway.name[i],
        kegg.dir = kegg.dir,
        limit = limit,
        bins = bins,
        both.dirs = both.dirs,
        discrete = discrete,
        low = low,
        mid = mid,
        high = high,
        na.col = na.col,
        ...
      )
    } else {
      cat("Skipped KEGG native rendering for pathway ", pathway.name[i], "!\n")
    }
    plot.data.gene <- cbind(plot.data.gene, cols.ts.gene)
    if (!is.null(plot.data.gene)) {
      cnames <- colnames(plot.data.gene)[-(1:8)]
      nsamp <- length(cnames) / 2
      if (nsamp > 1) {
        cnames[(nsamp + 1):(2 * nsamp)] <- paste(
          cnames[(nsamp + 1):(2 * nsamp)], "col",
          sep = "."
        )
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
        cnames[(nsamp + 1):(2 * nsamp)] <- paste(
          cnames[(nsamp + 1):(2 * nsamp)], "col",
          sep = "."
        )
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
}

# ---- my.keggview.native -----------------------------------------------------

my.keggview.native <- function(plot.data.gene = NULL,
                               plot.data.cpd = NULL,
                               cols.ts.gene = NULL,
                               cols.ts.cpd = NULL,
                               node.data,
                               pathway.name,
                               out.suffix = "pathview",
                               kegg.dir = ".",
                               multi.state = TRUE,
                               match.data = TRUE,
                               same.layer = TRUE,
                               res = 400,
                               cex = 0.25,
                               discrete = list(gene = FALSE, cpd = FALSE),
                               limit = list(gene = 1, cpd = 1),
                               bins = list(gene = 10, cpd = 10),
                               both.dirs = list(gene = TRUE, cpd = TRUE),
                               low = list(gene = "green", cpd = "blue"),
                               mid = list(gene = "gray", cpd = "gray"),
                               high = list(gene = "red", cpd = "yellow"),
                               na.col = "transparent",
                               new.signature = TRUE,
                               plot.col.key = TRUE,
                               key.align = "x",
                               key.pos = "topright",
                               ...) {
  img <- png::readPNG(
    paste(kegg.dir, "/", pathway.name, ".png", sep = "")
  )
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
  if ((match.data || !multi.state) && nc.gene != nc.cpd) {
    if (nc.gene > nc.cpd && !is.null(cols.ts.cpd)) {
      na.mat <- matrix(na.col, ncol = nplots - nc.cpd, nrow = nrow(cols.ts.cpd))
      cols.ts.cpd <- cbind(cols.ts.cpd, na.mat)
    }
    if (nc.gene < nc.cpd && !is.null(cols.ts.gene)) {
      na.mat <- matrix(
        na.col,
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
    img.file <- paste(
      kegg.dir,
      "/",
      pathway.name,
      ".",
      pn.suffix[np],
      ".png",
      sep = ""
    )
    out.msg <- sprintf(out.fmt, img.file)
    message("Info: ", out.msg)
    png(img.file, width = width, height = height, res = res)
    op <- par(mar = c(0, 0, 0, 0))
    plot(
      c(0, width),
      c(0, height),
      type = "n",
      xlab = "",
      ylab = "",
      xaxs = "i",
      yaxs = "i"
    )
    if (new.signature) {
      img[height - 4:25, 17:137, 1:3] <- 1
    }
    if (!same.layer) {
      rasterImage(img, 0, 0, width, height, interpolate = F)
    }
    if (!is.null(cols.ts.gene) && nc.gene >= np) {
      if (!multi.state) {
        cols.gene.plot <- cols.ts.gene[, np]
      }
      if (!same.layer) {
        render.kegg.node(
          plot.data.gene,
          cols.gene.plot,
          img,
          same.layer = same.layer,
          type = "gene",
          cex = cex
        )
      } else {
        # Manually set the width and height of gene boxes to fix a rendering
        # error found in some pathways. 9/6/2023 xj
        plot.data.gene$width <- 46
        plot.data.gene$height <- 17
        img <- render.kegg.node(
          plot.data.gene,
          cols.gene.plot,
          img,
          same.layer = same.layer,
          type = "gene"
        )
      }
    }
    if (!is.null(cols.ts.cpd) && nc.cpd >= np) {
      if (!multi.state) {
        cols.cpd.plot <- cols.ts.cpd[, np]
      }
      if (!same.layer) {
        render.kegg.node(
          plot.data.cpd,
          cols.cpd.plot,
          img,
          same.layer = same.layer,
          type = "compound",
          cex = cex
        )
      } else {
        img <- render.kegg.node(
          plot.data.cpd,
          cols.cpd.plot,
          img,
          same.layer = same.layer,
          type = "compound"
        )
      }
    }
    if (same.layer) {
      graphics::rasterImage(img, 0, 0, width, height, interpolate = FALSE)
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
    if (plot.col.key && !is.null(cols.ts.gene) && !all(na.col.gene)) {
      off.sets <- col.key(
        limit = limit$gene,
        bins = bins$gene,
        both.dirs = both.dirs$gene,
        discrete = discrete$gene,
        graph.size = pv.pars$gsizes,
        node.size = pv.pars$nsizes,
        key.pos = key.pos,
        cex = pv.pars$key.cex,
        lwd = pv.pars$key.lwd,
        low = low$gene,
        mid = mid$gene,
        high = high$gene,
        align = "n"
      )
      align <- key.align
    }
    ucol.cpd <- unique(as.vector(cols.ts.cpd))
    na.col.cpd <- ucol.cpd %in% c(na.col, NA)
    if (plot.col.key && !is.null(cols.ts.cpd) && !all(na.col.cpd)) {
      off.sets <- col.key(
        limit = limit$cpd,
        bins = bins$cpd,
        both.dirs = both.dirs$cpd,
        discrete = discrete$cpd,
        graph.size = pv.pars$gsizes,
        node.size = pv.pars$nsizes,
        key.pos = key.pos,
        off.sets = off.sets,
        cex = pv.pars$key.cex,
        lwd = pv.pars$key.lwd,
        low = low$cpd,
        mid = mid$cpd,
        high = high$cpd,
        align = align
      )
    }
    if (new.signature) {
      pathview.stamp(x = 17, y = 20, on.kegg = TRUE, cex = pv.pars$sign.cex)
    }
    par(pv.pars$op)
    dev.off()
  }
  return(invisible(pv.pars))
}
