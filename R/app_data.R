# App startup data
#
# Find the database folder, open the species table, and load the data every
# session shares. It is loaded once, by the first session, and kept in .app.
#
#   load_app_data()           fill .app; call first thing in the server
#   connect_convert_db()      open orgInfo.db (download the demo data if missing)
#   connect_convert_db_org()  open one species' database
#
# What .app holds:
#   datapath, org_info_file, db_ver, db_url   where the database lives
#   convert        open connection to orgInfo.db
#   orgInfo        the species table, common species first, STRING-db last
#   speciesChoice  species menu: "Best matching species", then common species
#   gmtFiles       path of each species' database file
#   keggSpeciesID  Ensembl dataset -> KEGG species code
#   quotes         quotes shown in the progress messages
#   GO_levels, level2Terms, annotatedSpeciesCounts   loaded but not used
# ----------------------------------------------------------------------------


.app <- new.env()


# Fill .app. Does nothing if it is already filled, so every session can call
# it and only the first one queries the database.
load_app_data <- function() {
  if (!is.null(.app$orgInfo)) {
    return(invisible(.app))
  }

  # Database location: IDEP_DATABASE if set, otherwise two folders up, and as a
  # last resort ./data115/ (where connect_convert_db() downloads the demo data).
  .app$db_ver <- "data115"
  .app$db_url <- "http://bioinformatics.sdstate.edu/data/"

  datapath <- Sys.getenv("IDEP_DATABASE")[1]
  if (nchar(datapath) == 0) {
    datapath <- paste0("../../data/")
  }
  datapath <- paste0(datapath, "/", .app$db_ver, "/")
  org_info_file <- paste0(datapath, "demo/orgInfo.db")
  if (!file.exists(org_info_file)) {
    datapath <- paste0("./", .app$db_ver, "/")
    org_info_file <- paste0(datapath, "demo/orgInfo.db")
  }
  .app$datapath <- datapath
  .app$org_info_file <- org_info_file

  .app$convert <- connect_convert_db()

  # Species table: alphabetical, then the common species first, with STRING-db
  # genomes last.
  orgInfo <- dbGetQuery(.app$convert, paste("select distinct * from orgInfo "))
  orgInfo <- orgInfo[order(orgInfo$name), ]

  top_choices <- c(
    "Human",
    "Mouse",
    "Drosophila melanogaster",
    "Zebrafish",
    "Caenorhabditis elegans",
    "Arabidopsis thaliana",
    "Saccharomyces cerevisiae",
    "Escherichia coli",
    "Rat",
    "Cow",
    "Pig",
    "Chicken",
    "Macaque",
    "Dog",
    "Zea mays",
    "Glycine max",
    "Oryza sativa Indica Group",
    "Oryza sativa Japonica Group",
    "Vitis vinifera"
  )

  ix <- match(orgInfo$name2, top_choices)
  orgInfo <- orgInfo[order(ix), ]
  orgInfo <- orgInfo[order(orgInfo$group == paste0("STRINGv", STRING_DB_VERSION)), ]
  .app$orgInfo <- orgInfo

  .app$annotatedSpeciesCounts <- sort(table(orgInfo$group)) # species per group

  # Species menu: "Best matching species" first, then the common species in a
  # fixed order. move2() moves element i to second place, so the species
  # below end up in reverse order: Human, Mouse, Rat, ...
  speciesChoice <- setNames(as.list(orgInfo$id), orgInfo$name2)
  speciesChoice <- append(setNames("BestMatch", "Best matching species"), speciesChoice)
  move2 <- function(i) c(speciesChoice[1], speciesChoice[i], speciesChoice[-c(1, i)])
  i <- which(names(speciesChoice) == "Vitis vinifera")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Oryza sativa Japonica Group")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Oryza sativa Indica Group")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Glycine max")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Zea mays")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Arabidopsis thaliana")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Saccharomyces cerevisiae")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Caenorhabditis elegans")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Drosophila melanogaster")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Dog")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Macaque")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Chicken")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Pig")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Zebrafish")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Cow")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Rat")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Mouse")
  speciesChoice <- move2(i)
  i <- which(names(speciesChoice) == "Human")
  speciesChoice <- move2(i)
  .app$speciesChoice <- speciesChoice

  .app$GO_levels <- dbGetQuery(.app$convert, "select distinct id,level from GO
                                 WHERE GO = 'biological_process'")
  .app$level2Terms <- .app$GO_levels[which(.app$GO_levels$level %in% c(2, 3)), 1] # level 2 and 3

  quotes <- dbGetQuery(.app$convert, " select * from quotes")
  quotes$quotes <- gsub("\\\"", "", quotes$quotes) # remove the quotes \"
  .app$quotes <- paste0(quotes$quotes, " -- ", quotes$author)

  keggSpeciesID <- orgInfo[, c("ensembl_dataset", "name", "KEGG")]
  colnames(keggSpeciesID)[3] <- "kegg"
  .app$keggSpeciesID <- keggSpeciesID

  .app$gmtFiles <- paste(datapath, "/db/", orgInfo$file, sep = "")

  invisible(.app)
}


# Open orgInfo.db, the database with the species table, ID mapping across all
# species, and the quotes. If it isn't there, download and unpack the demo
# data (data115.tar.gz) into the current folder first.
connect_convert_db <- function(datapath = .app$datapath) {
  if (!file.exists(.app$org_info_file)) {
    withProgress(message = "Download demo data and species database", {
      incProgress(0.2)
      file_name <- paste0(.app$db_ver, ".tar.gz")
      options(timeout = 300)
      download.file(
        url = paste0(.app$db_url, .app$db_ver, "/", file_name),
        destfile = file_name,
        mode = "wb",
        quiet = FALSE
      )
      untar(file_name)
      file.remove(file_name) # delete the tar file to save storage
    })
  }

  return(DBI::dbConnect(
    drv = RSQLite::dbDriver("SQLite"),
    dbname = .app$org_info_file,
    flags = RSQLite::SQLITE_RO
  ))
}


# Open one species' database (gene info, ID mapping, pathways), read-only.
# Returns the connection, or a "try-error" if the file can't be opened.
connect_convert_db_org <- function(datapath = .app$datapath, select_org) {
  ix <- which(.app$orgInfo$id == select_org)
  db_file <- .app$orgInfo[ix, "file"]
  return(try(
    DBI::dbConnect(
      drv = RSQLite::dbDriver("SQLite"),
      dbname = paste0(datapath, "db/", db_file),
      flags = RSQLite::SQLITE_RO
    )
  ))
}
