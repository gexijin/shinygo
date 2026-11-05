# Shared setup for the baseline tests.
#
# The tests need the species databases, which are not in the repo. They read
# IDEP_DATABASE (the same variable the app uses) and are skipped when it is
# unset or does not hold the yeast database. Run from the repo root with:
#
#   IDEP_DATABASE=~/data Rscript -e 'testthat::test_dir("tests/testthat")'

# Snapshot tests skip themselves "on CRAN" unless this is set. This is an app,
# never submitted to CRAN.
Sys.setenv(NOT_CRAN = "true")

yeast_db <- function() {
  file.path(
    path.expand(Sys.getenv("IDEP_DATABASE")),
    "data115", "db", "scerevisiae_gene_ensembl__Yeast.db"
  )
}

skip_if_no_db <- function() {
  testthat::skip_if(
    Sys.getenv("IDEP_DATABASE") == "" || !file.exists(yeast_db()),
    "IDEP_DATABASE is unset or has no yeast database"
  )
}

# Load global.R and R/ once per run, only after skip_if_no_db() has passed:
# global.R tries to download the whole database bundle when it can't find one.
app_env <- local({
  env <- NULL
  function() {
    if (is.null(env)) {
      # Same layout as a running app: global.R in one environment, R/ in a
      # child of it. Code in R/ can see global.R, but global.R can't see R/.
      # Loading both into one environment would hide that.
      app_global <- new.env(parent = globalenv())
      app_r <- new.env(parent = app_global)
      suppressMessages(shinytest2::load_app_env(
        testthat::test_path("..", ".."),
        renv = app_r, globalrenv = app_global
      ))
      # One list of everything, so tests can use env$FindOverlap. Each
      # function keeps its own environment, so the rule above still holds.
      env <<- c(as.list(app_global, all.names = TRUE), as.list(app_r, all.names = TRUE))
      env$load_app_data() # as the server does at the start of each session
    }
    env
  }
})

yeast_id <- "208" # Saccharomyces cerevisiae in orgInfo

# 50 yeast cell-cycle genes as systematic ORF names, plus one ID that maps to
# nothing. The yeast database maps ORF names but not gene symbols.
yeast_cell_cycle_genes <- paste(
  "YBR160W YMR199W YPL256C YAL040C YGR108W YPR119W YDL155W YLR210W YPR120C",
  "YGR109C YLR079W YGL116W YGL003C YER111C YLR182W YDL056W YOR083W YFR028C",
  "YFL009W YDR054C YDL132W YDR328C YNL172W YLR127C YKL022C YHR166C YBL084C",
  "YBL023C YEL032W YPR019W YLR274W YGL201C YBR202W YJL194W YLR103C YDR052C",
  "YDL017W YML065W YBR060C YPL153C YBR136W YBL088C YDR113C YGR098C YGL086W",
  "YJL030W YGR188C YOR026W YJL157C YJL187C NOTAGENE1"
)
