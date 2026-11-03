# R/mod_groups.R
mod_groups_ui <- function() {
  tabPanel("Groups",
    value = 7,
    downloadButton("downloadGrouping", "Download"),
    h5("Your genes are grouped by functional categories defined by high-level GO terms. "),
    tableOutput("grouping")
  )
}