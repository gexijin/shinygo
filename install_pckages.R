# Simple ShinyGO Package Installation Script with Error Tolerance

# CRAN packages
cran_packages <- c(
  "shiny", "shinyBS", "shinybusy", "ggplot2", "plotly", "gridExtra", 
  "ggpubr", "visNetwork", "igraph", "dendextend", "dplyr", "DT", 
  "reactable", "reshape2", "RSQLite", "DBI", "png", "tippy"
)

# Install only missing CRAN packages
missing_cran <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
if(length(missing_cran)) {
  cat("Installing CRAN packages:", paste(missing_cran, collapse = ", "), "\n")
  try(install.packages(missing_cran), silent = TRUE)
}

# Install BiocManager if needed
if (!require("BiocManager", quietly = TRUE)) {
  try(install.packages("BiocManager"), silent = TRUE)
}

# Bioconductor packages
bioc_packages <- c("pathview", "STRINGdb", "KEGGREST", "graph", "Rgraphviz", "KEGGgraph")

# Install only missing Bioconductor packages
missing_bioc <- bioc_packages[!(bioc_packages %in% installed.packages()[,"Package"])]
if(length(missing_bioc)) {
  cat("Installing Bioconductor packages:", paste(missing_bioc, collapse = ", "), "\n")
  try(BiocManager::install(missing_bioc, update = FALSE), silent = TRUE)
}

# Summary of what was installed
cat("\nInstallation complete. Checking final status...\n")
all_packages <- c(cran_packages, bioc_packages)
installed_status <- all_packages %in% installed.packages()[,"Package"]
successfully_installed <- sum(installed_status)
cat("Successfully installed/available:", successfully_installed, "out of", length(all_packages), "packages\n")

if(successfully_installed < length(all_packages)) {
  failed_packages <- all_packages[!installed_status]
  cat("Failed to install:", paste(failed_packages, collapse = ", "), "\n")
  cat("ShinyGO may still work with reduced functionality.\n")
}