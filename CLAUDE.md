# CLAUDE.md

## What this is

ShinyGO — an R/Shiny web app for gene enrichment analysis (GO terms, KEGG pathways, protein networks, promoter motifs, chromosome distribution). Hosted at http://bioinformatics.sdstate.edu/go. Author: Steven Ge (Ge Lab, SDSU).

## Layout

This is a classic three-file Shiny app (no package structure); Shiny auto-loads these by filename:

- `global.R` — startup: loads libraries, defines the SQLite database connection/config, and most analysis + plotting helper functions. Sourced once before the app starts.
- `ui.R` — front-end layout (tabs, inputs, outputs).
- `server.R` — back-end reactive logic wiring inputs to analyses and outputs.
- `utils_kegg_pathview.R` — self-contained KEGG pathway diagram rendering (vendored to remove the `pathview` Bioconductor dependency; sourced from `global.R`).
- `install_pckages.R` — installs CRAN + Bioconductor dependencies. Run once to set up.
- `www/` — static assets (images, favicon).

## Running

Open the folder in RStudio and click **Run App**, or from R: `shiny::runApp()` in the repo root.

## Data

The app needs species databases (SQLite) that are **not** in this repo. Location is resolved in `global.R`:
- Env var `IDEP_DATABASE`, else `../../data/`, else `./data113/` in the repo root.
- `db_ver` (currently `data113`) sets which version is expected.
- If missing, `connect_convert_db()` downloads demo + species data from `db_url` (`http://bioinformatics.sdstate.edu/data/`).

## Conventions

- No test suite or build step — verify changes by running the app.
- Global config uses `<<-` in `global.R` (intentional).
- Dependencies are listed in `install_pckages.R`, not a DESCRIPTION file.
