#' geneInfo
#'
#' @description Looks up annotation info (symbol, biotype, chromosome, Entrez
#'   ID, etc.) for every gene in the species' gene info table, and marks
#'   which rows belong to the query list vs. the rest of the genome. For
#'   Ensembl species, also flags rows that are duplicates of another gene
#'   (e.g. the same gene annotated on a patch chromosome) so callers can
#'   filter them out.
#'
#' @param converted The query gene list, as returned by `convertID()`
#'   (a list with `$IDs`, the matched Ensembl gene IDs). `NULL` or an empty
#'   `$IDs` short-circuits to the "ID not recognized!" placeholder.
#' @param selectOrg The selected species ID, or the "BestMatch" sentinel.
#'   Only species IDs coercible to a positive number (i.e. proper Ensembl
#'   species, not STRING-db genomes) get the duplicate-marking step.
#'
#' @return A 1x1 data frame with "ID not recognized!" if `converted` is
#'   `NULL` or empty. Otherwise the species' full gene info table, with a
#'   `Set` column ("List" if the gene is in `converted$IDs`, else "Genome")
#'   and, for Ensembl species, a `duplicated` column.
#'
#' @noRd
geneInfo <- function(converted, selectOrg) {
  if (is.null(converted)) {
    return(as.data.frame("ID not recognized!"))
  } # no ID
  querySet <- converted$IDs

  if (length(querySet) == 0) {
    return(as.data.frame("ID not recognized!"))
  }

  querySTMT <- paste0(
    "select * from geneInfo;"
  )

  # connect to the database, this becomes a global variable
  convert_species <- connect_convert_db_org(datapath, selectOrg)
  x <- dbGetQuery(convert_species, querySTMT)
  dbDisconnect(convert_species)

  # mark duplicated genes; mostly the same genes on pached chromosomes.
  # Preference order when picking which duplicate "wins": protein-coding
  # over non-coding, then shorter chromosome name (i.e. real chromosomes
  # like "20" before patch/scaffold names). Genes missing an Entrez ID are
  # never marked duplicated, since there's nothing to collide on.
  if(as.numeric(selectOrg) > 0) { # if it is a ENSEMBL species
    x <- x |>
      mutate(coding_status = if_else(gene_biotype == "protein_coding", TRUE, FALSE)) |> # TRUE for coding
      mutate(chr_name_length = nchar(chromosome_name)) |>    # chr 20 --> 2
      mutate(entrez_symbol = paste(entrezgene_id, symbol)) |>  # "7105 TSPAN6"
      arrange(entrez_symbol, entrezgene_id, -coding_status, chr_name_length) |>
      mutate(duplicated = duplicated(entrez_symbol)) |> # both entrez and symbol the same?
      #if entrez is missing, does not count
      mutate(duplicated = if_else(is.na(entrezgene_id), FALSE, duplicated)) |>
      select(-c(coding_status, chr_name_length, entrez_symbol)) # clean up
  }

  # flag whether each gene (genome-wide) is part of the query list
  Set <- match(x$ensembl_gene_id, querySet)
  Set[which(is.na(Set))] <- "Genome"
  Set[which(Set != "Genome")] <- "List"
  # x = cbind(x,Set) } # just for debuging
  return(cbind(x, Set))
}
