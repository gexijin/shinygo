# Species lookup functions
#
# Look up species details in orgInfo (the species table loaded by
# load_app_data()).
#
#   findSpeciesById()            species ID -> its orgInfo row
#   findSpeciesByIdName()        species ID -> display name
#   find_species_id_by_ensembl() Ensembl dataset name -> species ID
#   find_taxon_by_id()           species ID -> NCBI taxonomy ID
# ----------------------------------------------------------------------------


# The species' row in orgInfo.
findSpeciesById <- function(speciesID) {
  return(.app$orgInfo[which(.app$orgInfo$id == speciesID), ])
}

# The species' display name (orgInfo's third column, name2), e.g. "Human".
findSpeciesByIdName <- function(speciesID) {
  return(.app$orgInfo[which(.app$orgInfo$id == speciesID), 3])
}

# The species ID for an Ensembl dataset name, e.g. "hsapiens_gene_ensembl".
# The species table sends this name when a user clicks a species.
find_species_id_by_ensembl <- function(ensembl_dataset, org_info) {
  return(org_info[which(org_info$ensembl_dataset == ensembl_dataset), "id"])
}

# The species' NCBI taxonomy ID, used for STRING-db.
find_taxon_by_id <- function(species_id, org_info) {
  return(org_info[which(org_info$id == species_id), "taxon_id"])
}
