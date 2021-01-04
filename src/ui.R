####################################################
# Author: Eric Tulowetzke
# Lab: Ge Lab
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# Project: ShinyGO Restructure
# Purpose of project: rewrite ShinyGO61 code to be easier to develop
# File: ui.R
# Purpose of file: ui have shiny
# Start data: 01-03-2021 (mm-dd-yyyy)
# Data last modified: 01-03-2021 (mm-dd-yyyy)
#######################################################

pacman::p_load(shiny, reactable)
setwd("C:/Users/ericw/Documents/ge_lab/idep/database_shiny_app/shinygo/")
source("src/databaseView.R")

###list of all specie from inforg name2 data: 01-02-2021
specieList <- c("Coquerels sifaka", "Platypus", "Cat", "Hyrax", "Prairie vole", "Platyfish",
"Lesser hedgehog tenrec", "Black snub-nosed monkey", "Angola colobus", "Mouse Lemur", "Mouse SPRET/EiJ",
"Amazon molly", "Brazilian guinea pig", "Damara mole rat", "Dolphin", "Megabat", "Vervet-AGM", "Panda", 
"Fugu", "Naked mole-rat male", "Pig", "Chinese hamster CHOK1GS", "Cave fish", "Anole lizard", "Saccharomyces cerevisiae",
"Sheep", "Rat", "Gibbon", "Shrew mouse", "Tilapia", "Bolivian squirrel monkey", "Hedgehog", "Pika", "Coelacanth",
"Mas night monkey", "Xenopus", "Dog", "Pig-tailed macaque", "Gorilla", "Northern American deer mouse",
"Long-tailed chinchilla", "Golden snub-nosed monkey", "Ferret", "Spotted gar", "Tree Shrew", "Fruitfly", "Tarsier",
"Armadillo", "Mouse", "Squirrel", "Sooty mangabey", "Lesser Egyptian jerboa", "Turkey", "Shrew", "Chinese hamster CriGri",
"Rabbit", "Zebra Finch", "Horse", "Bonobo", "Medaka", "Cod", "Capuchin", "Sloth", "Naked mole-rat female",
"Kangaroo rat", "Alpaca", "C.intestinalis", "Lamprey", "Microbat", "Bushbaby", "C.savignyi", "Chimpanzee", "Guinea Pig",
"Golden Hamster", "Chicken", "Degu", "Elephant", "Zebrafish", "Cow", "Marmoset", "Tetraodon", "Olive baboon",
"Caenorhabditis elegans", "Flycatcher", "Ryukyu mouse", "Opossum", "Drill", "Stickleback", "Orangutan", "Chinese softshell turtle",
"Duck", "Macaque", "Crab-eating macaque", "Tasmanian devil", "Upper Galilee mountains blind mole rat", "Wallaby",
"Human", "Aegilops tauschii", "Oryza brachyantha", "Populus trichocarpa", "Prunus persica", "Solanum tuberosum",
"Setaria italica", "Solanum lycopersicum", "Oryza nivara", "Physcomitrella patens", "Glycine max", "Sorghum bicolor",
"Brachypodium distachyon", "Leersia perrieri", "Oryza punctata", "Oryza sativa Indica", "Selaginella moellendorffii",
"Galdieria sulphuraria", "Oryza glumaepatula", "Oryza barthii", "Oryza sativa Japonica", "Ostreococcus lucimarinus",
"Hordeum vulgare", "Brassica oleracea", "Brassica napus", "Oryza meridionalis", "Arabidopsis lyrata",
"Chondrus crispus", "Oryza rufipogon", "Triticum aestivum", "Brassica rapa", "Vitis vinifera", "Zea mays",
"Medicago truncatula", "Amborella trichopoda", "Beta vulgaris subsp", "vulgaris", "Chlamydomonas reinhardtii",
"Corchorus capsularis", "Oryza longistaminata", "Trifolium pratense", "Cyanidioschyzon merolae", "Oryza glaberrima",
"Theobroma cacao", "Musa acuminata", "Triticum urartu", "Arabidopsis thaliana", "Tribolium castaneum", "Drosophila grimshawi",
"Rhodnius prolixus", "Strongyloides ratti", "Amphimedon queenslandica", "Pediculus humanus", "Bombyx mori", "Caenorhabditis japonica", "Daphnia pulex", "Tetranychus urticae",
"Drosophila willistoni", "Ixodes scapularis", "Acyrthosiphon pisum", "Drosophila yakuba", "Caenorhabditis elegans", "Belgica antarctica",
"Atta cephalotes", "Aedes aegypti", "Trichoplax adhaerens", "Octopus bimaculoides", "Pristionchus pacificus", "Thelohanellus kitauei", "Drosophila sechellia", "Drosophila simulans", "Strongylocentrotus purpuratus",
"Apis mellifera", "Anoplophora glabripennis", "Drosophila melanogaster", "Danaus plexippus", "Caenorhabditis briggsae", "Megaselia scalaris", "Drosophila ananassae", "Onchocerca volvulus",
"Schistosoma mansoni", "Anopheles gambiae", "Culex quinquefasciatus", "Stegodyphus mimosarum", "Drosophila virilis", "Trichinella spiralis",
"Brugia malayi", "Lingula anatina", "Bombus impatiens", "Heliconius melpomene", "Caenorhabditis brenneri", "Mayetiola destructor",
"Zootermopsis nevadensis", "Crassostrea gigas", "Drosophila persimilis", "Drosophila erecta", "Caenorhabditis remanei", "Anopheles darlingi",
"Capitella teleta", "Nasonia vitripennis", "Loa loa", "Solenopsis invicta", "Lucilia cuprina", "Helobdella robusta", "Drosophila pseudoobscura",
"Melitaea cinxia", "Mnemiopsis leidyi", "Strigamia maritima", "Sarcoptes scabiei", "Nematostella vectensis", "Lepeophtheirus salmonis", "Lottia gigantea", "Adineta vaga", "Drosophila mojavensis", "Dendroctonus ponderosae")
specieList <- sort(specieList)

idtypeList <- c("ensembl_gene_id", "ensembl_gene_id_version", "ensembl_transcript_id", "ensembl_transcript_id_version", "ensembl_peptide_id", "ensembl_peptide_id_version", "ensembl_exon_id",
"embl", "hgnc_id", "hgnc_symbol", "hgnc_trans_name", "protein_id", "kegg_enzyme", "entrezgene", "refseq_mrna_predicted", "refseq_ncrna_predicted", "refseq_peptide", "refseq_peptide_predicted",
"rnacentral", "uniparc", "uniprot_gn_trans_name", "uniprot_gn", "uniprotswissprot", "uniprotsptrembl", "wikigene_id", "affy_cyngene_1_0_st_v1", "affy_cyrgene_1_0_st_v1", "affy_hc_g110", "affy_hg_focus",
"affy_hg_u133a", "affy_hg_u133a_2", "affy_hg_u133b", "affy_hg_u133_plus_2", "affy_hg_u95a", "affy_hg_u95av2", "affy_hg_u95b", "affy_hg_u95c", "affy_hg_u95d", "affy_hg_u95e", "affy_hta_2_0", "affy_huex_1_0_st_v2",
"affy_hugenefl", "affy_hugene_1_0_st_v1", "affy_hugene_2_0_st_v1", "affy_primeview", "affy_rhegene_1_0_st_v1", "affy_rhegene_1_1_st_v1", "affy_rhesus", "affy_u133_x3p", "agilent_gpl10157", "agilent_gpl10158", "agilent_gpl17465",
"agilent_gpl19384", "nimblegen_gpl13762", "nimblegen_gpl21301", "cdd", "gene3d", "hamap", "mobidblite", "hmmpanther", "blastprodom", "pfscan", "sfld", "interpro", "entrezgene_trans_name", "arrayexpress", "merops", "mirbase_id",
"mirbase_trans_name", "oxford_fgu_oa_gene", "oxford_fgu_oa_tscript", "pdb", "platypus_olfactory_receptor", "refseq_mrna", "refseq_ncrna", "unigene", "affy_platypus_exon", "ens_hs_gene", "ens_hs_transcript", "ens_hs_translation", "metacyc",
"mgi_symbol", "mgi_id", "mgi_trans_name", "reactome", "unipathway", "zfin_id_id", "zfin_id_symbol", "zfin_id_trans_name", "ccds", "chembl", "clone_based_ensembl_gene", "clone_based_ensembl_transcript", "clone_based_vega_gene", "clone_based_vega_transcript",
"epd", "reactome_gene", "reactome_transcript", "ucsc", "affy_mg_u74a", "affy_mg_u74av2", "affy_mg_u74b", "affy_mg_u74bv2", "affy_mg_u74c", "affy_mg_u74cv2", "affy_moe430a", "affy_moe430b", "affy_moex_1_0_st_v1", "affy_mogene_1_0_st_v1", "affy_mogene_2_1_st_v1",
"affy_mouse430a_2", "affy_mouse430_2", "affy_mu11ksuba", "affy_mu11ksubb", "agilent_sureprint_g3_ge_8x60k", "agilent_wholegenome", "agilent_wholegenome_4x44k_v1", "agilent_wholegenome_4x44k_v2", "codelink_codelink", "illumina_mouseref_8", "illumina_mousewg_6_v1",
"illumina_mousewg_6_v2", "phalanx_onearray", "affy_porcine", "agilent_cho2agl44v1", "intenz", "phi", "refseq_dna", "sgd_gene", "affy_yeast_2", "affy_yg_s98", "rgd_symbol", "rgd_id", "rgd_trans_name", "affy_rae230a", "affy_rae230b", "affy_raex_1_0_st_v1", "affy_ragene_1_0_st_v1",
"affy_ragene_2_1_st_v1", "affy_rat230_2", "affy_rg_u34a", "affy_rg_u34b", "affy_rg_u34c", "affy_rn_u34", "affy_rt_u34", "agilent_wholegenome_4x44k_v3", "illumina_ratref_12_v1", "xenbase", "xenbase_trans_name", "affy_x_tropicalis", "vgnc", "vgnc_trans_name", "affy_canine_2", "flybase_gene_id",
"flybasename_gene", "flybase_transcript_id", "flybasename_transcript", "flybase_translation_id", "flybasename_translation", "string", "uniprotvarsplic", "affy_drosgenome1", "affy_drosophila_2", "ens_mm_transcript", "ens_mm_translation", "genedb", "ottg", "ottt", "ottp", "vega_gene", "vega_transcript",
"vega_translation", "agilent_sureprint_gpl7083_4x44k", "agilent_sureprnt_gpl16709_4x44k", "ens_ga_gene", "ens_ga_transcript", "ens_ga_translation", "cint_aniseed_v1", "cint_jgi_v1", "affy_cint06a520380f", "agilent_arraystar", "agilent_wholegenome_4x44k", "nimblegen_nimblegen_13k", "agilent_037725_hamarrayv",
"affy_chicken", "agilent_059389_chicken_ge_8x60k", "affy_zebrafish", "agilent_g2518a", "agilent_g2519f", "leiden_leiden2", "leiden_leiden3", "affy_bovine", "wormbase_gene", "wormbase_gseqname", "wormbase_locus", "wormbase_transcript", "wormpep_id", "affy_c_elegans", "agilent_012795", "agilent_015061", "agilent_020186", "agilent_gpl13394",
"agilent_gpl13914", "agilent_gpl14144", "agilent_gpl19516", "agilent_gpl8304", "nimblegen_gpl8673", "slri_gpl3518", "ucsf_gpl9450", "wustl_wustl_c_elegans", "seg", "agilent_agilent_8x15k", "dbass3_id", "dbass5_id", "hpa", "ens_lrg_gene", "ens_lrg_transcript", "agilent_cgh_44b", "agilent_gpl6848", "agilent_sureprint_g3_ge_8x60k_v2",
"illumina_humanht_12_v3", "illumina_humanht_12_v4", "illumina_humanref_8_v3", "illumina_humanwg_6_v1", "illumina_humanwg_6_v2", "illumina_humanwg_6_v3", "gramene_plant_reactome", "trnascan_se", "goa", "agi_gene", "agi_transcript", "po_id", "gramene_markersdb_est", "gramene_markersdb_mrna", "po", "plantgdb_put", "refseq_dna_predicted", 
"tair_symbol", "affy_poplar", "pride", "swiss_model", "pgsc_gene", "biocyc", "brenda", "kegg", "peroxibase", "bgi_gene", "bgi_est_cluster", "fsts", "geneindex", "rap_gene", "affy_rice", "agilent_g2519f_015241", "agilent_g4138a_012106", "nsf_bgiyale", "nsf_nsf20k", "nsf_nsf45k", "baylorcollege", "tigr_locus", "rhea", "barlex", "ena_feature_gene", 
"ena_feature_transcript", "ena_feature_protein", "affy_affymetrixbarleygenomearray", "agilent_barleygeneexpressionmicroarray", "archive_plants", "brad_transcript", "tair_locus", "tigr_geneindex", "affy_vitis_vinifera", "maizegdb", "affy_affymetrixmaizegenomearray", "mtgd", "hssp", "nasc_gene_id", "tair_locus_model", "tair_translation", 
"affy_ath1_121501", "agilent_g2519f_015059", "agilent_g2519f_021169", "catma_catma", "flybasecgid_gene", "flybasecgid_transcript", "flybasecgid_translation", "rnammer", "vb_community_annotation", "vb_external_description", "pubmed", "jgi_gene", "jgi_transcript", "vb_community_symbol", "immunodb", "vb_rna_description", "agilent_jhsph_agilent_gpl4877", 
"agilent_liv_agilent_8x15k_v1", "agilent_oxford_agilent_44k_v1", "agilent_uci_agilent_16k", "agilent_uq_agilent_4x44k_v1", "apogent_wisc_hemocyte_2k_v1_v2", "custom_est_nd_tigrtc_9_6k_v1", "custom_est_ucr_gillmgmt_v2", "liv_liv_aegdetox_0_25k_v1", "liv_liv_aegdetox_0_25k_v2", "nimblegen_nd_nimblegen_12plex", "spbase", "schistodb", "celera_gene", 
"celera_pep", "celera_trans", "ribosomal_protein_gene_database", "affy_plasmodium_anopheles", "agilent_agilent_020449_44k", "agilent_ic_genimmunity_4x44k_v1", "agilent_lstm_an_gambiae_agam15k_v1_0", "agilent_oxford_agilent_13k_v1", "custom_est_embl_mmc1_20k_v1", "custom_est_embl_mmc2_12k_v1", "custom_est_liv_gamdetox_0_25k_v1", "custom_est_liv_gamdetox_0_25k_v2",
"nimblegen_nd_65k_v1", "agilent_niid_detox_2_5k_v1", "flybase_annotation_id", "transfac", "zngp", "lncc", "gfbgp", "smar_community_annotation", "genbank")
idtypeList <- sort(idtypeList)
idtypeList <- c("None", idtypeList)


ui <- fluidPage(
    selectInput("userSpecie", "What's your specie name?", specieList),
    selectInput("userIDtype", "What's your ID type? (Optional)", idtypeList),
    textAreaInput("geneList", "Gene List (Optional)", value = "No genes"),
    actionButton("submit", "submit"),
    textOutput("text"),
    reactableOutput("table")
)