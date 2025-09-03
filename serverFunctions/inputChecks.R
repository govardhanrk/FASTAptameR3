# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

#' Check for non-null file inputs
check_fileInput <- function(finput = NULL){
  
  # check if the input is NULL
  notValid <- ifelse(is.null(finput), TRUE, FALSE)
  return(notValid)
}

#' Check for valid file extension (only use for modules with a single file input)
check_extension <- function(fpath = NULL, extensions = NULL){
  
  # check if the file extension is valid
  notValid <- ifelse(toupper(tools::file_ext(fpath)) %in% toupper(extensions), FALSE, TRUE)
  return(notValid)
}

#' Check for valid file extensions (only use for modules with multiple file input)
check_extensions <- function(finput = NULL, extensions = NULL){
  
  # check if the files have valid extensions
  notValid <- ifelse(
    sum(toupper(tools::file_ext(finput$datapath)) %in% toupper(extensions)) != nrow(finput),
    TRUE,
    FALSE
  )
  return(notValid)
}

#' Check for valid patterns in regex-like functions
#' Translate - comma-separated alphanumeric modifications to the available translations; separated by spaces
#' Motif Search - comma- or space-separated alphanumeric motifs to search
#' Distance - reference sequence
check_regexInputs <- function(pattern = NULL, separator = NULL, userInputs = NULL){
  
  # check if the input is valid for the subsequent regex operations
  notValid <- ifelse(grepl(pattern, gsub(separator, "", userInputs)), TRUE, FALSE)
  return(notValid)
}

#' Check for input strings in user inputs
#' Motif Search - the search query
#' Distance - reference sequence
check_emptyString <- function(stringInput = NULL){
  
  # check if the input string is empty
  notValid <- ifelse(stringInput == "", TRUE, FALSE)
  return(notValid)
}

#' Check for the number of files provided
#' Motif Tracker, Cluster Enrich - 2+ files
#' Sequence Enrich - exactly 2 files
check_numFiles <- function(finput = NULL, isExact = TRUE, numFiles = 2){
  
  # check if you have 1) exactly the number of files needed (isExact == TRUE) or the min. number of files (isExact == FALSE)
  if(isExact){
    notValid <- ifelse(nrow(finput) != numFiles, TRUE, FALSE)
  } else{
    notValid <- ifelse(nrow(finput) < numFiles, TRUE, FALSE)
  }
  
  return(notValid)
}

#' Check for the number of ordered files
#' Motif Tracker, Cluster Enrich - 2+ files
#' Sequence Enrich - exactly 2 files
check_numOrdered <- function(fselected = NULL, isExact = TRUE, numFiles = 2){
  
  # check if you have 1) exactly the number of files needed (isExact == TRUE) or the min. number of files (isExact == FALSE)
  if(isExact){
    notValid <- ifelse(length(fselected) != 2, TRUE, FALSE)
  } else{
    notValid <- ifelse(length(fselected) < 2, TRUE, FALSE)
  }
  
  return(notValid)
}

#' Check if the first ordered input is the default "*" character
check_validOrder <- function(fselected = NULL){
  
  # check if the first element is an asterisk
  notValid <- ifelse(fselected[1] == "*", TRUE, FALSE)
  return(notValid)
}

#' Check if the number of queries and aliases in Motif Tracker match
check_queryAlias <- function(queries = NULL, aliases = NULL){
  
  # get number of queries and aliases, respectively
  numQueries <- length(unlist(strsplit(queries, split = "\n")))
  numAliases <- length(unlist(strsplit(aliases, split = "\n")))
  
  # check if queries and aliases are of the same length; however, an empty alias list (meaning none provided) is acceptable
  notValid <- ifelse(numQueries != numAliases & aliases[1] != "", TRUE, FALSE)
  
  return(notValid)
}

#' Check if the data.frame from the Sequence Enrich module has cluster information
check_clusterInEnrich <- function(enrichDF = NULL){
  
  # check for cluster column in data.frame from Enrich
  notValid <- ifelse(enrichDF %>% dplyr::select(dplyr::contains("Cluster")) %>% ncol() == 0, TRUE, FALSE)
  return(notValid)
}

#' Check if the first line of the input FASTA file has metadata entries for clusters
check_clusterInID <- function(fpath = NULL){
  
  # check for cluster metadata in FASTA input
  notValid <- ifelse(lengths(strsplit(readLines(fpath, n = 1), split = "-")) != 6, TRUE, FALSE)
  return(notValid)
}

#' Check if the column names of the input CSV file has expected entries from the Recluster analysis
check_reclusterInID <- function(fpath = NULL){
  
  # check for cluster metadata in FASTA input
  notValid <- ifelse(length(colnames(read.csv(fpath, nrows = 1))) != 14, TRUE, FALSE)
  return(notValid)
}

#' Check if the selected clusters for the k-mer plot are valid
check_kmerClusters <- function(clusters = NULL){
  
  # confirm that there are 1-8 clusters present in the selection
  notValid <- ifelse(clusters[1] == "*" | length(clusters) < 1 | length(clusters) > 8, TRUE, FALSE)
  return(notValid)
}

#' Check if the selected cluster for the MSA is valid
check_clusterSelection <- function(cluster = NULL){
  
  # confirm that there is exactly 1x cluster present in the selection
  notValid <- ifelse(cluster[1] == "*" | length(cluster) != 1, TRUE, FALSE)
  return(notValid)
}

#' Check if the translation modifications are valid
check_translate_mods <- function(codons = NULL, translations = NULL){
  
  # check if both are empty (both or neither should be empty)
  if(xor(codons != "", translations != "")){
    return(TRUE)
  }
  
  # get lengths after splitting by comma
  codon_length <- codons %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist() %>% length()
  translation_length <- translations %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist() %>% length()
  
  # check if both are of the same length after splitting by comma
  if(codon_length != translation_length){
    return(TRUE)
  }
  
  # if modifications are valid, return FALSE (means that notification does not trigger)
  return(FALSE)
}

#' Check if the uploaded file exceeds the total number of allowed lines
check_nlines <- function(fpath = NULL, nlines = 10000){
  
  # check if the file has too many lines
  notValid <- ifelse(LaF::determine_nlines(fpath) > nlines, TRUE, FALSE)
  return(notValid)
}
