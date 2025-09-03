#' Compute the average enrichment per position per residue for a single super-cluster
#'
#' `fa_posEnrich` computes the average enrichment of reclustered, aligned sequences for every residue at every position for a single super-cluster.
#' This function first aligns all sequences in a user-defined super-cluster.
#' Compute the average sequence enrichment for each column of the MSA and each valid character in the MSA (including the gap character, "-").
#' For a DNA example, compute the average enrichment for sequences with (A,C,G,T,-) at position 1, and then repeat this for every subsequent position.
#' Thus, this example will have 5 x L average enrichment scores, where 5 corresponds to the alphabet size, and L corresponds to the length of the aligned sequences.
#'
#' @param faDF_recluster A reclustered data.frame from fa_recluster
#' @param cluster_selection The cluster to be considered for this analysis (default: 1); must be an integer that corresponds to a cluster present in the data
#' @param seq_type One of dna (default) or protein
#'
#' @seealso [fa_recluster()], [msa::msa()], [fa_posEnrich_barplot()], [fa_posEnrich_heatmap()]
#'
#' @return A data.frame with the average enrichment value for each residue at each position
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' clusterData <- list(
#'   fa_clusterLED(faDF = countData[[1]]),
#'   fa_clusterLED(faDF = countData[[2]])
#' )
#' reclusterData <- fa_recluster(
#'   faDF1_cluster = clusterData[[1]],
#'   faDF2_cluster = clusterData[[2]]
#' )
#'
#' # compute position-level enrichment for the 3rd super-cluster
#' posEnrichData <- fa_posEnrich(
#'   faDF_recluster = reclusterData,
#'   cluster_selection = 3,
#'   seq_type = "dna"
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_posEnrich <- function(faDF_recluster = NULL, cluster_selection = 1, seq_type = "dna"){

  # select acceptable alphabet based on user input
  seq_alphabet <- list(
    "dna" = c("A","C","G","T","-"),
    "protein" = c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y","-")
  )[[as.symbol(seq_type)]]

  # define alphabet filter, which depends on the user input
  alphabet_filter <- paste0("[^", paste(seq_alphabet, collapse = ""), "]")

  # if nucleotide sequences, convert U to T
  if(seq_type == "dna"){
    recluster_df <- faDF_recluster %>% dplyr::mutate(Sequences = gsub("U", "T", .data$Sequences))
  }

  # filter for sequences in one cluster that have no ambiguities but do have enrichment scores, select for sequences + enrichment values, perform MSA
  posenrich_df <- faDF_recluster %>%
    dplyr::filter(.data$Cluster == cluster_selection & !grepl(alphabet_filter, .data$Sequences) & !is.na(.data$Enrichment)) %>%
    dplyr::select(.data$Sequences, .data$Enrichment) %>%
    dplyr::mutate(
      Sequences = msa::msa(inputSeqs = .data$Sequences, type = seq_type, order = "input") %>% as.character(),
      Enrichment = as.numeric(.data$Enrichment)
    )

  # make character matrix and bind enrichment scores to it
  seq_mat <- cbind(
    data.frame(stringr::str_split(posenrich_df$Sequences, "", simplify = TRUE)),
    posenrich_df
  )

  # initialize data.frame to hold enrichment values
  pe_results <- data.frame(Residue = seq_alphabet)

  # iterate through each position defined in the sequence matrix
  # for each position, compute the average sequence enrichment for each character at that position
  # e.g., the respective average enrichment of all sequences with [A,C,G,T,-] at the first position, and so on
  for(i in 1:(ncol(seq_mat) - 2)){

    # group by characters at the given position and compute the average enrichment of all sequences with those characters at that position
    temp <- seq_mat %>%
      dplyr::group_by(!!as.symbol(paste0("X", i))) %>%
      dplyr::summarise(AvEn = mean(.data$Enrichment)) %>%
      dplyr::rename(Residue = paste0("X", i)) %>%
      dplyr::rename_with(~ paste0("X", i), "AvEn")

    # merge the grouped summary in the results data.frame
    pe_results <- pe_results %>% dplyr::left_join(temp, by = "Residue")
  }

  # pivot long
  pe_results <- pe_results %>%
    tidyr::pivot_longer(!.data$Residue, names_to = "Position", values_to = "AvEnrich") %>%
    dplyr::mutate(
      Position = gsub("X", "", .data$Position) %>% as.numeric(),
      AvEnrich = round(.data$AvEnrich, digits = 3)
    )

  # return results
  return(pe_results)
}
