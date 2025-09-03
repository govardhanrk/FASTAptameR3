#' Identify over-enriched motifs
#'
#' `fa_fsbc_motifDiscovery` is a wrapper around the FSBC code, which identifies over-enriched substrings from an input sequence set.
#' This code is adapted from "FSBC: FAST STRING-BASED CLUSTERING FOR HT-SELEX DATA" by Kato et al. (DOI: 10.1186/s12859-020-03607-1).
#'
#' @param faDF A FASTAptameR data.frame
#' @param minReads The minimum read count for a sequence to be included in the analysis (default: 10)
#' @param lengthRange A numeric vector with the minimum and maximum length of motifs to consider (default: 5, 10)
#'
#' @seealso [fa_count()], [fa_fsbc_rootMotifs()], [fa_fsbc_motifDiscoveryPlot()]
#'
#' @return A data.frame with detected motifs
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # find overrepresented motifs with the FSBC tools
#' motifData <- fa_fsbc_motifDiscovery(faDF = countData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_fsbc_motifDiscovery <- function(faDF = NULL, minReads = 10, lengthRange = c(5, 10)){

  # read FASTA file
  fastaSeqs <- faDF %>%
    dplyr::filter(.data$Reads >= minReads) %>%
    dplyr::pull(.data$Sequences)

  # make DNAStringSet object from sequences
  sequenceSet <- Biostrings::DNAStringSet(fastaSeqs)

  # calculate sequence frequencies
  sequenceFreqs <- fsbc_calc_freq(sequenceSet)

  # calculate base frequencies
  baseFreqs <- fsbc_get_base_ratio(sequenceSet)

  # find over-enriched subsequences (motifs)
  motifSet <- fsbc_search_subseq(
    x = sequenceFreqs,
    freq = sequenceFreqs@metadata$freq,
    symbols = baseFreqs, lmin = min(lengthRange), lmax = max(lengthRange)
  ) %>%
    tibble::rownames_to_column("Motif") %>%
    dplyr::mutate(
      R = round(.data$R, digits = 3),
      P = round(.data$P, digits = 5),
      Z = round(.data$Z, digits = 3),
      ZZ = round(.data$ZZ, digits = 3)
    ) %>%
    dplyr::rename(Motif_Length = .data$L, Rank = .data$rank) %>%
    dplyr::select(.data$Motif, .data$P, .data$ZZ, .data$Motif_Length, .data$Rank) %>%
    dplyr::mutate(SeqCount = sapply(.data$Motif, function(x) sum(grepl(x, fastaSeqs))) %>% as.vector())

  # return set of motifs
  return(motifSet)
}

#' Collapse shorter sub-motifs into larger root motifs
#'
#' `fa_fsbc_rootMotifs` condenses the output of `fa_fsbc_motifDiscovery`, which may return sub-motifs.
#' We define "sub-motifs" as a shorter motif that can be found in its entirety in a longer motif (e.g., "AACG" is a sub-motif of "AACGTTT").
#' In this case, the count of occurrences for the sub-motif will be added to the longer motif.
#' However, one sub-motif may map back onto multiple longer motifs (e.g., "AACG" maps onto "AACGTTT" and "AACGTTA").
#' In this example, the sub-motif count will be added to both of the longer motifs.
#'
#' @param motif_discovery_df A data.frame from fa_fsbc_motifDiscovery
#'
#' @seealso @seealso [fa_count()], [fa_fsbc_motifDiscovery()], [fa_fsbc_motifDiscoveryPlot()]
#'
#' @return A condensed data.frame in which shorter motifs that are perfectly matched to longer motifs are merged
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # find motifs with FSBC and condense motifs
#' motifData <- fa_fsbc_motifDiscovery(faDF = countData) %>%
#'   fa_fsbc_rootMotifs(motif_discovery_df = .)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_fsbc_rootMotifs <- function(motif_discovery_df = NULL){

  # extract motif, make columns for motif length and number of submotifs
  motif_df <- motif_discovery_df %>%
    dplyr::select(.data$Motif) %>%
    dplyr::mutate(Motif_Length = nchar(.data$Motif), Submotif_Count = NA)

  # initialize data.frame for motif roots
  motif_roots <- data.frame()

  # iterate until all motifs are roots or counts for the roots
  while(nrow(motif_df) != 0){

    # progress indicator
    message(glue::glue("Starting length {max(motif_df$Motif_Length)}"))

    # separate max-length motifs
    motif_maxLen <- motif_df %>% dplyr::filter(.data$Motif_Length == max(.data$Motif_Length))
    motif_df <- motif_df %>% dplyr::filter(.data$Motif_Length != max(.data$Motif_Length))

    # count number of submotifs for each max-length motif; a submotif may be present in multiple root motifs
    motif_maxLen$Submotif_Count <- sapply(
      1:nrow(motif_maxLen),
      function(x) sum(stringr::str_detect(string = motif_maxLen$Motif[x], pattern = motif_df$Motif))
    )

    # iterate through all max-length motifs to omit any detected submotifs
    for(i in 1:nrow(motif_maxLen)){

      # only keep shorter motifs if they are not submotifs for any roots
      motif_df <- motif_df %>%
        dplyr::filter(stringr::str_detect(string = motif_maxLen$Motif[i], pattern = motif_df$Motif, negate = TRUE))
    }

    # append max-length motifs to data.frame of motif roots
    motif_roots <- rbind(motif_roots, motif_maxLen)
  }

  # merge roots with FSBC stats
  motif_roots <- motif_roots %>%
    dplyr::select(-.data$Motif_Length) %>%
    dplyr::left_join(motif_discovery_df, by = "Motif")

  # return merged data
  return(motif_roots)
}
