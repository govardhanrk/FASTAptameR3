#' Track motif occurrences across populations
#'
#' `fa_motif_motifTracker` tracks the occurrences of motifs across multiple input populations.
#'
#' @param faDF_list A list of FASTAptameR data.frames; there should be one data.frame per population name
#' @param populationNames A list of names for each data.frame; there should be one population name per data.frame
#' @param queryList A list of motifs to track across populations
#' @param queryAliases An optional list of aliases for the motifs; if supplied, there must be one alias per motif
#' @param motifType One of Nucleotide (default) or Protein; the Nucleotide option considers nucleotide ambiguity codes
#'
#' @seealso [fa_count()], [fa_motif_sequenceTracker()], [fa_motif_trackerEnrichment()], [fa_motif_motifTrackerPlot()]
#'
#' @return A data.frame with data related to motif occurrence(s) across multiple populations
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#'
#' # count motifs in input files
#' trackerData <- fa_motif_motifTracker(
#'   faDF_list = countData,
#'   populationNames = c("Population A", "Population B"),
#'   queryList = c("AAA", "AACG"),
#'   queryAliases = c("motif1", "motif2"),
#'   motifType = "Nucleotide"
#' )
#'
#' # compute motif enrichment
#' enrichData <- fa_motif_trackerEnrichment(trackerDF = trackerData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motif_motifTracker <- function(faDF_list = NULL, populationNames = NULL, queryList = NULL, queryAliases = NULL, motifType = "Nucleotide"){

  # for each query, format the motif
  queryList_mod <- sapply(queryList, function(x) fa_motif_format(motifList = x, motifType = motifType)) %>% as.vector()
  queryList_mod <- paste0("(?=.*", queryList_mod, ")") %>% gsub(pattern = "\\|", replacement = ")(?=.*")

  # initialize target data.frame
  targetDF <- data.frame()

  # initialize list of sequence counts
  lengthList <- list()

  # iterate through each fasta file
  for(i in 1:length(faDF_list)){

    # read and format FASTA input
    formatDF <- faDF_list[[i]]

    # get number of sequences in FASTA file
    lengthList[[i]] <- sum(formatDF$Reads)

    # iterate through each formatted query
    for(j in 1:length(queryList_mod)){

      # row bind targetDF with formatted fasta input after searching for query
      targetDF <- rbind(
        targetDF,
        formatDF %>%
          dplyr::filter(grepl(queryList_mod[j], .data$Sequences, perl = TRUE)) %>%
          dplyr::mutate(Motif = queryList[j], PopulationNum = i, PopulationName = populationNames[i])
      )
    }
  }

  # unlist length list
  lengthList <- lengthList %>% unlist()

  # group by population and motif, get total reads and percentage for each motif
  targetDF <- targetDF %>%
    dplyr::group_by(.data$PopulationNum, .data$PopulationName, .data$Motif) %>%
    dplyr::summarise(TotalReads = sum(.data$Reads)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Percentage = round(.data$TotalReads / lengthList[.data$PopulationNum] * 100, 2)) %>%
    dplyr::arrange(.data$Motif)

  # add Alias column if they were supplied
  if(length(queryAliases) != 0){

    targetDF <- targetDF %>%
      dplyr::left_join(data.frame(Motif = .data$queryList, Alias = .data$queryAliases), by = "Motif") %>%
      dplyr::select(.data$PopulationNum, .data$PopulationName, .data$Motif, .data$Alias, .data$TotalReads, .data$Percentage)
  }

  # return targetDF
  return(targetDF)
}

#' Track sequence occurrence across populations
#'
#' `fa_motif_sequenceTracker` tracks the occurrences of sequences across multiple input populations.
#'
#' @param faDF_list A list of FASTAptameR data.frames; there should be one data.frame per population name
#' @param populationNames A list of names for each data.frame; there should be one population name per data.frame
#' @param queryList A list of sequences to track across populations
#' @param queryAliases An optional list of aliases for the motifs; if supplied, there must be one alias per motif
#'
#' @seealso [fa_count()], [fa_motif_motifTracker()], [fa_motif_trackerEnrichment()], [fa_motif_sequenceTrackerPlot()]
#'
#' @return A data.frame with data related to sequence occurrence(s) across multiple populations
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#'
#' # count sequence occurrences across files
#' trackerData <- fa_motif_sequenceTracker(
#'   faDF_list = countData,
#'   populationNames = c("Population A", "Population B"),
#'   queryList = c(sequence1, sequence2),
#'   queryAliases = c("seq1", "seq2")
#' )
#'
#' # compute sequence enrichment
#' enrichData <- fa_motif_trackerEnrichment(trackerDF = trackerData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motif_sequenceTracker <- function(faDF_list = NULL, populationNames = NULL, queryList = NULL, queryAliases = NULL){

  # initialize targetDF
  targetDF <- data.frame()

  # iterate through all input files; format file, pull sequences from query list, add population; bind to previous df
  for(i in 1:length(faDF_list)){
    targetDF <- rbind(
      targetDF,
      faDF_list[[i]] %>%
        dplyr::filter(.data$Sequences %in% queryList) %>%
        dplyr::mutate(PopulationNum = i, PopulationName = populationNames[i])
    )
  }

  # rearrange columns and sort by Sequences
  targetDF <- targetDF %>%
    dplyr::select(.data$PopulationNum, .data$PopulationName, .data$Sequences, .data$Rank, .data$Reads, .data$RPU, -.data$ID) %>%
    dplyr::arrange(.data$Sequences)

  # add Alias column if they were supplied
  if(length(queryAliases) != 0){

    targetDF <- targetDF %>%
      dplyr::left_join(data.frame(Sequences = queryList, Alias = queryAliases), by = "Sequences") %>%
      dplyr::select(.data$PopulationNum, .data$PopulationName, .data$Sequences, .data$Alias, .data$Rank, .data$Reads, .data$RPU)
  }

  # return targetDF
  return(targetDF)
}

#' Compute enrichment of specific motifs or sequences across populations
#'
#' `fa_motif_trackerEnrichment` computes the enrichment of user-defined motifs or sequences from `fa_motif_motifTracker` or `fa_motif_sequenceTracker`.
#'
#' @param trackerDF A data.frame from `fa_motif_motifTracker` or `fa_motif_sequenceTracker`
#'
#' @seealso [fa_motif_motifTracker()], [fa_motif_sequenceTracker()]
#'
#' @return A data.frame with enrichment scores for each query
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#'
#' # count motifs in input files
#' trackerData <- fa_motif_motifTracker(
#'   faDF_list = countData,
#'   populationNames = c("Population A", "Population B"),
#'   queryList = c("AAA", "AACG"),
#'   queryAliases = c("motif1", "motif2"),
#'   motifType = "Nucleotide"
#' )
#'
#' # compute motif enrichment
#' enrichData <- fa_motif_trackerEnrichment(trackerDF = trackerData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motif_trackerEnrichment <- function(trackerDF = NULL){

  # ungroup data.frame and rename 3rd column (either Sequences or Motif) to Query
  trackerDF <- trackerDF %>%
    dplyr::ungroup() %>%
    dplyr::rename(dplyr::any_of(c(Query = "Motif", Query = "Sequences")))

  # initialize empty data.frame to hold enrichment scores
  enrichmentDF <- data.frame()

  # fill enrichmentDF with one query at a time
  for(query in unique(trackerDF$Query)){

    # split tibble by query
    temp <- trackerDF %>% dplyr::filter(.data$Query == query) %>% as.data.frame()

    # iterate through each consecutive population pair
    for(i in 2:nrow(temp)){

      # fill enrichmentDF with comparisons and corresponding enrichments
      enrichmentDF <- rbind(
        enrichmentDF,
        data.frame(
          Comparison = paste0(temp$PopulationName[i], " : ", temp$PopulationName[i-1]),
          Query = query,
          Alias = ifelse("Alias" %in% colnames(temp), temp$Alias[i], "-"),
          Enrichment = round(temp[i, ncol(temp)] / temp[i-1, ncol(temp)], 2)
        )
      )
    }
  }

  return(enrichmentDF)
}
