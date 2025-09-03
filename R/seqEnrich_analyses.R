#' Compute the enrichment of sequences from multiple populations
#'
#' `fa_enrich` merges two counted data.frames by sequence and computes sequence enrichment by considering the ratio of the RPU values.
#' Thus, it is critical to use the same scaling factor when normalizing the read counts in `fa_count`.
#'
#' @param faDF1 A FASTAptameR data.frame
#' @param faDF2 A FASTAptameR data.frame
#' @param keepNA Logical indicating if sequences only present in a single population should be kept (default: FALSE)
#'
#' @seealso [fa_count()], [fa_enrich_histogram()] [fa_enrich_scatter()], [fa_enrich_ra()]
#'
#' @return A merged data.frame with columns for enrichment and log2(enrichment)
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countDataList <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#'
#' # compute sequence enrichment between the two files
#' enrichData <- fa_enrich(faDF1 = countDataList[[1]], faDF2 = countDataList[[2]])
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_enrich <- function(faDF1 = NULL, faDF2 = NULL, keepNA = FALSE){

  # initialize list of formatted files
  sequenceDF_list <- list(faDF1, faDF2)

  # read and format all input files; store them as a list
  for(i in 1:length(sequenceDF_list)){

    # how to rename populations
    population_names <- c("ID", "Rank", "Reads", "RPU", "Cluster", "RankInCluster", "LED")
    names(population_names) <- paste0(population_names, ".", letters[i])

    # add renamed data to list
    sequenceDF_list[[i]] <- sequenceDF_list[[i]] %>% dplyr::rename(dplyr::any_of(population_names))
  }

  # full or inner join, depending on keepNA
  mergeDF <- plyr::join_all(sequenceDF_list, by = "Sequences", type = ifelse(keepNA, "full", "inner"))

  # move sequence column to first column, calculate enrichment and log2(enrichment)
  mergeDF <- mergeDF %>%
    dplyr::select(.data$Sequences, dplyr::everything()) %>%
    dplyr::mutate(
      Enrichment = round(.data$RPU.b / .data$RPU.a, 3),
      log2E = round(log2(.data$RPU.b / .data$RPU.a), 3)
    ) %>%
    replace(is.na(), 0) %>%
    dplyr::arrange(.data$Rank.a)

  return(mergeDF)
}
