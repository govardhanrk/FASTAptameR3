#' Concatenate sequences from two counted data.frames
#'
#' `fa_recount` concatenates two counted data.frames, which can be useful when a single file is too large for a single run but can be split into smaller files.
#' After merging by sequence, this function adds the read count and then renormalizes it based on a common scaling factor.
#'
#' @param faDF1 A FASTAptameR data.frame
#' @param faDF2 A FASTAptameR data.frame
#' @param scaling_factor The scaling factor to be used for normalizing the read count (default: 1e6)
#'
#' @seealso [fa_count()]
#'
#' @return A FASTAptameR data.frame
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
#' # concatenate files and recompute sequence features
#' recountData <- fa_recount(faDF1 = countDataList[[1]], faDF2 = countDataList[[2]])
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recount <- function(faDF1 = NULL, faDF2 = NULL, scaling_factor = 1e6){

  # read two FASTA files as data.frames and only keep sequence + reads
  countDF1 <- faDF1 %>% dplyr::select(.data$Sequences, .data$Reads)
  countDF2 <- faDF2 %>% dplyr::select(.data$Sequences, .data$Reads)

  # merge by sequence, add read counts, recalculate RPU
  countMerge <- dplyr::full_join(countDF1, countDF2, by = "Sequences") %>%
    tidyr::replace_na(list(Reads.x = 0, Reads.y = 0)) %>%
    dplyr::mutate(Reads = .data$Reads.x + .data$Reads.y) %>%
    dplyr::select(-c(.data$Reads.x, .data$Reads.y)) %>%
    dplyr::arrange(dplyr::desc(.data$Reads)) %>%
    tibble::rowid_to_column(var = "Rank") %>%
    dplyr::mutate(
      RPU = round(.data$Reads / (sum(.data$Reads) / scaling_factor), 0),
      Length = as.numeric(nchar(.data$Sequences)),
      ID = paste0(">", .data$Rank, "-", .data$Reads, "-", .data$RPU)
    ) %>%
    dplyr::relocate(.data$ID, .data$Rank, .data$Reads, .data$RPU, .data$Length, .data$Sequences)

  # return merged data.frame
  return(countMerge)
}
