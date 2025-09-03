#' Compute the LED between a query sequence and every other sequence in the target file
#'
#' `fa_distance` computes the LED between a user-defined query sequence and every other sequence in the input data.frame.
#'
#' @param faDF A FASTAptameR data.frame
#' @param querySequence A query sequence used as a reference for computing the Levenshtein edit distance (LED)
#'
#' @seealso [fa_count()], [fa_distance_histogram()]
#'
#' @return A data.frame with a column for the LED between the sequence and query sequence
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # compute distance
#' fa_distance(faDF = countData, querySequence = query)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_distance <- function(faDF = NULL, querySequence = NULL){

  # get length of query sequence
  seqLength <- nchar(querySequence)

  # compute LED between each target sequence and the query sequence
  distanceDF <- faDF %>%
    dplyr::mutate(Distance = drop(utils::adist(x = toupper(.data$Sequences), y = toupper(querySequence)))) %>%
    dplyr::select(.data$ID, .data$Rank, .data$Reads, .data$RPU, .data$Sequences, .data$Distance)

  # order by distance and rearrange columns
  distanceDF <- distanceDF %>% dplyr::arrange(.data$Distance)

  # return data.frame after ordering by distance
  return(distanceDF)
}
