#' Omit sequences with user-defined motifs
#'
#' `fa_motifOmit` omits sequences from a counted data.frame if they have at least one user-defined motif (`partial` = TRUE) or all user-defined motifs (`partial` = FALSE)
#'
#' @param faDF A FASTAptameR data.frame
#' @param motif A comma-separated list of motifs (e.g., "ACT,AAA,ACG")
#' @param partial A logical indicating whether partial matches are allowed (default: FALSE)
#' @param motifType A character string indicating the type of motif (default: "Nucleotide"); Nucleotide motifs allow ambiguities
#'
#' @seealso [fa_count()], [fa_motifSearch()]
#'
#' @return A filtered data.frame that omits sequences with the supplied motifs
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # omit sequences with a user-defined motif
#' countData_omit <- fa_motifOmit(
#'   faDF = countData,
#'   motif = "UUCG,CGGG",
#'   partial = FALSE,
#'   motifType = "Nucleotide"
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motifOmit <- function(faDF = NULL, motif = NULL, partial = FALSE, motifType = "Nucleotide"){

  # format motif input
  motif <- fa_motif_format(motifList = motif, motifType = motifType)

  # filter faDF for sequences without the motif(s)
  if(partial){

    # partial filter uses the OR operator
    faDF <- faDF %>% dplyr::filter(!grepl(motif, .data$Sequences))
  } else{

    # full filter repeatedly filters sequences
    fullMatchMotifs <- paste0("(?=.*", motif, ")") %>%
      gsub(pattern = "\\|", replacement = ")(?=.*")

    faDF <- faDF %>% dplyr::filter(!grepl(fullMatchMotifs, .data$Sequences, perl = TRUE))
  }

  # return filtered data
  return(faDF)
}
