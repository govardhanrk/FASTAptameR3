#' Keep sequences that have user-defined motifss
#'
#' `fa_motifSearch` only keeps sequences from a counted data.frame if they have at least one user-defined motif (`partial` = TRUE) or all user-defined motifs (`partial` = FALSE)
#'
#' @param faDF A FASTAptameR data.frame
#' @param motif A comma-separated list of motifs (e.g., "ACT,AAA,ACG")
#' @param highlight A logical indicating whether motifs should be placed inside of parentheses (default: FALSE)
#' @param partial A logical indicating whether partial matches are allowed (default: FALSE)
#' @param motifType A character string indicating the type of motif (default: "Nucleotide"); Nucleotide motifs allow ambiguities
#'
#' @seealso [fa_count()], [fa_motifOmit()]
#'
#' @return A filtered data.frame that only includes sequences with the supplied motifs
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # only keep sequences with a user-defined motif
#' countData_search <- fa_motifSearch(
#'   faDF = countData,
#'   motif = "UUCG,CGGG",
#'   partial = FALSE,
#'   motifType = "Nucleotide"
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motifSearch <- function(faDF = NULL, motif = NULL, highlight = FALSE, partial = FALSE, motifType = "Nucleotide"){

  # convert case of motif; convert U to T, handle degenerate code in motif
  motif <- fa_motif_format(motifList = motif, motifType = motifType)

  # filter faDF for sequences with the motif(s)
  if(partial){

    # partial filter uses the OR operator
    faDF <- faDF %>% dplyr::filter(grepl(motif, .data$Sequences))
  } else{

    # full filter repeatedly filters sequences
    fullMatchMotifs <- paste0("(?=.*", motif, ")") %>% gsub(pattern = "\\|", replacement = ")(?=.*")

    faDF <- faDF %>% dplyr::filter(grepl(fullMatchMotifs, .data$Sequences, perl = TRUE))
  }

  # put parentheses around motif
  if(highlight){

    motif <- unlist(strsplit(motif, split = "\\|"))

    for(i in 1:length(motif)){

      faDF$Sequences <- gsub(motif[i], paste0("(", motif[i], ")"), faDF$Sequences)
    }
  }

  # return filtered data
  return(faDF)
}
