#' Make text in ggplot2 objects bold, and set font family, size, and face
#'
#' @param p A ggplot2 object created by a FASTAptameR visualization command
#'
#' @return The same ggplot2 object with updated aesthetics
#' @export
#'
#' @importFrom rlang .data
boldPlots <- function(p = NULL){

  # adjust theme
  p <- p +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times New Roman", size=14, face= "bold", colour= "black"),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.3)
    )

  # return modified plot
  return(p)
}

#' Read a FASTAptameR-formatted FASTA file into a data.frame
#'
#' @param fastaInput A FASTA file that is in the FASTAptameR format: sequence identifiers and sequences on alternating lines; the identifier must contain at least the rank, read count, and RPU in a hyphen-separated string
#'
#' @seealso [fa_count()], [fa_formatOutput()]
#'
#' @return A data.frame that contains the same information from the FASTAptameR FASTA file
#' @export
#'
#' @examples
#' \dontrun{
#' # read into data.frame
#' countData <- fa_formatInput(fastaInput = "PATH/TO/FASTAPTAMER.fasta")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_formatInput <- function(fastaInput = NULL){

  # read data, save IDs, convert sequences to data.frame
  fastaData <- readLines(fastaInput)

  # based on the sequence identifier, determine how many columns should be used
  whichcols <- ifelse(stringr::str_count(fastaData[1], pattern = "-") == 2, 3, 6)

  # make data.frame from FASTA file
  fastaDF <- data.frame(
    ID = fastaData[seq(1, length(fastaData), 2)],
    Sequences = fastaData[seq(2, length(fastaData), 2)]
  ) %>%
    dplyr::mutate(temp = gsub(">", "", .data$ID)) %>%
    tidyr::separate(
      col = .data$temp,
      into = c("Rank", "Reads", "RPU", "Cluster", "RankInCluster", "LED")[1:whichcols],
      sep = "-",
      convert = TRUE
    ) %>%
    janitor::remove_empty(which = "cols")

  # return the data.frame
  return(fastaDF)
}

#' Format a FASTAptameR data.frame as a FASTA file
#'
#' @param outputData A FASTAptameR data.frame like the one generated from fa_formatInput
#'
#' @seealso [fa_count()], [fa_formatInput()]
#'
#' @return A FASTA-like vector with the same information as the FASTAptameR data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # format as FASTA, and write to file
#' countFASTA <- fa_formatOutput(outputData = countData)
#' writeLines(countFASTA, "PATH/TO/OUTPUT.fasta")
#' }
#'
#' @importFrom rlang .data
fa_formatOutput <- function(outputData = NULL){

  # create shape for FASTA output
  fastaOutput <- rep(NA, nrow(outputData)*2)

  # add sequence IDs and sequences
  fastaOutput[seq(1,length(fastaOutput),2)] <- outputData$ID
  fastaOutput[seq(2,length(fastaOutput),2)] <- as.character(outputData$Sequences)

  # return the data structure
  return(fastaOutput)
}
