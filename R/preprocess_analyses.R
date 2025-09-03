#' Preprocess FASTA / FASTQ files before counting them
#'
#' `fa_preprocess` 1) trims user-defined constant regions off sequences, 2) omits sequences with a length outside a user-defined range, and 3) omits sequences with an average error above a user-defined threshold.
#' Trimming occurs with a fuzzy match algorithm that searches for the constant 5' and/or 3' subsequences at the start or end of the sequences, respectively.
#' The error filter is based on the average Phred quality score for the entire sequence.
#'
#' @param inputPath The file path to the FASTA / FASTQ file to preprocess; can be gzipped
#' @param const5p A character string corresponding to the 5' constant region to trim (default: "", implying no 5' constant region)
#' @param const3p A character string corresponding to the 3' constant region to trim (default: "", implying no 5' constant region)
#' @param length_range An integer vector corresponding to the shortest and longest allowable sequences after trimming constant regions (default: 10, 100)
#' @param max_error The maximum allowed error in Phred quality (default: 0.005)
#'
#' @seealso [fa_count()], [utils::aregexec()], [Biostrings::PhredQuality()]
#'
#' @return A data.frame with preprocessed sequences; this still needs to pass through `fa_count` before it can be used in other FASTAptameR modules
#' @export
#'
#' @examples
#' \dontrun{
#' # preprocess data
#' preprocessData <- fa_preprocess(
#'   inputPath = "PATH/TO/FILE.fastq",
#'   const5p = const5p,
#'   const3p = const3p,
#'   length_range = c(50, 60),
#'   max_error = 0.01
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_preprocess <- function(inputPath = NULL, const5p = "", const3p = "", length_range = c(10, 100), max_error = 0.005){

  ## Preliminaries
  # get file extension
  if(tolower(tools::file_ext(inputPath)) %in% c("fq", "fastq")){
    fext <- "fastq"
  } else if(tolower(tools::file_ext(inputPath)) %in% c("fa", "fasta")){
    fext <- "fasta"
  } else if(grepl("\\.fastq\\.gz$", inputPath)){
    fext <- "fastq"
  } else if(grepl("\\.fasta\\.gz$", inputPath)){
    fext <- "fasta"
  } else{
    return(NULL)
  }

  # read string set
  seq_set <- Biostrings::readBStringSet(
    filepath = inputPath,
    format = fext,
    use.names = TRUE,
    with.qualities = ifelse(fext == "fastq", TRUE, FALSE)
  )

  # make data.frame
  seq_df <- data.frame(Sequence = seq_set) %>% tibble::rownames_to_column(var = "ID")

  # optionally add qualities if the original file had them
  if(fext == "fastq"){
    seq_df <- cbind(seq_df, data.frame(Quality = seq_set@elementMetadata$qualities))
  }

  ## Trimming
  # optionally remove 5' constant region if it was supplied
  if(const5p != ""){

    # get positions with matches
    positions <- utils::aregexec(const5p, seq_df$Sequence, max.distance = 0.1)

    # extract sequence matches by position; set non-matches to "XXXX", which shouldn't be in the data
    res_sequence <- regmatches(seq_df$Sequence, positions)
    res_sequence[lengths(res_sequence)==0] <- "XXXX"
    seq_df$Sequence <- sapply(1:length(res_sequence), function(x) sub(res_sequence[[x]], "", seq_df$Sequence[x], fixed = TRUE))

    # handle Quality scores if they are present
    if("Quality" %in% colnames(seq_df)){

      # extract quality matches by position
      res_quality <- regmatches(seq_df$Quality, positions)
      res_quality[lengths(res_quality)==0] <- "XXXX"
      seq_df$Quality <- sapply(1:length(res_quality), function(x) sub(res_quality[[x]], "", seq_df$Quality[x], fixed = TRUE))
    }
  }

  # optionally remove 5' constant region if it was supplied
  if(const3p != ""){

    # get positions with matches
    positions <- utils::aregexec(const3p, seq_df$Sequence, max.distance = 0.1)

    # extract sequence matches by position; set non-matches to "XXXX", which shouldn't be in the data
    res_sequence <- regmatches(seq_df$Sequence, positions)
    res_sequence[lengths(res_sequence)==0] <- "XXXX"
    seq_df$Sequence <- sapply(1:length(res_sequence), function(x) sub(res_sequence[[x]], "", seq_df$Sequence[x], fixed = TRUE))

    # handle Quality scores if they are present
    if("Quality" %in% colnames(seq_df)){

      # extract quality matches by position
      res_quality <- regmatches(seq_df$Quality, positions)
      res_quality[lengths(res_quality)==0] <- "XXXX"
      seq_df$Quality <- sapply(1:length(res_quality), function(x) sub(res_quality[[x]], "", seq_df$Quality[x], fixed = TRUE))
    }
  }

  ## Length filtering
  # length filter for sequences
  seq_df <- seq_df %>% dplyr::filter(nchar(.data$Sequence) >= min(length_range) & nchar(.data$Sequence) <= max(length_range))

  ## Quality filtering
  # if there is a column for Quality, get average error and filter
  if("Quality" %in% colnames(seq_df)){

    seq_df <- seq_df %>% dplyr::mutate(
      Avg_Error = methods::as(Biostrings::PhredQuality(seq_df$Quality), "NumericList") %>%
        lapply(function(x) mean(x)) %>%
        unlist()
    ) %>%
      dplyr::filter(.data$Avg_Error <= max_error)
  }

  # return trimmed, length-filtered data.frame
  return(seq_df)
}
