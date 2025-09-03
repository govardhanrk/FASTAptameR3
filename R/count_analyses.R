#' Count the number of occurrences of each unique sequence in a FASTA / FASTQ file
#'
#' `fa_count` counts the number of occurrences for all unique sequences in an input FASTA / FASTQ file (that can be gzipped) and sorts sequences by read count.
#' Each sequence in the final data.frame has the following features: read count, normalized read count (RPU = Reads Per Unit, where "unit" is a user-defined scaling factor), and sequence length.
#' The sequence ID is a FASTA identifier in this form: ">{Rank}-{Reads}-{RPU}".
#'
#' @param dataInput Path to a FASTA or FASTQ file; can be gzipped
#' @param reverseComplement A logical value indicating if the function should return the reverse complement of sequences
#' @param scaling_factor The numeric value used to calculate RPU (Reads Per Unit); 1e6 = Reads per Million
#'
#' @seealso [fa_preprocess()], [fa_recount()], [fa_count_rpr()], [fa_count_histogram()], [fa_count_binnedAbundance()]
#'
#' @return A data.frame with one row per unique sequence in the input file; each sequence includes its read count, normalized read count, and rank by read count
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_count <- function(dataInput = NULL, reverseComplement = FALSE, scaling_factor = 1e6){

  # bind variable
  var <- Sequences <- Length <- RPU <- ID <- NULL

  # get start time
  startTime <- Sys.time()

  # read FASTQ/A file and keep the sequences in a data.frame
  if(toupper(tools::file_ext(dataInput)) == "FASTQ"){

    allSeqs <- data.frame(Sequences = LaF::get_lines(dataInput, line_numbers = seq(2, LaF::determine_nlines(dataInput), by = 4)))
  } else if(toupper(tools::file_ext(dataInput)) == "FASTA"){

    # check if sequences have IDs
    if(readLines(dataInput, n = 1) %>% substr(1, 1) == ">"){

      allSeqs <- data.frame(Sequences = LaF::get_lines(dataInput, line_numbers = seq(2, LaF::determine_nlines(dataInput), by = 2)))
    } else{

      allSeqs <- data.frame(Sequences = LaF::get_lines(dataInput, line_numbers = seq(1, LaF::determine_nlines(dataInput), by = 1)))
    }
  } else if(toupper(tools::file_ext(dataInput)) == "GZ"){

    # check if FASTQ or FASTA
    if(gsub(paste0("\\.", tools::file_ext(dataInput)), "", dataInput) %>% tools::file_ext() %>% toupper() == "FASTQ"){

      allSeqs <- data.frame(Sequences = Biostrings::readBStringSet(dataInput, format = "fastq"))
      rownames(allSeqs) <- NULL
    } else if(gsub(paste0("\\.", tools::file_ext(dataInput)), "", dataInput) %>% tools::file_ext() %>% toupper() == "FASTA"){

      allSeqs <- data.frame(Sequences = Biostrings::readBStringSet(dataInput, format = "fasta"))
      rownames(allSeqs) <- NULL
    }
  } else{

    return(NULL)
  }

  # define function for counting
  countFxn <- function(x){
    data.table::data.table(x)[, data.table::.N, keyby = x]
  }

  # count/sort unique sequences; add rank, RPU, and sequence ID to data.frame
  seqCounts <- countFxn(allSeqs$Sequences) %>%
    as.data.frame() %>%
    dplyr::rename(Sequences = .data$x, Reads = .data$N) %>%
    dplyr::arrange(dplyr::desc(.data$Reads)) %>%
    tibble::rowid_to_column(var = "Rank") %>%
    dplyr::mutate(
      Sequences = gsub("\r", "", as.character(.data$Sequences)),
      RPU = round(.data$Reads / (sum(.data$Reads) / scaling_factor), 0),
      Length = as.numeric(nchar(.data$Sequences)),
      ID = paste0(">", .data$Rank, "-", .data$Reads, "-", .data$RPU)
    ) %>%
    dplyr::relocate(.data$ID, .data$Rank, .data$Reads, .data$RPU, .data$Length, .data$Sequences)

  # optionally make reverse complement
  if(reverseComplement){
    seqCounts$Sequences <- Biostrings::DNAStringSet(seqCounts$Sequences) %>%
      Biostrings::reverseComplement() %>%
      as.character()
  }

  # message elapsed time
  message(gsub("Time difference of", "Elapsed time:", utils::capture.output(round(Sys.time() - startTime, 2))))

  # return data.frame
  return(seqCounts)
}

#' Compute metadata for a counted data.frame
#'
#' `fa_count_metadata` prints a message containing the number of unique sequences, total number of reads, and the Simpson's index of diversity for a data.frame generated with `fa_count`.
#'
#' @param countData A data.frame from fa_count
#'
#' @seealso [fa_count()]
#'
#' @return A vector of metadata: total sequence count, unique sequence count, Simpson's index of diversity
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, compute metadata
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' fa_count_metadata(countData = countData)
#' }
#'
fa_count_metadata <- function(countData = NULL){

  # get number of (non-)redundant sequences
  uniqueSeqs <- paste0("Unique sequences: ", nrow(countData))
  totalSeqs <- paste0("Total sequences: ", sum(countData$Reads))

  # get Simpson's Index of Diversity
  sid <- paste0(
    "Simpson's Index of Diversity: ",
    round(1 - sum(countData$Reads * (countData$Reads - 1)) / (sum(countData$Reads) * (sum(countData$Reads) - 1)), 2)
  )

  # return number of (non-)redundant sequences
  return(c(totalSeqs, uniqueSeqs, sid))
}
