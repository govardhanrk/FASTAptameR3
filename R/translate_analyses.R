#' Format the translation mapping for `fa_translate`
#'
#' `fa_translate_mapping` is initialized with a standard genetic code and can be modified with a user-defined data.frame of changes to said code.
#' The columns of this data.frame represent the three-letter codon and the corresponding one-letter amino acid.
#' The standard genetic codes are specified in `fa_translate`.
#' Importantly, this is a helper function that is not intended to be used outside of `fa_translate`.
#'
#' @param inputChanges A data.frame with two columns (Codon and Translation), indicating modifications to the specificed genetic code
#' @param translateSelection A standard name that refers to a specific JSON file with codon usage information
#'
#' @seealso [fa_translate()]
#'
#' @return A dictionary-like data.frame to use for translating sequences
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_translate_mapping <- function(inputChanges = NULL, translateSelection = "Standard"){

  # load translations
  load("data/translations.rda")

  # read JSON file with standard genetic code
  translationDF <- translations %>% dplyr::filter(.data$whichGroup == translateSelection) %>% dplyr::select(-.data$whichGroup)

  # if input codons or translations are each of length 0, return unmodified data.frame
  if(is.null(inputChanges)){
    message("No changes to translation mapping!")
    return(translationDF)
  }

  # omit special characters; ensure codons are length 3 and that translations are length 1
  inputChanges <- inputChanges %>%
    dplyr::mutate(
      Codon = gsub("[^a-zA-Z0-9]", "", .data$Codon),
      Translation = gsub("[^a-zA-Z0-9]", "", .data$Translation)
    ) %>%
    dplyr::filter(nchar(.data$Codon) == 3 & nchar(.data$Translation) == 1)

  # make modifications to the translation
  translationDF <- dplyr::full_join(translationDF, inputChanges, by = "Codon") %>%
    dplyr::mutate(Translation = ifelse(is.na(.data$Translation.y), .data$Translation.x, .data$Translation.y)) %>%
    dplyr::select(.data$Codon, .data$Translation)

  # return modified mappings
  return(translationDF)
}

#' Translate sequences according to a provided genetic code or a user-defined genetic code
#'
#' `fa_translate` translates nucleotide sequences in a counted data.frame to amino acid sequences based on a standard or modified genetic code.
#' The open reading frame can be set with the `orf` parameter.
#' Optionally, translated sequences can be recounted (set `converge` = TRUE) such that the counts of unique nucleotide sequences that code for the same protein sequence are summed together.
#' The following genetic codes are recognized:
#' 1. Standard
#' 2. Vertebrate mitochondrial
#' 3. Yeast mitochondrial
#' 4. Mold, protozoan, and coelenterate mitochondrial + Mycoplasma / Spiroplasma
#' 5. Invertebrate mitochondrial
#' 6. Ciliate, dasycladacean and Hexamita nuclear
#' 7. Echinoderm and flatworm mitochondrial
#' 8. Euplotid nuclear
#' 9. Alternative yeast nuclear
#' 10. Ascidian mitochondrial
#' 11. Alternative flatworm mitochondrial
#' 12. Blepharisma nuclear
#' 13. Chlorophycean mitochondrial
#' 14. Trematode mitochondrial
#' 15. Scenedesmus obliquus mitochondrial
#' 16. Pterobranchia mitochondrial
#' Additionally, these codes can be modified by supplying a two-column data.frame consisting of 3-letter codons and the resulting 1-letter amino acid codes.
#'
#' @param faDF A FASTAptameR data.frame
#' @param orf The open reading frame to use for translating (default: 1, implying that translations should start at position 1)
#' @param converge A logical indicating whether sequences should be recounted after translating them
#' @param inputChanges A data.frame with two columns (Codon and Translation), indicating modifications to the specificed genetic code
#' @param translateSelection A standard name that refers to a specific JSON file with codon usage information
#'
#' @seealso [fa_count()], [fa_count_rpr()], [fa_count_histogram()]
#'
#' @return A data.frame with translated sequences
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # example of how to format the "inputChanges" data.frame
#' inputChanges <- data.frame(Codon = "AAA", Translation = "Z")
#'
#' # translate data
#' translateData <- fa_translate(faDF = countData, inputChanges = inputChanges)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_translate <- function(faDF = NULL, orf = 1, converge = TRUE, inputChanges = NULL, translateSelection = "Standard"){

  # get sequence IDs
  seqIDs <- faDF %>% dplyr::pull(.data$ID)

  # make all sequences capital, modify by ORF (trim start of string), convert T to U,
  # remove end characters if length is not divisible by 3 (would not be translated),
  # split into groups of three, remove white space at end of split strings
  Sequences <- faDF %>%
    dplyr::pull(.data$Sequences) %>%
    as.character() %>%
    toupper() %>%
    substring(first = orf) %>%
    gsub(pattern = "T", replacement = "U") %>%
    sapply(function(x) gsub(paste0(".{", nchar(x) %% 3, "}$"), "", x)) %>%
    as.vector() %>%
    gsub(pattern = "(.{3})", replacement = "\\1 ") %>%
    trimws(which = "right")

  # translate sequences: ambiguous codons to X, remove white spaces after translation
  translationMapping <- fa_translate_mapping(inputChanges = inputChanges, translateSelection = translateSelection)

  # translate sequences according to user modifications (if any)
  for(i in 1:nrow(translationMapping)){
    Sequences <- gsub(translationMapping$Codon[i], translationMapping$Translation[i], Sequences)
  }

  # finally, remove white spaces between amino acids
  Sequences <- gsub(" ", "", Sequences)

  # make data.frame from IDs and Sequences
  translateDF <- data.frame(ID = seqIDs, Sequences = Sequences) %>%
    dplyr::mutate(temp = gsub(">", "", .data$ID)) %>%
    tidyr::separate(col = .data$temp, into = c("Rank", "Reads", "RPU", "Cluster", "RankInCluster", "LED"), sep = "-", convert = TRUE) %>%
    janitor::remove_empty(which = "cols")

  # optionally merge non-unique amino acid sequences
  if(converge){

    translateDF <- translateDF %>%
      dplyr::select(.data$Reads, .data$RPU, .data$Sequences) %>%
      dplyr::group_by(.data$Sequences) %>%
      dplyr::summarise(Reads = sum(as.numeric(.data$Reads)), RPU = sum(as.numeric(.data$RPU))) %>%
      dplyr::arrange(-.data$Reads) %>%
      tibble::rowid_to_column(var = "Rank") %>%
      dplyr::mutate(ID = glue::glue(">{Rank}-{Reads}-{RPU}")) %>%
      dplyr::left_join(as.data.frame(table(Sequences))) %>%
      dplyr::rename(Unique.Nts = .data$Freq) %>%
      dplyr::select(.data$ID, .data$Rank, .data$Reads, .data$RPU, .data$Unique.Nts, .data$Sequences)
  }

  # add length to data.frame
  translateDF <- translateDF %>%
    dplyr::mutate(Length = nchar(.data$Sequences)) %>%
    dplyr::relocate(.data$Length, .before = .data$Sequences)

  # return translated data.frame
  return(translateDF)
}
