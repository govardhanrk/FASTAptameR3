#' Format a motif query list as a Perl-like regular expression
#'
#' @param motifList A comma-separated list of motifs (i.e., a vector of length 1)
#' @param motifType One of Nucleotide (default) or Protein; when Nucleotide is selected, the script will interpret nucleotide ambiguity codes
#'
#' @return A formatted list of motifs
#'
#' @importFrom dplyr %>%
fa_motif_format <- function(motifList = NULL, motifType = "Nucleotide"){

  # format motif(s) as a Perl query; if "Nucleotide" is specified, consider the degenerate codes
  if(motifType == "Nucleotide"){

    motifList <- motifList %>%
      paste(collapse = ",") %>%
      toupper() %>%
      gsub(pattern = " ", replacement = "") %>%
      gsub(pattern = "R", replacement = "[AG]") %>%
      gsub(pattern = "Y", replacement = "[CT]") %>%
      gsub(pattern = "W", replacement = "[AT]") %>%
      gsub(pattern = "S", replacement = "[GC]") %>%
      gsub(pattern = "M", replacement = "[AC]") %>%
      gsub(pattern = "K", replacement = "[GT]") %>%
      gsub(pattern = "B", replacement = "[^A]") %>%
      gsub(pattern = "D", replacement = "[^C]") %>%
      gsub(pattern = "H", replacement = "[^G]") %>%
      gsub(pattern = "V", replacement = "[^T]") %>%
      gsub(pattern = "N", replacement = "[ACGT]") %>%
      gsub(pattern = ",", replacement = "|")
  } else{

    motifList <- motifList %>%
      paste(collapse = ",") %>%
      toupper() %>%
      gsub(pattern = " ", replacement = "") %>%
      gsub(pattern = ",", replacement = "|")
  }

  # return formatted motif(s)
  return(motifList)
}
