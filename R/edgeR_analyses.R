#' Perform edgeR-based differential analysis between two sets of conditions
#'
#' `fa_edgeR_pairTest` performs a pairwise differential analysis test between two conditions.
#' Each condition should be represented as a list of counted data.frames.
#'
#' @param faDF_cond1 A list of FASTAptameR data.frames for the first condition
#' @param faDF_cond2 A list of FASTAptameR data.frames for the second condition
#' @param pcutoff A numeric threshold for the BH-corrected p-values; used to determine significance (default: 0.1)
#'
#' @seealso [fa_count()], [fa_edgeR_pairTestViz()], [edgeR::exactTest()]
#'
#' @return A data.frame with the results from the edgeR test
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files in condition 1
#' countData_cond1 <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE3.fastq")
#' )
#'
#' # count FASTQ files in condition 2
#' countData_cond2 <- list(
#'   fa_count(dataInput = "PATH/TO/FILE4.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE5.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE6.fastq")
#' )
#'
#' # use edgeR
#' edgeRData <- fa_edgeR_pairTest(
#'   faDF_cond1 = countData_cond1,
#'   faDF_cond2 = countData_cond2,
#'   pcutoff = 0.05
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_edgeR_pairTest <- function(faDF_cond1 = NULL, faDF_cond2 = NULL, pcutoff = 0.1){

  ## PREPARE DATA FOR EDGER
  # format for merging
  for(i in 1:length(faDF_cond1)){

    faDF_cond1[[i]] <- faDF_cond1[[i]] %>%
      dplyr::select(.data$Sequences, .data$ID, .data$Reads) %>%
      dplyr::rename_with(~paste0(glue::glue(".{LETTERS[i]}")), c(.data$ID, .data$Reads))
  }

  for(i in 1:length(faDF_cond2)){

    faDF_cond2[[i]] <- faDF_cond2[[i]] %>%
      dplyr::select(.data$Sequences, .data$ID, .data$Reads) %>%
      dplyr::rename_with(~paste0(., glue::glue(".{letters[i]}")), c(.data$ID, .data$Reads))
  }

  # merge each condition
  cond1_merged <- faDF_cond1 %>% plyr::join_all(by = "Sequences", type = "inner")
  cond2_merged <- faDF_cond2 %>% plyr::join_all(by = "Sequences", type = "inner")

  # merge conditions
  cond_merged <- plyr::join(cond1_merged, cond2_merged, by = "Sequences", type = "inner")

  # matrix for differential analysis
  cond_mtx <- cond_merged %>% dplyr::select(dplyr::contains("Reads")) %>% as.matrix()
  rownames(cond_mtx) <- cond_merged$Sequences

  # groups for edgeR analysis
  cond_groups <- c(rep("cond1", length(faDF_cond1)), rep("cond2", length(faDF_cond2)))

  ## edgeR analysis
  # create edgeR list object
  d <- edgeR::DGEList(counts = cond_mtx, group = factor(cond_groups))

  # amend the library size such that it represents the original populations, not the merged ones
  d$samples$lib.size <- c(
    lapply(faDF_cond1, function(x) sum(x$Reads)),
    lapply(faDF_cond2, function(x) sum(x$Reads))
  ) %>% unlist()

  # calculate normalization factors
  d <- edgeR::calcNormFactors(d)

  # estimate common and tag-wise dispersion
  d <- edgeR::estimateCommonDisp(d, verbose = TRUE)
  d <- edgeR::estimateTagwiseDisp(d)

  # paired DEA test
  d_test <- edgeR::exactTest(d, pair = c(1, 2))$table %>%
    dplyr::mutate(
      logFC = round(.data$logFC, digits = 3),
      logCPM = round(.data$logCPM, digits = 3),
      PValue = stats::p.adjust(.data$PValue, method = "BH") %>% round(digits = 3),
      PClass = ifelse(.data$PValue < pcutoff, "Sig.", "Not Sig.")
    ) %>%
    tibble::rownames_to_column(var = "Sequence") %>%
    dplyr::relocate(.data$Sequence, .after = .data$PClass)

  # return test results
  return(d_test)
}
