#' Merge counted data.frames according to user-defined criteria
#'
#' `fa_dataMerge` merges a list of counted data.frames with a full, inner, or left join.
#'
#' @param faDF_list A list of data.frames generated with FASTAptameR
#' @param mergeType The type of merge to use when joining all data.frames in the list; "full" (default), "inner", or "left"
#'
#' @seealso [fa_count()], [fa_dataMerge_seqPersistence()], [fa_dataMerge_UpSetR()]
#'
#' @return A merged data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countDataList <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE3.fastq")
#' )
#'
#' # full merge
#' fa_dataMerge(faDF_list = countDataList, mergeType = "full")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_dataMerge <- function(faDF_list = NULL, mergeType = "full"){

  # add each DF-converted FASTA file to the list
  for(i in 1:length(faDF_list)){

    # how to rename populations
    population_names <- c("ID", "Rank", "Reads", "RPU", "Cluster", "RankInCluster", "LED")
    names(population_names) <- paste0(population_names, ".", letters[i])

    # add renamed data to list
    fa2DF_list[[i]] <- faDF_list[[i]] %>%
      dplyr::rename(dplyr::any_of(population_names))
  }

  # do the merging
  mergeDF <- plyr::join_all(faDF_list, by = "Sequences", type = mergeType)

  # move sequences to first column
  mergeDF <- mergeDF %>% dplyr::select(.data$Sequences, dplyr::everything())

  # return the merged data.frame
  return(mergeDF)
}
