#' Recluster two pre-clustered populations
#'
#' `fa_recluster` merges clustered sequences from one population into another one, generating super-clusters.
#' Consider the sets of seed sequences from two populations, A and B.
#' When a seed sequence in B is within a user-defined LED of a seed sequence in A, all sequences from the corresponding B cluster are added into the A cluster.
#' The seed sequence for these super-clusters corresponds to the sequence with the highest RPU. Thus, the same scaling factor should be used when RPU is calculated in `fa_count`.
#' The final, reclustered data.frame contains columns for sequence enrichment between populations, rank in cluster, and distance to cluster seed.
#'
#' @param faDF1_cluster A clustered FASTAptameR data.frame
#' @param faDF2_cluster A clustered FASTAptameR data.frame
#' @param led_threshold The maximum Levenshtein edit distance (LED) between a seed sequence and a new sequence (default: 7)
#'
#' @seealso [fa_count()], [fa_clusterLED()], [fa_recluster_heatmap()], [fa_recluster_popSize()], [fa_recluster_RPU()], [fa_recluster_LED()], [fa_recluster_enrich()]
#'
#' @return A data.frame with reclustered sequences
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' clusterData <- list(
#'   fa_clusterLED(faDF = countData[[1]]),
#'   fa_clusterLED(faDF = countData[[2]])
#' )
#' reclusterData <- fa_recluster(
#'   faDF1_cluster = clusterData[[1]],
#'   faDF2_cluster = clusterData[[2]]
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster <- function(faDF1_cluster = NULL, faDF2_cluster = NULL, led_threshold = 7){

  # get seeds for each population
  p1_seeds <- faDF1_cluster %>% dplyr::filter(.data$RankInCluster == 1) %>% dplyr::pull(.data$Sequences)
  p2_seeds <- faDF2_cluster %>% dplyr::filter(.data$RankInCluster == 1) %>% dplyr::pull(.data$Sequences)

  # compute LED between all cluster; rows = population 1, cols = population 2
  led_matrix <- utils::adist(x = p1_seeds, y = p2_seeds)

  # mark all cluster seeds in population 2 that are within some LED of cluster seeds in population 1
  clusters2merge <- lapply(1:nrow(led_matrix), function(x) which(led_matrix[x,] < led_threshold)) %>%
    lapply(function(x) ifelse(length(x) == 0, NA, x))

  # data.frame with unmerged clusters
  unmerged_clusters <- faDF2_cluster %>%
    dplyr::select(-c(.data$RankInCluster, .data$LED)) %>%
    dplyr::mutate(Population = 2)

  # new data.frame for merged clusters
  merged_clusters <- faDF1_cluster %>%
    dplyr::select(-c(.data$RankInCluster, .data$LED)) %>%
    dplyr::mutate(Population = 1)

  # strip clusters from population 2 to add to population 1
  for(i in 1:length(clusters2merge)){

    # check if the cluster has a valid merge
    if(!is.na(clusters2merge[[i]])){

      # add cluster from population 2 to new data.frame
      merged_clusters <- rbind(
        merged_clusters,
        unmerged_clusters %>%
          dplyr::filter(.data$Cluster == clusters2merge[[i]]) %>%
          dplyr::mutate(Cluster = i)
      )

      # remove newly added cluster from the unmerged population
      unmerged_clusters <- unmerged_clusters %>%
        dplyr::filter(.data$Cluster != clusters2merge[[i]])
    }
  }

  # add unmerged clusters to population
  for(cl in unique(unmerged_clusters$Cluster)){

    # add unmerged cluster to data.frame
    merged_clusters <- rbind(
      merged_clusters,
      unmerged_clusters %>%
        dplyr::filter(.data$Cluster == cl) %>%
        dplyr::mutate(Cluster = max(merged_clusters$Cluster) + 1)
    )

    # remove newly added cluster from the leftover population
    unmerged_clusters <- unmerged_clusters %>% dplyr::filter(.data$Cluster != cl)
  }

  # split by population and perform full join
  merge_list <- merged_clusters %>%
    dplyr::group_by(.data$Population) %>%
    dplyr::group_split() %>%
    as.list()

  # rename columns in list to preserve them after merging
  for(i in 1:length(merge_list)){

    # how to rename populations
    population_names <- c("ID", "Rank", "Reads", "RPU", "Cluster", "RankInCluster", "LED")
    names(population_names) <- paste0(population_names, ".", letters[i])

    # add renamed data to list
    merge_list[[i]] <- merge_list[[i]] %>%
      dplyr::rename(dplyr::any_of(population_names)) %>%
      dplyr::select(!.data$Population)
  }

  # full join + formatting, and then compute enrichment and log2(enrichment)
  merge_df <- merge_list %>%
    plyr::join_all(by = "Sequences", type = "full") %>%
    dplyr::mutate(Cluster = ifelse(is.na(.data$Cluster.a), .data$Cluster.b, .data$Cluster.a)) %>%
    dplyr::select(-c(.data$Cluster.a, .data$Cluster.b)) %>%
    dplyr::relocate(c(.data$Sequences, .data$Cluster), .before = .data$ID.a) %>%
    dplyr::mutate(
      Enrichment = round(.data$RPU.b / .data$RPU.a, 3),
      log2E = round(log2(.data$RPU.b / .data$RPU.a), 3)
    )

  # add cluster rank
  merge_df <- merge_df %>%
    dplyr::group_by(.data$Cluster) %>%
    dplyr::mutate(RankInCluster = rank(-.data$Enrichment, ties.method = "first"), LED = NA) %>%
    dplyr::ungroup()

  # initialize temporary data.frame
  temp <- data.frame()

  # iterate through all unique clusters to get LED to cluster seed
  for(cl in unique(merge_df$Cluster)){

    # get sequence that is ranked #1 in cluster
    seq_maxRPU <- merge_df %>% dplyr::filter(.data$Cluster == cl & .data$RankInCluster == 1) %>% dplyr::pull(.data$Sequences)

    # add LED between every sequence and its cluster seed
    temp <- rbind(
      temp,
      merge_df %>% dplyr::filter(.data$Cluster == cl) %>% dplyr::mutate(LED = drop(utils::adist(x = seq_maxRPU, y = .data$Sequences)))
    )
  }

  # move cluster statistics to be after the cluster column, and then sort by cluster and rank
  temp <- temp %>%
    dplyr::relocate(c(.data$RankInCluster, .data$LED), .after = .data$Cluster) %>%
    dplyr::arrange(.data$Cluster, dplyr::desc(.data$Enrichment))

  # return reclustered data
  return(temp)
}
