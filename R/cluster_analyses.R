#' Cluster sequences with the Levenshtein edit distance (LED)
#'
#' `fa_clusterLED` sorts input sequences according to their RPU (Reads Per Unit; computed in `fa_count`) and then omits sequences with a read count less than `minReads`.
#' The clustering algorithm proceeds according to the following steps:
#' 1. Set the most abundant, unclustered sequence as the cluster seed
#' 2. Add all unclustered sequences with a user-defined LED to the cluster
#' 3. Set aside clustered sequences and repeat
#' These steps repeat until the algorithm generates a user-defined number of clusters or until all sequences are clustered. Unclustered sequences are only kept if `keepNC` is TRUE.
#'
#' @param faDF A FASTAptameR data.frame that at least has columns for ID, Rank, Reads, RPU, and Sequence; this is generated with `fa_count` or many other functions in the package
#' @param minReads The minimum number of reads that a sequence needs to be considered for clustering (default: 10)
#' @param maxLED The maximum allowed Levenshtein Edit Distance (LED) allowed between an non-clustered sequence and a seed sequence (default: 7)
#' @param totalClusters The total number of clusters to generate; a sufficiently large number will result in the algorithm continuing until all sequences are clustered (default: 30)
#' @param keepNC A logical value that determines if non-clustered sequences should be kept (default: TRUE)
#'
#' @seealso [fa_count()], [fa_clusterDiversity()], [fa_clusterMSA()], [fa_recluster()]
#'
#' @return A FASTAptameR data.frame that includes columns for Cluster, Rank in the Cluster (determined by RPU), and the LED between the sequence and the cluster's seed sequence
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, cluster data
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' clusterData <- fa_clusterLED(
#'   faDF = countData,
#'   minReads = 5,
#'   maxLED = 10,
#'   totalClusters = 100,
#'   keepNC = FALSE
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_clusterLED <- function(faDF = NULL, minReads = 10, maxLED = 7, totalClusters = 30, keepNC = TRUE){

  # filter by read count, initialize cluster-specific columns
  clusterData <- faDF %>%
    dplyr::arrange(dplyr::desc(.data$RPU)) %>%
    dplyr::filter(.data$Reads >= minReads) %>%
    dplyr::mutate(Cluster = NA, RankInCluster = NA, LED = NA) %>%
    dplyr::select(-.data$ID)

  # sample sequences; assume they are sorted by number of reads
  Sequences <- as.list(clusterData$Sequences)

  # create cluster iterator
  clusterNumber <- 1

  # only create a user-specified number of clusters and while you still have sequences
  while(clusterNumber <= totalClusters & length(Sequences) != 0){

    # get start time
    startTime <- Sys.time()

    # reset iterator (counter)
    counter <- 1

    # first element of seq. list (most abundant seq.) is seed; remove from seq. list
    clusterList <- Sequences[1]
    Sequences <- Sequences[-1]

    # make list for string distances
    seqLED <- list(0)

    # only iterate while you still have sequences and while you are not at the end of the list
    while(length(Sequences) != 0 & counter < length(Sequences)){

      # check LED between seed and current sequence
      if(as.numeric(utils::adist(clusterList[[1]], Sequences[[counter]])) <= maxLED){

        # add LED to seqLED list
        seqLED <- c(seqLED, as.numeric(utils::adist(clusterList[[1]], Sequences[[counter]])))

        # if LED is < maxLED, add sequence to cluster list; set sequence from original list to NA
        clusterList <- c(clusterList, Sequences[[counter]])
        Sequences[[counter]] <- NA
      }

      # check next sequence in list
      counter <- counter + 1
    }

    # remove NAs from sequence list; these seq.s are now clustered
    Sequences <- Sequences[!is.na(Sequences)]

    # turn clustered sequences in data frame and merge back into clusterData
    seqDF <- data.frame(
      Sequences = unlist(clusterList),
      Cluster = clusterNumber,
      RankInCluster = 1:length(clusterList),
      LED = unlist(seqLED),
      stringsAsFactors = FALSE
    )
    clusterData[which(clusterData$Sequences %in% seqDF$Sequences),c("Cluster", "RankInCluster", "LED")] <- seqDF %>%
      dplyr::select(-.data$Sequences)

    # message with number of unique sequences in cluster
    message(
      paste(
        paste0("Finished cluster ", clusterNumber, ": ", nrow(seqDF), " unique sequences"),
        paste0(gsub("Time difference of", "Elapsed time:", utils::capture.output(round(Sys.time() - startTime, 2)))),
        sep = "<br/>"
      )
    )

    # bump the cluster number
    clusterNumber <- clusterNumber + 1
  }

  # keep or omit non-clustered sequences
  if(keepNC){

    # replace NAs with 0
    clusterData[is.na(clusterData)] <- 0

    # separate cluster 0
    cluster0 <- clusterData %>% dplyr::filter(.data$Cluster == 0)
    clusterData <- clusterData %>% dplyr::filter(.data$Cluster != 0)

    # add rank in cluster (determined by RPU)
    cluster0 <- cluster0 %>%
      dplyr::arrange(dplyr::desc(.data$RPU)) %>%
      dplyr::mutate(RankInCluster = rank(rev(.data$RankInCluster), ties.method = "first"))

    # compute LED between the cluster seed and every other sequence
    cluster0$LED <- utils::adist(cluster0$Sequences[1], cluster0$Sequences) %>% as.vector()

    # add cluster 0 back to main data
    clusterData <- rbind(clusterData, cluster0)

  } else{
    clusterData <- stats::na.omit(clusterData)
  }

  # add cluster information to sequence IDs
  clusterData <- clusterData %>%
    dplyr::mutate(ID = glue::glue("{Rank}-{Reads}-{RPU}-{Cluster}-{RankInCluster}-{LED}")) %>%
    dplyr::select(.data$ID, dplyr::everything())

  return(clusterData)
}
