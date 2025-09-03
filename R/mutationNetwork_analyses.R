#' Find the shortest path between two sequences if a path exists under the given constraints
#'
#' `fa_mutationNetwork` uses Dijkstra's shortest path algorithm and LEDs between sequences to find the shortest mutational path between two input sequences (if it exists).
#'
#' @param faDF A FASTAptameR data.frame
#' @param startNode The start node is the first sequence in the path of the mutation network
#' @param endNode The end node is the final sequence in the path of the mutation network
#' @param maxCost This is the maximum Levenshtein edit distance (LED) allowed between any two connected sequences (default: 1)
#'
#' @seealso [fa_count()]
#'
#' @return A data.frame with the each step of the mutation network if a path exists between the start and end nodes
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # find shortest path between most abundant sequence and a user-defined query sequence
#' pathData <- fa_mutationNetwork(
#'   faDF = countData,
#'   startNode = countData$Sequences[1],
#'   endNode = querySequence,
#'   maxCost = 3
#' )
#' }
#'
#' @importFrom dplyr %>%
fa_mutationNetwork <- function(faDF = NULL, startNode = NULL, endNode = NULL, maxCost = 1){

  # pull sequences from FASTA file, add start / end nodes
  seqList <- c(faDF$Sequences, startNode, endNode) %>% unique()

  # initialize data.frame of nodes and connections
  seqConnections <- data.frame(from_vertex = NA, to_vertex = NA, cost = NA)
  rowCount <- 1

  # message
  message("Starting distance calculations.")

  # add connections if their edit distances are within the user-defined threshold
  for(i in 1:(length(seqList) - 1)){

    j <- i + 1

    while(j <= length(seqList)){

      editDistance <- as.vector(utils::adist(seqList[i], seqList[j]))

      if(editDistance <= maxCost){

        seqConnections[rowCount,] <- c(seqList[i], seqList[j], editDistance)
        rowCount <- rowCount + 1
      }

      j <- j + 1
    }
  }

  # message
  message("Starting path calculations.")

  # make graph and only keep connections when
  seqGraph <- cppRouting::makegraph(seqConnections)

  # get path from starting node to ending node, allowing only one hop per
  mutationPath <- tryCatch(
    expr = cppRouting::get_path_pair(seqGraph, from = startNode, to = endNode)[[1]],
    error = function(cond) return(c(startNode, endNode))
  )

  # check if path was not found
  if(length(mutationPath) == 1){

    return(mutationPath)
  }

  # initialize data.frame for results
  mutationNetwork <- data.frame(From_Sequence = rep(NA, length(mutationPath) - 1), To_Sequence = rep(NA, length(mutationPath) - 1))

  # format results into data.frame
  for(i in 2:length(mutationPath)){

    mutationNetwork$From_Sequence[i-1] <- mutationPath[i-1]
    mutationNetwork$To_Sequence[i-1] <- mutationPath[i]
  }

  # add LED between sequence pairs
  mutationNetwork$Transition_Cost <- sapply(
    1:nrow(mutationNetwork),
    function(i) utils::adist(mutationNetwork$From_Sequence[i], mutationNetwork$To_Sequence[i]) %>% as.vector()
  )

  # return the network
  return(mutationNetwork)
}
