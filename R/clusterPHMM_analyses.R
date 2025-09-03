#' Derive a PHMM from the MSA of a single cluster
#'
#' `fa_phmm_create` derives a profile hidden Markov model (PHMM) for a single aligned cluster.
#'
#' @param faDF_msa A data.frame with aligned sequences from fa_clusterMSA
#'
#' @seealso [fa_clusterLED()], [fa_clusterMSA()], [fa_phmm_simulate()], [aphid::derivePHMM()]
#'
#' @return An object of the "PHMM" class from the `aphid` package
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, cluster data, perform MSA, derive PHMM
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' clusterData <- fa_clusterLED(faDF = countData)
#' msaData <- fa_clusterMSA(faDF_cluster = clusterData, clusterSelection = 10, seq_type = "dna")
#' phmmObj <- fa_phmm_create(faDF_msa = msaData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_phmm_create <- function(faDF_msa = NULL){

  # pull sequences, convert to DNAbin object
  msa_seqs <- faDF_msa %>%
    dplyr::pull(.data$Sequences) %>%
    strsplit(split = "") %>%
    sapply(tolower) %>%
    t()

  # learn PHMM
  phmm <- aphid::derivePHMM(msa_seqs, pseudocounts = "Laplace")

  # return PHMM
  return(phmm)
}

#' Simulate sequences according to a derived PHMM
#'
#' `fa_phmm_simulate` simulates sequences from a PHMM derived from `fa_phmm_create`.
#'
#' @param msaPHMM An object of the "PHMM" class; generated with fa_phmm_create
#' @param numSeq The number of sequences to simulate (default: 10)
#' @param seqLength The target length of simulated sequences (default: 70)
#'
#' @seealso [fa_clusterLED()], [fa_clusterMSA()], [fa_phmm_create()], [aphid::generate()]
#'
#' @return A vector of simulated sequences
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, cluster data, perform MSA, derive PHMM, generate sequences with PHMM
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' clusterData <- fa_clusterLED(faDF = countData)
#' msaData <- fa_clusterMSA(faDF_cluster = clusterData, clusterSelection = 10, seq_type = "dna")
#' phmmObj <- fa_phmm_create(faDF_msa = msaData)
#' sim_seqs <- fa_phmm_simulate(msaPHMM = phmmObj, numSeq = 20, seqLength = 56)
#' }
#'
#' @importFrom dplyr %>%
fa_phmm_simulate <- function(msaPHMM = NULL, numSeq = 10, seqLength = 70){

  # simulate sequences but only keep the unique ones
  simseqs <- sapply(1:numSeq, function(x) paste0(aphid::generate(msaPHMM, size = seqLength), collapse = "")) %>%
    unique() %>%
    toupper()

  # return simulated sequences
  return(simseqs)
}
