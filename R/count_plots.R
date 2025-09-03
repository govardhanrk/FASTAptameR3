#' Return a reads-per-rank line plot
#'
#' `fa_count_rpr` returns a line plot showing the relationship between sequence rank and read count.
#'
#' @param countData A data.frame from `fa_count`
#' @param minReads The minimum number of reads required to include a sequence in the plot (default: 10)
#' @param maxRanks The maximum number of sequences to include (default: 1000)
#' @param xaxis The title of the x-axis (default: "Ranks of unique sequences")
#' @param yaxis The title of the y-axis (default: "Total reads per unique sequence")
#' @param plot_title The title of the full plot (default: "Read count for each rank")
#' @param line_color The color of the line (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_count()], [fa_count_histogram()], [fa_count_binnedAbundance()]
#'
#' @return A plotly object with line plot showing the relationship between sequence rank and number of reads
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # make line plot
#' fa_count_rpr(countData = countData, minReads = 5, maxRanks = 1000)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_count_rpr <- function(
    countData = NULL,
    minReads = 10,
    maxRanks = 1000,
    xaxis = "Ranks of unique sequences",
    yaxis = "Total reads per unique sequence",
    plot_title = "Read count for each rank",
    line_color = "skyblue"
){

  # filter by minReads and maxRanks
  seqCounts <- countData %>% dplyr::filter(.data$Reads >= minReads & .data$SequenceRank <= maxRanks)

  # make plot
  p <- ggplot2::ggplot(seqCounts, ggplot2::aes(x = .data$SequenceRank, y = .data$Reads, group = 1)) +
    ggplot2::geom_line(colour = line_color, size = 2) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_classic()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "reads_per_rank"))

  # return figure
  return(fig)
}

#' Return histograms of sequence lengths
#'
#' `fa_count_histogram` returns two sequence-length histograms: 1) for unique sequences and 2) for all reads.
#'
#' @param countData A data.frame from `fa_count`
#' @param xaxis The title of the x-axis (default: "Sequence length")
#' @param yaxes The titles of the y-axis (defaults: "Unique sequences", "Read count")
#' @param plot_title The title of the plot (default: "Sequence-length histogram")
#' @param bar_outline The color for the bar outlines (default: "black")
#' @param bar_fill The color for the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_count()], [fa_count_rpr()], [fa_count_binnedAbundance()]
#'
#' @return A plotly object with two histograms for the sequence lengths for 1) unique reads and 2) all reads
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # make histogram
#' fa_count_histogram(countData = countData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_count_histogram <- function(
    countData = NULL,
    xaxis = "Sequence length",
    yaxes = c("Unique sequences", "Read count"),
    plot_title = "Sequence-length histogram",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # make histogram of sequence lengths
  p1 <- ggplot2::ggplot(countData, ggplot2::aes(.data$Length)) +
    ggplot2::geom_bar(fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxes[1]) +
    ggplot2::theme_classic()

  p2 <- countData %>%
    dplyr::group_by(.data$Length) %>%
    dplyr::summarise(TotalReads = sum(.data$Reads)) %>%

    ggplot2::ggplot(ggplot2::aes(x = .data$Length, y = .data$TotalReads)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxes[2]) +
    ggplot2::theme_classic()

  # make bold, interactive figures
  fig1 <- boldPlots(p = p1) %>% plotly::ggplotly()
  fig2 <- boldPlots(p = p2) %>% plotly::ggplotly()

  # make interactive figure
  fig <- plotly::subplot(fig1, fig2, titleY = TRUE, titleX = TRUE, nrows = 2, margin = 0.1) %>%
    plotly::layout(title = paste0("<b>", plot_title, "</b>"), showlegend = FALSE, margin = list(t = 50)) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "sequence_lengths"))

  # return interactive plot
  return(fig)
}

#' Return a binned abundance plot
#'
#' `fa_count_binnedAbundance` returns a bar plot in which bars correspond to a range of read counts (e.g., 10-100 reads for a sequence).
#' The height of the bar corresponds to its fraction in the population, and the color corresponds to the number of unique reads for the bin.
#'
#' @param countData A data.frame from `fa_count`
#' @param useSingleton A logical indicating if singletons (i.e., sequences with only one read) should be in their own bin (default: TRUE)
#' @param breaks A numeric vector of breakpoints used to create discrete bins for read counts (default: 10, 100, 1000)
#' @param xaxis The title of the x-axis (default: "Unique sequences")
#' @param yaxis The title of the y-axis (default: "Fraction of population")
#' @param plot_title The title of the plot (default: "Binned sequence abundance")
#' @param bar_outline The color for the bar outlines (default: "black")
#' @param bar_fill The color for the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_count()], [fa_count_rpr()], [fa_count_histogram()]
#'
#' @return A plotly object showing the number of sequences in each bin, where bins are defined by read counts
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#'
#' # make bar plot
#' fa_count_binnedAbundance(countData = countData, useSingleton = TRUE, breaks = c(20, 200, 2000))
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_count_binnedAbundance <- function(
    countData = NULL,
    useSingleton = TRUE,
    breaks = c(10,100,1000),
    xaxis = "Unique sequences",
    yaxis = "Fraction of population",
    plot_title = "Binned sequence abundance",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # initialize breaks if not specified by user
  if(length(breaks) == 1){
    if(breaks == ""){
      breaks <- c(10,100,1000)
    }
  }

  # remove breaks outside of the range of the data
  breaks <- breaks[breaks > min(countData$Reads) & breaks < max(countData$Reads)]

  # add min and max read values to breaks vector, sort, filter for unique values
  breaks_mod <- c(0, breaks, max(countData$Reads)) %>% sort() %>% unique()

  # get vector of labels for cut
  labelNames <- vector()
  for(i in 2:length(breaks_mod)){

    if(i == 2 & useSingleton){
      labelNames[i-1] <- paste0("1 < Reads < ", breaks_mod[i])
    } else{
      labelNames[i-1] <- paste0(breaks_mod[i-1], " <= Reads < ", breaks_mod[i])
    }
  }

  # get vector for factor labels
  if(useSingleton){
    factorNames <- c("Singleton", labelNames)
  } else{
    factorNames <- labelNames
  }

  # label reads by break points
  countData_formatted <- countData %>%
    dplyr::mutate(BinnedReads = as.character(cut(.data$Reads, breaks = breaks_mod, labels = labelNames, right = TRUE))) %>%
    dplyr::mutate(
      BinnedReads = dplyr::case_when(
        useSingleton & .data$Reads == 1 ~ "Singleton",
        TRUE ~ .data$BinnedReads
      )
    ) %>%

    # group by bins, count number of unique sequences per bin, and count number of reads per bin
    dplyr::group_by(.data$BinnedReads) %>%
    dplyr::summarise(
      n = dplyr::n(),
      TotalReads = sum(.data$Reads)
    ) %>%

    # get fraction of reads in each bin; sort by fraction
    dplyr::mutate(Fraction = round(.data$TotalReads / sum(.data$TotalReads), 2)) %>%
    dplyr::arrange(dplyr::desc(.data$Fraction)) %>%

    # reorder bin levels
    dplyr::mutate(BinnedReads = factor(.data$BinnedReads, levels = factorNames))

  # make barplot
  p <- ggplot2::ggplot(countData_formatted, ggplot2::aes(.data$BinnedReads, .data$Fraction)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "binned_abundance"))

  # return interactive figure
  return(fig)
}
