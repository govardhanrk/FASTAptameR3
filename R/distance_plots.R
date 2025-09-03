#' Return distance histograms
#'
#' `fa_distance_histogram` returns two histograms (1. for unique sequences and 2. for all reads) to visualize the Levenshtein edit distance (LED) between a query sequence and all other sequences in a population
#'
#' @param distanceData A data.frame from `fa_distance`
#' @param xaxis The title of the x-axis (default: "Distance from query")
#' @param yaxes The titles of the y-axis (defaults: "Unique sequences", "Read count")
#' @param plot_title The title of the plot (default: "Distance histograms")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param bar_fill The color of the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_distance()]
#'
#' @return A plotly object with a histogram of distances between all sequences and a query sequence
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, compute LED between counted sequences and query sequence
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' distanceData <- fa_distance(faDF = countData, querySequence = query)
#'
#' # make histogram
#' fa_distance_histogram(distanceData = distanceData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_distance_histogram <- function(
    distanceData = NULL,
    xaxis = "Distance from query",
    yaxes = c("Unique sequences", "Read count"),
    plot_title = "Distance histograms",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # distance histogram with unique sequences
  p1 <- ggplot2::ggplot(distanceData, ggplot2::aes(.data$Distance)) +
    ggplot2::geom_bar(fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxes[1])
    ggplot2::theme_classic()

  # make bold, interactive plot
  p1 <- boldPlots(p = p1) %>%
    plotly::ggplotly()

  # distance histogram with total reads
  p2 <- distanceData %>%
    dplyr::group_by(.data$Distance) %>%
    dplyr::summarise(TotalReads = sum(.data$Reads)) %>%

    ggplot2::ggplot(ggplot2::aes(x = .data$Distance, y = .data$TotalReads)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxes[2]) +
    ggplot2::theme_classic()

  # make bold, interactive plot
  p2 <- boldPlots(p = p2) %>%
    plotly::ggplotly()

  # make interactive figure
  fig <- plotly::subplot(p1, p2, titleY = TRUE, titleX = TRUE, nrows = 2, margin = 0.1, shareX = TRUE) %>%
    plotly::layout(title = paste0("<b>", plot_title, "</b>"), showlegend = FALSE, margin = list(t = 50)) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "distance_histograms"))

  # return interactive histogram
  return(fig)
}
