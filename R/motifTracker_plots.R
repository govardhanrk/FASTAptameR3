#' Return a line plot of motif abundance across populations
#'
#' `fa_motif_motifTrackerPlot` returns a line plot that shows the percentage of sequences with user-defined motifs across multiple populations.
#' This current formulation assumes that populations have some form of temporal dependence, such as consecutive rounds in a SELEX experiment.
#'
#' @param trackerDF A data.frame from `fa_motif_motifTracker`
#' @param xaxis The title of the x-axis (default: "Population")
#' @param yaxis The title of the y-axis (default: "Percentage")
#' @param plot_title The title of the plot (default: "Motif tracker")
#' @param colour_palette_disc A discrete RColorBrewer palette (default: "Dark2")
#'
#' @seealso [RColorBrewer::brewer.pal.info], [fa_motif_motifTracker()]
#'
#' @return A plotly object with a line plot showing motif occurrence in each population
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, count motifs across files
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' trackerData <- fa_motif_motifTracker(
#'   faDF_list = countData,
#'   populationNames = c("Population A", "Population B"),
#'   queryList = c("AAA", "AACG"),
#'   queryAliases = c("motif1", "motif2"),
#'   motifType = "Nucleotide"
#' )
#'
#' # make line plot
#' fa_motif_motifTrackerPlot(trackerDF = trackerData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motif_motifTrackerPlot <- function(
    trackerDF = NULL,
    xaxis = "Population",
    yaxis = "Percentage",
    plot_title = "Motif tracker",
    colour_palette_disc = "Dark2"
){

  # make base plot
  if("Alias" %in% colnames(trackerDF)){
    p <- ggplot2::ggplot(trackerDF, ggplot2::aes(
      x = .data$PopulationName, y = .data$Percentage,
      colour = .data$Alias,
      text = glue::glue(
        "
      Population number: {.data$PopulationNum}
      Population name: {.data$PopulationName}
      Alias: {.data$Alias}
      Motif: {.data$Motif}
      Total reads: {.data$TotalReads}
      Percentage: {.data$Percentage}
      "
      )
    ))
  } else{
    p <- ggplot2::ggplot(trackerDF, ggplot2::aes(
      x = .data$PopulationName, y = .data$Percentage,
      colour = .data$Motif,
      text = glue::glue(
        "
      Population number: {.data$PopulationNum}
      Population name: {.data$PopulationName}
      Motif: {.data$Motif}
      Total reads: {.data$TotalReads}
      Percentage: {.data$Percentage}
      "
      )
    ))
  }

  # create line plot
  p <- p +
    ggplot2::geom_line(group = 1, size = 1) +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_x_continuous(breaks = unique(trackerDF$Population)) +
    ggplot2::scale_colour_brewer(palette = colour_palette_disc) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_classic()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(legend = list(orientation = 'h', y = -0.2)) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "motif_tracker"))

  # return interactive figure
  return(fig)
}

#' Return a line plot of sequence abundance across populations
#'
#' `fa_motif_sequenceTrackerPlot` returns a line plot showing the RPU of user-defined sequences across multiple populations.
#' The current formulation assumes that populations have some form of temporal dependence, such as consecutive rounds in a SELEX experiment.
#'
#' @param trackerDF A data.frame from `fa_motif_sequenceTracker`
#' @param xaxis The x-axis title (default: "Population")
#' @param yaxis The y-axis title (default: "RPU")
#' @param plot_title The plot title (default: "Sequence tracker")
#' @param colour_palette_disc A discrete RColorBrewer palette (default: "Dark2")
#'
#' @seealso [RColorBrewer::brewer.pal.info], [fa_motif_sequenceTracker()]
#'
#' @return A plotly object with a line plot showing sequence occurrence in each population
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, count sequence occurrences across files
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' trackerData <- fa_motif_sequenceTracker(
#'   faDF_list = countData,
#'   populationNames = c("Population A", "Population B"),
#'   queryList = c(sequence1, sequence2),
#'   queryAliases = c("seq1", "seq2")
#' )
#'
#' # make line plot
#' fa_motif_sequenceTrackerPlot(trackerDF = trackerData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_motif_sequenceTrackerPlot <- function(
    trackerDF = NULL,
    xaxis = "Population",
    yaxis = "RPU",
    plot_title = "Sequence tracker",
    colour_palette_disc = "Dark2"
){

  # make base plot
  if("Alias" %in% colnames(trackerDF)){
    p <- ggplot2::ggplot(trackerDF, ggplot2::aes(
      x = .data$PopulationName, y = .data$RPU,
      colour = .data$Alias,
      text = glue::glue(
        "
      Population number: {.data$PopulationNum}
      Population name: {.data$PopulationName}
      Alias: {.data$Alias}
      Sequence: {.data$Sequences}
      Rank: {.data$Rank}
      Reads: {.data$Reads}
      RPU: {.data$RPU}
      "
      )
    ))
  } else{
    p <- ggplot2::ggplot(trackerDF, ggplot2::aes(
      x = .data$PopulationName, y = .data$RPU,
      colour = .data$Sequences,
      text = glue::glue(
        "
      Population number: {.data$PopulationNum}
      Population name: {.data$PopulationName}
      Sequence: {.data$Sequences}
      Rank: {.data$Rank}
      Reads: {.data$Reads}
      RPU: {.data$RPU}
      "
      )
    ))
  }

  # make line plot
  p <- p +
    ggplot2::geom_line(group = 1, size = 1) +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_x_continuous(breaks = unique(trackerDF$Population)) +
    ggplot2::scale_colour_brewer(palette = colour_palette_disc) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_classic()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(legend = list(orientation = 'h', y = -0.2)) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "sequence_tracker"))

  # return interactive figure
  return(fig)
}
