#' Return a bar plot with the average enrichment per position, regardless of the character at that position
#'
#' `fa_posEnrich_barplot` returns a bar plot with one bar per position in the aligned sequences. The height of each bar corresponds to the average enrichment of sequences.
#' A much more informative plot is generated with `fa_posEnrich_heatmap`, so we recommend using that version instead.
#'
#' @param faDF_posEnrich A data.frame from `fa_posEnrich`
#' @param xaxis The title of the x-axis (default: "Position")
#' @param yaxis The title of the y-axis (default: "Avg. enrichment")
#' @param plot_title The title of the plot (default: "Enrichment per position")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param bar_fill The color of the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_posEnrich()], [fa_posEnrich_heatmap()]
#'
#' @return A plotly object with a bar plot showing the average enrichment per position after aggregating the residue-specific enrichment values
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together, compute position enrichment
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
#' posEnrichData <- fa_posEnrich(faDF_recluster = reclusterData, cluster_selection = 1)
#'
#' # generate bar plot
#' fa_posEnrich_barplot(faDF_posEnrich = posEnrichData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_posEnrich_barplot <- function(
    faDF_posEnrich = NULL,
    xaxis = "Position",
    yaxis = "Avg. enrichment",
    plot_title = "Enrichment per position",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # format for plotting
  barplot_df <- faDF_posEnrich %>%
    dplyr::group_by(.data$Position) %>%
    dplyr::summarise(AvAvEnrich = mean(.data$AvEnrich, na.rm = TRUE))

  # make plot
  p <- ggplot2::ggplot(
    barplot_df,
    ggplot2::aes(x = .data$Position, y = .data$AvAvEnrich, text = glue::glue("Position: {.data$Position}, Avg. enrichment at position: {.data$AvAvEnrich}"))
  ) +
    ggplot2::geom_bar(stat = "identity", fill = bar_fill, colour = bar_outline) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "position_enrich_barplot"))

  # return interactive figure
  return(fig)
}

#' Return a heat map showing the average enrichment per position, with regard to the character at that position
#'
#' `fa_posEnrich_heatmap` returns a heat map in which the x-axis corresponds to each position of an MSA,
#' the y-axis corresponds to possible characters (e.g., D/RNA populations will only have rows corresponding to the four nucleotides or a gap),
#' and the color corresponds to the enrichment value of sequences with the indicated character at the indicated position.
#'
#' @param faDF_posEnrich A data.frame from `fa_posEnrich`
#' @param xaxis The title of the x-axis (default: "Position")
#' @param yaxis The title of the y-axis (default: "Residue")
#' @param legend_title The title of the legend (default: "Avg. enrichment")
#' @param plot_title The title of the plot (default: "Enrichment per position per residue")
#' @param fill_palette_cont A continuous color palette (default: "magma")
#'
#' @seealso [ggplot2::scale_fill_viridis_c()], [fa_posEnrich()], [fa_posEnrich_barplot()]
#'
#' @return A plotly object with a heat map showing the average enrichment per position per residue at that position
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together, compute position enrichment
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
#' posEnrichData <- fa_posEnrich(faDF_recluster = reclusterData, cluster_selection = 1)
#'
#' # make heat map
#' fa_posEnrich_heatmap(faDF_posEnrich = posEnrichData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_posEnrich_heatmap <- function(
    faDF_posEnrich = NULL,
    xaxis = "Position",
    yaxis = "Residue",
    legend_title = "Avg. enrichment",
    plot_title = "Enrichment per position per residue",
    fill_palette_cont = "magma"
){

  # make plot
  p <- ggplot2::ggplot(
    faDF_posEnrich,
    ggplot2::aes(
      x = .data$Position,
      y = .data$Residue,
      fill = .data$AvEnrich,
      text = glue::glue("Position: {.data$Position}, Residue: {.data$Residue}, Avg. enrichment of residue at position: {.data$AvEnrich}")
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = fill_palette_cont) +
    ggplot2::labs(x = xaxis, y = yaxis, fill = legend_title, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "position_enrich_heatmap"))

  # return interactive figure
  return(fig)
}
