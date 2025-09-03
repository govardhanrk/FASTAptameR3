#' Return a scatter plot with the results of the motif discovery module
#'
#' `fa_fsbc_motifDiscoveryPlot` returns a scatter plot that displays the results of the motif discovery module (`fa_fsbc_motifDiscovery` and `fa_motifDiscovery`), which is built with the FSBC tool (DOI: 10.1186/s12859-020-03607-1).
#' The x-axis, y-axis, and color respectively correspond to the motif's 1) rank after sorting by normalized z-scores, 2) -log10(p-value), and 3) length.
#'
#' @param motif_discovery_df A data.frame from `fa_motifDiscovery`
#' @param xaxis The title of the x-axis (default: "Rank by normalized z-score")
#' @param yaxis The title of the y-axis (default: "-log10(p)")
#' @param legend_title The title of the legend (default: "Length")
#' @param plot_title The title of the plot (default: "Over-enriched strings")
#' @param colour_palette_cont A continuous color palette (default: "magma")
#'
#' @seealso [ggplot2::scale_colour_viridis_c()], [fa_fsbc_motifDiscovery()]
#'
#' @return A plotly object with a bubble plot showing Rank vs p-value for detected motifs
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ file, find overrepresented motifs with the FSBC tools
#' countData <- fa_count(dataInput = "PATH/TO/FILE.fastq")
#' motifData <- fa_fsbc_rootMotifs(motif_discovery_df = fa_fsbc_motifDiscovery(faDF = countData))
#'
#' # make scatter plot
#' fa_fsbc_motifDiscoveryPlot(motif_discover_df = motifData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_fsbc_motifDiscoveryPlot <- function(
    motif_discovery_df = NULL,
    xaxis = "Rank by normalized z-score",
    yaxis = "-log10(p)",
    legend_title = "Length",
    plot_title = "Over-enriched strings",
    colour_palette_cont = "magma"
){

  # generate bubble plot where x-axis is the "Rank by Normalized Z-Score", and y-axis is the -log10(p-value)
  p <- ggplot2::ggplot(motif_discovery_df) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$Rank, y = -log10(.data$P),
        colour = .data$Motif_Length, size = .data$Motif_Length,
        text = glue::glue("Motif: {.data$Motif}, Rank: {.data$Rank}, -log10(p-value): {-log10(.data$P)}, Motif length: {.data$Motif_Length}")
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$Rank, y = -log10(.data$P), size = .data$Motif_Length),
      shape = 21, colour = "black", stroke = 0.15
    ) +
    ggplot2::scale_colour_viridis_c(option = colour_palette_cont) +
    ggplot2::scale_size(guide = "none") +
    ggplot2::labs(x = xaxis, y = yaxis, colour = legend_title, title = plot_title) +
    ggplot2::theme_classic()

  # make plot bold
  p <- boldPlots(p)

  # make figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "motif_discovery"))

  # return figure
  return(fig)
}
