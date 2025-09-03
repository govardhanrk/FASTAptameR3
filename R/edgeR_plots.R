#' Return a scatter plot with the results of a paired DEA test with edgeR
#'
#' `fa_edgeR_pairTestViz` returns a scatter plot of logCPM vs logFC.
#' Point color and shape respectively correspond to whether the sequence is considered significant based on corrected p-values from edgeR and a user-defined threshold in `fa_edgeR()`.
#'
#' @param edgeR_DF A data.frame from `fa_edgeR`
#' @param xaxis The x-axis title (default: "logCPM")
#' @param yaxis The y-axis title (default: "logFC")
#' @param plot_title The plot title (default: "edgeR results")
#' @param point_colours A vector of color names for significant (default: "red") and insignificant (default: "black") points
#'
#' @seealso [grDevices::colors()], [fa_edgeR_pairTest()]
#'
#' @return A plotly object with a scatter plot of logCPM vs logFC from the edgeR table
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files in condition 1
#' countData_cond1 <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE3.fastq")
#' )
#'
#' # count FASTQ files in condition 2
#' countData_cond2 <- list(
#'   fa_count(dataInput = "PATH/TO/FILE4.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE5.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE6.fastq")
#' )
#'
#' # use edgeR
#' edgeRData <- fa_edgeR_pairTest(
#'   faDF_cond1 = countData_cond1,
#'   faDF_cond2 = countData_cond2,
#'   pcutoff = 0.05
#' )
#'
#' # make volcano plot
#' fa_edgeR_pairTestViz(edgeR_DF = edgeRData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_edgeR_pairTestViz <- function(
    edgeR_DF = NULL,
    xaxis = "logCPM",
    yaxis = "logFC",
    plot_title = "edgeR results",
    point_colours = c("red", "black")
){

  # plot results
  p <- ggplot2::ggplot(
    edgeR_DF,
    ggplot2::aes(
      x = .data$logCPM, y = .data$logFC,
      colour = .data$PClass, shape = .data$PClass,
      text = glue::glue("logCPM: {.data$logCPM}, logFC: {.data$logFC}, p-value: {.data$PValue}")
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Sig." = point_colours[1], "Not Sig." = point_colours[2])) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::hide_legend() %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "edgeR"))

  # return interactive figure of paired edgeR DEA results
  return(fig)
}
