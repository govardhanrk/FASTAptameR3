#' Return a histogram of fold changes
#'
#' `fa_enrich_histogram` returns a histogram of sequence enrichment scores after a log2 transformation.
#'
#' @param enrichDF An enrichment data.frame from `fa_enrich`
#' @param xaxis The title of the x-axis (default: "log2(Enrichment)")
#' @param yaxis The title of the y-axis (default: "Unique sequence count")
#' @param plot_title The title of the plot (default: "log2E histogram")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param bar_fill The color of the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_enrich()], [fa_enrich_scatter()], [fa_enrich_ra()]
#'
#' @return A plotly object with a histogram of enrichment scores
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, compute enrichment
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' enrichData <- fa_enrich(faDF1 = countData[[1]], faDF2 = countData[[2]])
#'
#' # make histogram
#' fa_enrich_histogram(enrichDF = enrichData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_enrich_histogram <- function(
    enrichDF = NULL,
    xaxis = "log2(Enrichment)",
    yaxis = "Unique sequence count",
    plot_title = "log2E histogram",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # make plot
  p <- ggplot2::ggplot(enrichDF, ggplot2::aes(.data$log2E)) +
    ggplot2::geom_histogram(color = bar_outline, fill = bar_fill, bins = 30) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "enrich_histogram"))

  # return interactive figure
  return(fig)
}

#' Return a scatter plot of RPUs in two populations
#'
#' `fa_enrich_scatter` returns a scatter plot in which points correspond to sequences and coordinates correspond to RPU in the 1st and 2nd populations (RPU.a and RPU.b, respectively).
#'
#' @param enrichDF An enrichment data.frame from `fa_enrich`
#' @param xaxis The title of the x-axis (default: "RPU.a")
#' @param yaxis The title of the y-axis (default: "RPU.b")
#' @param plot_title The title of the plot (default: "RPU.a vs RPU.b")
#' @param point_colour The color of the points (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_enrich()], [fa_enrich_histogram()], [fa_enrich_ra()]
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, compute enrichment
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' enrichData <- fa_enrich(faDF1 = countData[[1]], faDF2 = countData[[2]])
#'
#' # make histogram
#' fa_enrich_scatter(enrichDF = enrichData)
#' }
#'
#' @return A plotly object with a scatter plot RPU in the first population vs RPU in the second population
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_enrich_scatter <- function(
    enrichDF = NULL,
    xaxis = "RPU.a",
    yaxis = "RPU.b",
    plot_title = "RPU.a vs RPU.b",
    point_colour = "skyblue"
){

  # add epsilon factor to all RPUs
  enrichDF <- enrichDF %>% dplyr::mutate(RPU.a = .data$RPU.a + 0.001, RPU.b = .data$RPU.b + 0.001)

  # make plot
  p <- ggplot2::ggplot(enrichDF, ggplot2::aes(x = .data$RPU.a, y = .data$RPU.b, text = .data$Sequences)) +
    ggplot2::geom_point(colour = point_colour, alpha = 0.5) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "enrich_RPU"))

  # return interactive figure
  return(fig)
}

#' Return an RA plot from RPUs in two populations
#'
#' `fa_enrich_ra` generates a ratio average (RA) plot from RPU values in the 1st and 2nd populations (RPU.a and RPU.b, respectively).
#' A small epsilon factor (epsilon = 0.001) is added to the RPU values in both populations to avoid dividing by 0s or taking the log2 of them.
#' R of the RA plot is defined as log2(RPU.b / RPU.a), which corresponds to the fold change in RPU.
#' A of the RA plot is defined as log2(RPU.b * RPU.a) / 2, which corresponds to the average log2-transformed RPU.
#'
#' @param enrichDF An enrichment data.frame from `fa_enrich`
#' @param xaxis The title of the x-axis (default: "Average log2(RPU)")
#' @param yaxis The title of the y-axis (default: "Fold change")
#' @param plot_title The title of the plot (default: "Enrichment RA plot")
#' @param point_colour The color of the points (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_enrich()], [fa_enrich_histogram()], [fa_enrich_scatter()]
#'
#' @return A plotly object with an RA scatter plot
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, compute enrichment
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' enrichData <- fa_enrich(faDF1 = countData[[1]], faDF2 = countData[[2]])
#'
#' # make histogram
#' fa_enrich_ra(enrichDF = enrichData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_enrich_ra <- function(
    enrichDF = NULL,
    xaxis = "Average log2(RPU)",
    yaxis = "Fold change",
    plot_title = "Enrichment RA plot",
    point_colour = "skyblue"
){

  # add epsilon factor if RPM is equal to 0
  enrichDF <- enrichDF %>% dplyr::mutate(RPU.a = .data$RPU.a + 0.001, RPU.b = .data$RPU.b + 0.001)

  # add fold change and average log RPM
  enrichDF <- enrichDF %>%
    dplyr::mutate(
      R = log2(.data$RPU.b / .data$RPU.a),
      A = 0.5 * log2(.data$RPU.b * .data$RPU.a)
    )

  # make plot
  p <- ggplot2::ggplot(enrichDF, ggplot2::aes(x = .data$A, y = .data$R, text = .data$Sequences)) +
    ggplot2::geom_point(colour = point_colour, alpha = 0.5) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "enrich_RA"))

  # return interactive figure
  return(fig)
}
