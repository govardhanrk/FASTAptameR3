#' Return a heat map of the Levenshtein edit distances (LEDs) between cluster seeds
#'
#' `fa_recluster_heatmap` returns a heat map in which the x- and y-axes correspond to cluster seeds from their respective populations, and the color corresponds to the LED between seeds.
#' This visualization can indicate an appropriate LED for `fa_recluster()`.
#'
#' @param faDF1_cluster A clustered FASTAptameR data.frame
#' @param faDF2_cluster A clustered FASTAptameR data.frame
#' @param xaxis The title of the x-axis (default: "Population 1 clusters")
#' @param yaxis The title of the y-axis (default: "Population 2 clusters")
#' @param legend_title The title of the legend (default: "LED")
#' @param plot_title The title of the plot (default: "LED between cluster seeds")
#' @param fill_palette_cont A continuous color palette (default: "magma")
#'
#' @seealso [ggplot2::scale_fill_viridis_c()], [fa_clusterLED()], [fa_recluster()]
#'
#' @return A plotly object with a heat map showing the LED between cluster seeds in two populations
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data
#' countData <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq")
#' )
#' clusterData <- list(
#'   fa_clusterLED(faDF = countData[[1]]),
#'   fa_clusterLED(faDF = countData[[2]])
#' )
#'
#' # generate heatmap before reclustering
#' fa_recluster_heatmap(faDF1_cluster = clusterData[[1]], faDF2_cluster = clusterData[[2]])
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster_heatmap <- function(
    faDF1_cluster = NULL,
    faDF2_cluster = NULL,
    xaxis = "Population 1 clusters",
    yaxis = "Population 2 clusters",
    legend_title = "LED",
    plot_title = "LED between cluster seeds",
    fill_palette_cont = "magma"
){

  # get seeds for each population
  p1_seeds <- faDF1_cluster %>% dplyr::filter(.data$RankInCluster == 1) %>% dplyr::pull(.data$Sequences)
  p2_seeds <- faDF2_cluster %>% dplyr::filter(.data$RankInCluster == 1) %>% dplyr::pull(.data$Sequences)

  # compute LED between all cluster; rows = population 1, cols = population 2
  led_matrix <- utils::adist(x = p1_seeds, y = p2_seeds)

  # heat map of LEDs between cluster seeds
  p <- led_matrix %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var = "x") %>%
    tidyr::gather(key = "y", value = "z", -1) %>%
    dplyr::mutate(y = gsub("V", "", .data$y) %>% as.numeric()) %>%

    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$x, y = .data$y, fill = .data$z,
        text = glue::glue("Population 1 cluster seed: {.data$x}, Population 2 cluster seed: {.data$y}, LED: {.data$z}")
      )
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = fill_palette_cont) +
    ggplot2::labs(x = xaxis, y = yaxis, fill = legend_title, title = plot_title) +
    ggplot2::theme_bw()

  # make plot bold
  p <- boldPlots(p)

  # make figure interactive
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "recluster_heatmap"))

  # return figure
  return(fig)
}

#' Return a bar plot of population contributions to clusters
#'
#' `fa_recluster_popSize` returns a grouped bar plot that shows the number of sequences from each population that make up the new clusters.
#'
#' @param recluster_df A reclustered FASTAptameR data.frame from `fa_recluster`
#' @param xaxis The title of the x-axis (default: "Cluster")
#' @param yaxis The title of the y-axis (default: "Sequence count")
#' @param legend_title The title of the legend (default: "Population")
#' @param plot_title The title of the plot (default: "Sequence count per cluster")
#' @param bar_outline The color of the bar outline (default: "black")
#' @param fill_palette_disc A discrete RColorBrewer palette (default: "Dark2")
#'
#' @seealso [grDevices::colors()], [RColorBrewer::brewer.pal.info], [fa_recluster()], [fa_recluster_RPU()], [fa_recluster_LED()], [fa_recluster_enrich()]
#'
#' @return A plotly object with a grouped bar plot showing the number of unique sequences from each population for each new cluster
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
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
#'
#' # generate bar plot
#' fa_recluster_popSize(recluster_df = reclusterData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster_popSize <- function(
    recluster_df = NULL,
    xaxis = "Cluster",
    yaxis = "Sequence count",
    legend_title = "Population",
    plot_title = "Sequence count per cluster",
    bar_outline = "black",
    fill_palette_disc = "Dark2"
){

  # count number of sequences per cluster
  recluster_summary <- recluster_df %>%
    dplyr::group_by(.data$Cluster) %>%
    dplyr::summarise(
      A = sum(!is.na(.data$ID.a)),
      B = sum(!is.na(.data$ID.b))
    ) %>%
    tidyr::pivot_longer(!.data$Cluster, names_to = "Population", values_to = "NumSeqs")

  # make plot
  p <- ggplot2::ggplot(
    recluster_summary,
    ggplot2::aes(
      x = .data$Cluster,
      y = .data$NumSeqs,
      fill = .data$Population,
      text = glue::glue("Cluster: {.data$Cluster}, Sequence count: {.data$NumSeqs}, Population: {.data$Population}")
    )
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", colour = bar_outline) +
    ggplot2::scale_fill_brewer(palette = fill_palette_disc) +
    ggplot2::labs(x = xaxis, y = yaxis, fill = legend_title, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "recluster_count"))

  # return figure
  return(fig)
}

#' Return a bar plot of the average RPU per population per cluster
#'
#' `fa_recluster_RPU` returns a grouped bar plot that shows the average RPU of sequences from each population in each cluster.
#'
#' @param recluster_df A reclustered FASTAptameR data.frame from `fa_recluster`
#' @param xaxis The title of the x-axis (default: "Cluster")
#' @param yaxis The title of the y-axis (default: "Avg. RPU")
#' @param legend_title The title of the legend (default: "Population")
#' @param plot_title The title of the plot (default: "Avg. RPU per cluster")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param fill_palette_disc A discrete RColorBrewer palette (default: "Dark2")
#'
#' @seealso [grDevices::colors()], [RColorBrewer::brewer.pal.info], [fa_recluster()], [fa_recluster_popSize()], [fa_recluster_LED()], [fa_recluster_enrich()]
#'
#' @return A plotly object with a grouped bar plot showing the average RPU per population for each new cluster
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
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
#'
#' # generate bar plot
#' fa_recluster_RPU(recluster_df = reclusterData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster_RPU <- function(
    recluster_df = NULL,
    xaxis = "Cluster",
    yaxis = "Avg. RPU",
    legend_title = "Population",
    plot_title = "Avg. RPU per cluster",
    bar_outline = "black",
    fill_palette_disc = "Dark2"
){

  # for each cluster, compute avg and sterr of RPU
  rpu_summary <- recluster_df %>%
    dplyr::group_by(.data$Cluster) %>%
    dplyr::summarise(
      Avg_RPU.A = mean(.data$RPU.a, na.rm = TRUE),
      StE_RPU.A = stats::sd(.data$RPU.a, na.rm = TRUE) / sum(!is.na(.data$RPU.a)),
      Avg_RPU.B = mean(.data$RPU.b, na.rm = TRUE),
      StE_RPU.B = stats::sd(.data$RPU.b, na.rm = TRUE) / sum(!is.na(.data$RPU.b))
    )

  # move avg and ste to long format, and then merge
  avg_rpu <- rpu_summary %>%
    dplyr::select(.data$Cluster, dplyr::contains("Avg")) %>%
    tidyr::pivot_longer(!.data$Cluster, names_to = "Population", values_to = "Avg_RPU", names_prefix = "Avg_RPU\\.")

  ste_rpu <- rpu_summary %>%
    dplyr::select(.data$Cluster, dplyr::contains("StE")) %>%
    tidyr::pivot_longer(!.data$Cluster, names_to = "Population", values_to = "StE_RPU", names_prefix = "StE_RPU\\.")

  long_rpu <- dplyr::full_join(avg_rpu, ste_rpu, by = c("Cluster", "Population"))

  # make plot
  p <- ggplot2::ggplot(
    long_rpu,
    ggplot2::aes(
      x = .data$Cluster, y = .data$Avg_RPU, fill = .data$Population,
      text = glue::glue("Cluster: {.data$Cluster}, Avg. RPU: {.data$Avg_RPU}, Population: {.data$Population}")
    )
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", colour = bar_outline) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Avg_RPU - .data$StE_RPU, ymax = .data$Avg_RPU + .data$StE_RPU),
      width = 0.2, position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::scale_fill_brewer(palette = fill_palette_disc) +
    ggplot2::labs(x = xaxis, y = yaxis, fill = legend_title, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "recluster_RPU"))

  # return figure
  return(fig)
}

#' Return a bar plot of the average Levenshtein edit distance (LED) per population per cluster
#'
#' `fa_recluster_LED` returns a bar plot that shows the average LED between all sequences in a cluster and that cluster's seed sequence.
#'
#' @param recluster_df A reclustered FASTAptameR data.frame from `fa_recluster`
#' @param xaxis The title of the x-axis (default: "Cluster")
#' @param yaxis The title of the y-axis (default: "Avg. LED")
#' @param plot_title The title of the plot (default: "Avg. LED per cluster")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param bar_fill The color fo the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_recluster()], [fa_recluster_popSize()], [fa_recluster_RPU()], [fa_recluster_enrich()]
#'
#' @return A plotly object showing the average LED between all sequences in the cluster and the cluster seed
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
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
#'
#' # generate bar plot
#' fa_recluster_LED(recluster_df = reclusterData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster_LED <- function(
    recluster_df = NULL,
    xaxis = "Cluster",
    yaxis = "Avg. LED",
    plot_title = "Avg. LED per cluster",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # summarize LED by cluster
  led_df <- recluster_df %>%
    dplyr::group_by(.data$Cluster) %>%
    dplyr::summarise(
      Avg_LED = mean(.data$LED, na.rm = TRUE),
      StE_LED = stats::sd(.data$LED, na.rm = TRUE) / dplyr::n()
    )

  # make plot
  p <- ggplot2::ggplot(led_df, ggplot2::aes(x = .data$Cluster, y = .data$Avg_LED, text = glue::glue("Cluster: {.data$Cluster}, Avg. LED: {.data$Avg_LED}"))) +
    ggplot2::geom_bar(stat = "identity", colour = bar_outline, fill = bar_fill) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$Avg_LED - .data$StE_LED, ymax = .data$Avg_LED + .data$StE_LED),
      width = 0.2, position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "recluster_LED"))

  # return figure
  return(fig)
}

#' Return a box plot of the sequence enrichment per cluster
#'
#' `fa_recluster_enrich` returns a box plot that shows sequence enrichment statistics for each cluster.
#'
#' @param recluster_df A reclustered FASTAptameR data.frame from `fa_recluster`
#' @param xaxis The title of the x-axis (default: "Cluster")
#' @param plot_title The title of the plot (default: "Sequence enrichment per super-cluster")
#' @param box_outline The color of the box outlines (default: "black")
#' @param box_fill The color of the box fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_recluster()], [fa_recluster_popSize()], [fa_recluster_RPU()], [fa_recluster_LED()]
#'
#' @return A plotly object with a box plot showing the sequence enrichment for each cluster
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files, cluster data, recluster populations together
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
#'
#' # generate box plot
#' fa_recluster_enrich(recluster_df = reclusterData)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_recluster_enrich <- function(
    recluster_df = NULL,
    xaxis = "Cluster",
    plot_title = "Sequence enrichment per super-cluster",
    box_outline = "black",
    box_fill = "skyblue"
){

  # make plot
  p <- ggplot2::ggplot(recluster_df, ggplot2::aes(x = as.factor(.data$Cluster), y = .data$Enrichment)) +
    ggplot2::geom_boxplot(colour = box_outline, fill = box_fill) +
    ggplot2::labs(x = xaxis, title = plot_title) +
    ggplot2::theme_bw()

  # make bold plot
  p <- boldPlots(p)

  # make interactive figure
  fig <- plotly::ggplotly(p) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "recluster_enrich"))

  # return interactive figure
  return(fig)
}
