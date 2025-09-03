#' Return a sequence persistence bar chart
#'
#' `fa_dataMerge_seqPersistence` returns a bar chart that shows the number of populations in which sequences are found (e.g., 100 sequences found in 3 populations, 40 sequences found in 2 populations, etc.).
#'
#' @param faDF_list A list of FASTAptameR data.frames
#' @param xaxis The title of the x-axis (default: "Population count")
#' @param yaxis The title of the y-axis (default: "Unique read count")
#' @param plot_title The title of the plot (default: "Sequence persistence plot")
#' @param bar_outline The color of the bar outlines (default: "black")
#' @param bar_fill The color of the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_dataMerge()], [fa_dataMerge_UpSetR()]
#'
#' @return A plotly object with a bar plot that shows the persistence of sequences across rounds
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countDataList <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE3.fastq")
#' )
#'
#' # make persistence plot
#' fa_dataMerge_seqPersistence(faDF_list = countDataList)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_dataMerge_seqPersistence <- function(
    faDF_list = NULL,
    xaxis = "Population count",
    yaxis = "Unique read count",
    plot_title = "Sequence persistence plot",
    bar_outline = "black",
    bar_fill = "skyblue"
){

  # initialize empty data frame
  seqPersist <- data.frame()

  # iterate through all inputs
  for(i in 1:length(faDF_list)){

    # bind main df with newly read sequences
    seqPersist <- rbind(
      seqPersist,
      faDF_list[[i]] %>% dplyr::select(.data$Sequences, .data$Reads)
    )
  }

  # count number of sequences in 1 round, 2 rounds, etc.
  seqPersist <- seqPersist %>%
    dplyr::pull(.data$Sequences) %>%
    table() %>%
    as.data.frame() %>%
    dplyr::group_by(.data$Freq) %>%
    dplyr::summarise(seqCount = dplyr::n_distinct())

  # make bar plot
  p <- ggplot2::ggplot(
    seqPersist,
    ggplot2::aes(
      .data$Freq, .data$seqCount,
      text = glue::glue(
        "
        No. Rounds Detected: {.data$Freq}
        No. Unique Reads: {.data$seqCount}
        "
      )
    )
  ) +
    ggplot2::geom_bar(stat = "identity", colour = bar_outline, fill = bar_fill) +
    ggplot2::labs(x = xaxis, y = yaxis, title = plot_title) +
    ggplot2::scale_x_continuous(breaks = unique(seqPersist$Freq)) +
    ggplot2::theme_classic()

  # make bold text
  p <- boldPlots(p = p)

  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "sequence_persistence"))

  # return interactive figure
  return(fig)
}

#' Return an UpSetR plot
#'
#' `fa_dataMerge_UpSetR` returns an UpSetR plot showing the sequences shared across multiple populations
#'
#' @param faDF_list A list of FASTAptameR data.frames
#' @param fastaNames A list of names to use in the plot; should be one name per data.frame in faDF_list
#' @param xaxis The title of the x-axis (default: "Sequences per set")
#' @param yaxis The title of the y-axis (default: "Sequence intersections")
#' @param bar_fill The color of the bar fills (default: "skyblue")
#'
#' @seealso [grDevices::colors()], [fa_dataMerge()], [fa_dataMerge_seqPersistence()]
#'
#' @return An UpSet plot
#' @export
#'
#' @examples
#' \dontrun{
#' # count FASTQ files
#' countDataList <- list(
#'   fa_count(dataInput = "PATH/TO/FILE1.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE2.fastq"),
#'   fa_count(dataInput = "PATH/TO/FILE3.fastq")
#' )
#'
#' # make UpSet plot
#' fa_dataMerge_UpSetR(faDF_list = countDataList)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
fa_dataMerge_UpSetR <- function(
    faDF_list = NULL,
    fastaNames = NULL,
    xaxis = "Sequences per set",
    yaxis = "Sequence intersections",
    bar_fill = "skyblue"
){

  # initialize empty list
  sequenceList <- list()

  # create list of lists for sequences
  for(i in 1:length(faDF_list)){
    sequenceList[[i]] <- faDF_list[[i]] %>% dplyr::pull(.data$Sequences)
    names(sequenceList)[i] <- fastaNames[i]
  }

  # make UpSetR plot (not a ggplot2 object, so it cannot be made interactive)
  p <- UpSetR::upset(
    UpSetR::fromList(sequenceList),
    order.by = "freq",
    main.bar.color = bar_fill,
    sets.bar.color = bar_fill,
    sets.x.label = xaxis,
    mainbar.y.label = yaxis,
    text.scale = 2, point.size = 3, line.size = 1,
    mb.ratio = c(0.65,0.35)
  )

  # return plot
  return(p)
}
