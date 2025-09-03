# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

recountTab <- tabPanel(
  "Recount",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for first input file
      fileInput(
        "recountInput1",
        label = strong("Choose 1st FASTA to recount:"),
        multiple = FALSE,
        placeholder = "FASTA files",
        accept = c(".fasta")
      ),
      
      # ask for second input file
      fileInput(
        "recountInput2",
        label = strong("Choose 2nd FASTA to recount:"),
        multiple = FALSE,
        placeholder = "FASTA files",
        accept = c(".fasta")
      ),
      
      # slider to select scaling factor (default is 1e6, which leads to RPM)
      radioButtons(
        "recountSlider_scalingFactor",
        label = strong("What value should be used to normalize read counts?"),
        choices = c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6), selected = 1e6,
        inline = TRUE
      ),
      shinyBS::bsTooltip(
        "recountSlider_scalingFactor",
        "1e0, 1e3, and 1e6 are 'Reads', 'Reads per Thousand', and 'Reads per Million', respectively."
      ),
      
      # select file type for download
      radioButtons(
        "recountDownloadType",
        label = strong("FASTA or CSV download?"),
        choices = c("FASTA", "CSV"), selected = "FASTA",
        inline = TRUE
      ),
      shinyBS::bsTooltip(
        "recountDownloadType",
        "FASTA is required for subsequent modules; CSV retains all features from data table"
      ),
      
      # start button
      actionButton("recountStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("recountDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # add space
      tags$br(),
      tags$br(),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # slider for minimum number of reads in "Reads per Rank" plot
      sliderInput(
        "recountSlider_minReads",
        label = strong("Min. number of reads to plot:"),
        min = 0, max = 1000,
        value = 10, step = 10
      ),
      shinyBS::bsTooltip("recountSlider_minReads", "What is the min. number of reads to plot?"),
      
      # slider for maximum rank in "Reads per Rank" plot
      sliderInput(
        "recountSlider_maxRanks",
        label = strong("Max. rank to plot:"),
        min = 10, max = 1000,
        value = 100, step = 10
      ),
      shinyBS::bsTooltip("recountSlider_maxRanks", "How many of the top ranks should be plotted?"),
      
      # plot customizations for RPR plot
      radioButtons(
        "recount_rpr_custom",
        label = strong("Adjust default reads-per-rank plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recount_rpr_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recount_rpr_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recount_rpr_xaxis", label = strong("X-axis - represents sequence rank."), value = "Ranks of unique sequences"),
        textAreaInput("recount_rpr_yaxis", label = strong("Y-axis - represents sequence abundance."), value = "Total reads per unique sequence"),
        textAreaInput("recount_rpr_title", label = strong("Plot title."), value = "Read count for each rank"),
        colourpicker::colourInput("recount_rpr_linecolour", "Color for plot line.", "skyblue"),
      ),
      
      # Reads per rank plot
      actionButton("recount_rprPlotStart", label = h5("Reads per rank"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("recount_rprPlotStart", "Shows a line plot comparing reads and ranks of unique sequences"),
      shinyBS::bsModal(
        id = "recount_rprPlotWindow",
        title = "Reads per rank",
        trigger = "recount_rprPlotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("recount_rprPlotOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for sequence length histogram
      radioButtons(
        "recount_histogram_custom",
        label = strong("Adjust default sequence-length histogram?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recount_histogram_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recount_histogram_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recount_histogram_xaxis", label = strong("X-axis - represents sequence length."), value = "Sequence length"),
        textAreaInput("recount_histogram_yaxis1", label = strong("Y-axis 1 - represents unique sequence count."), value = "Unique sequences"),
        textAreaInput("recount_histogram_yaxis2", label = strong("Y-axis 2 - represents total sequence count."), value = "Read count"),
        textAreaInput("recount_histogram_title", label = strong("Plot title."), value = "Sequence-length histogram"),
        colourpicker::colourInput("recount_histogram_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("recount_histogram_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # start button for seq. length histogram
      actionButton("recount_seqHistStart", label = h5("Sequence-length histogram"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("recount_seqHistStart", "Shows a histogram of sequence lengths."),
      shinyBS::bsModal(
        id = "recount_seqHistWindow",
        title = "Sequence-length histogram",
        trigger = "recount_seqHistStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("recount_seqHistOutput", height = "650px"))
      )
    ),
    
    # display count output as data.table
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("recountOutput")))
  )
)
