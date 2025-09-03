# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

diffAnalysisTab <- tabPanel(
  
  "Differential Analysis",
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ask first condition's input files
      shinyBS::tipify(
        fileInput(
          "diffAnalysis_input1",
          label = strong("Condition 1 files:"),
          multiple = TRUE,
          placeholder = "FASTA files",
          accept = c(".fasta")
        ),
        "Holding ctrl (Windows) or command (Mac) will allow you to click multiple files."
      ),
      
      # ask second condition's input files
      shinyBS::tipify(
        fileInput(
          "diffAnalysis_input2",
          label = strong("Condition 2 files:"),
          multiple = TRUE,
          placeholder = "FASTA files",
          accept = c(".fasta")
        ),
        "Holding ctrl (Windows) or command (Mac) will allow you to click multiple files."
      ),
      
      # slider for minimum number of reads to cluster
      sliderInput("diffAnalysis_pval", label = strong("Cutoff for p-value:"), min = 0.01, max = 0.5, value = 0.1, step = 0.01),
      shinyBS::bsTooltip("diffAnalysis_pval", "Maximum BH-corrected p-value to consider as significant."),
      
      # start button
      actionButton("diffAnalysis_start", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button for summary
      downloadButton("diffAnalysis_download", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for edgeR plot
      radioButtons(
        "edgeR_plot_custom",
        label = strong("Adjust default edgeR plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("edgeR_plot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.edgeR_plot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, and plot title
        textAreaInput("edgeR_plot_xaxis", label = strong("X-axis - represents log(counts per million)."), value = "logCPM"),
        textAreaInput("edgeR_plot_yaxis", label = strong("Y-axis - represents log(fold-change)."), value = "logFC"),
        textAreaInput("edgeR_plot_title", label = strong("Plot title."), value = "edgeR results"),
        
        # colour input for sig. vs not sig. points
        colourpicker::colourInput("edgeR_plot_sigcolour", "Color for significant points.", "red"),
        colourpicker::colourInput("edgeR_plot_insigcolour", "Color for insignificant points.", "black"),
      ),
      
      # start button for edgeR plot
      actionButton("edgeR_plotStart", label = h5("edgeR plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("edgeR_plotStart", "Shows the results of the edgeR analysis."),
      shinyBS::bsModal(
        id = "edgeR_plotWindow",
        title = "edgeR plot",
        trigger = "edgeR_plotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("diffAnalysis_plot", height = "650px"))
      )
    ),
    
    # main panel
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("diffAnalysis_output")))
  )
)
