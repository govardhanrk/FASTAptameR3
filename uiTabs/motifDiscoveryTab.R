# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifDiscoveryTab <- tabPanel(
  
  "Discovery",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "motifDiscovery_input",
        label = strong("Input data*:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c('.fasta')
      ),
      
      # warning about supported alphabet
      h5("*This currently only supports DNA characters, but alternative alphabets will be supported soon!"),
      
      # slider for minimum number of reads to consider
      sliderInput("motifDiscovery_minReads", label = strong("Min. number of reads to consider:"), min = 0, max = 1000, value = 10, step = 5),
      shinyBS::bsTooltip("motifDiscovery_minReads", "Min. number of reads for sequences to be retained in analyis."),
      
      # set min and max lengths for motifs
      sliderInput(
        "motifDiscovery_lengthRange",
        label = strong("Motif length range:"),
        min = 3, max = 15,
        value = c(5, 10), step = 1
      ),
      shinyBS::bsTooltip("motifDiscovery_lengthRange", "The minimum and maximum string lengths to consider in the analysis."),
      
      # start button
      actionButton("motifDiscovery_start", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("motifDiscovery_download", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for scatter plot
      radioButtons(
        "motifDiscovery_scatter_custom",
        label = strong("Adjust default plots?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("motifDiscovery_scatter_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.motifDiscovery_scatter_custom == 'Yes'",
        
        # text input for plot
        textAreaInput("motifDiscovery_scatter_xaxis", label = strong("X-axis - represents motif rank."), value = "Rank by normalized z-score"),
        textAreaInput("motifDiscovery_scatter_yaxis", label = strong("Y-axis - represents -log10(p-value)."), value = "-log10(p)"),
        textAreaInput("motifDiscovery_scatter_legend", label = strong("Legend title - represents motif length."), value = "Length"),
        textAreaInput("motifDiscovery_scatter_title", label = strong("Plot title."), value = "Over-enriched strings"),
        
        # select input for colour palette
        selectInput(
          "motifDiscovery_scatter_palette",
          label = strong("Color palette."),
          choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
          selected = "magma",
          multiple = FALSE
        )
      ),
      
      # start button for abundance plot
      actionButton("motifDiscovery_scatterStart", label = h5("Motif discovery plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("motifDiscovery_scatterStart", "Shows the binned abundance of sequences."),
      shinyBS::bsModal(
        id = "motifDiscovery_scatterWindow",
        title = "Motif discovery plot",
        trigger = "motifDiscovery_scatterStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("motifDiscovery_plot", height = "650px"))
      )
    ),
    
    # display output as plot and data.table
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("motifDiscovery_output")))
  )
)

