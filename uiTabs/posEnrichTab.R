# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

posEnrichTab <- tabPanel(
  "Position Enrichment",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "posEnrichInput", 
        label = strong("Choose data to analyze:"),
        multiple = FALSE,
        placeholder = "Recluster CSV file",
        accept = c(".csv")
      ),
      
      # start button
      actionButton("posEnrich_getClusters", label = h5("Get clusters:"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("posEnrich_getClusters", "This button determines which super-clusters are available for the analysis."),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # slider for top number of clusters to plot
      selectizeInput("posEnrich_cluster", label = strong("Perform analysis for which cluster?"), choices = "*", multiple = FALSE), 
      shinyBS::bsTooltip("posEnrich_cluster", "Which single super-cluster should be analyzed?"),
      
      # get sequence type
      radioButtons(
        "posEnrich_seqType",
        label = strong("Type of sequences?"),
        choices = c("Nucleotide", "AminoAcid"),
        selected = "Nucleotide",
        inline = TRUE
      ),
      shinyBS::bsTooltip("posEnrich_seqType", "Nucleotide or amino acid sequences?"),
      
      # start button
      actionButton("posEnrich_start", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("posEnrich_Download", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for bar plot
      radioButtons(
        "posEnrich_barplot_custom",
        label = strong("Adjust default bar plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("posEnrich_barplot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.posEnrich_barplot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, and plot title
        textAreaInput("posEnrich_barplot_xaxis", label = strong("X-axis - represents position in alignment."), value = "Position"),
        textAreaInput("posEnrich_barplot_yaxis", label = strong("Y-axis - represents mean enrichment at the position."), value = "Avg. enrichment"),
        textAreaInput("posEnrich_barplot_title", label = strong("Plot title."), value = "Enrichment per position"),
        
        # colours for bar fill and outline
        colourpicker::colourInput("posEnrich_barplot_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("posEnrich_barplot_barfill", "Color for bar fill.", "skyblue"),
      ),
      
      # start button for abundance plot
      actionButton("posEnrich_barStart", label = h5("Position enrichment"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("posEnrich_barStart", "Shows the average enrichment of residues at each position of the alignment."),
      shinyBS::bsModal(
        id = "posEnrich_barWindow",
        title = "Position enrichment",
        trigger = "posEnrich_barStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("posEnrich_barplot_output", height = "650px"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for heat map
      radioButtons(
        "posEnrich_heatmap_custom",
        label = strong("Adjust default heat map?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("posEnrich_heatmap_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.posEnrich_heatmap_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title and plot title
        textAreaInput("posEnrich_heatmap_xaxis", label = strong("X-axis - represents position in alignment."), value = "Position"),
        textAreaInput("posEnrich_heatmap_yaxis", label = strong("Y-axis - represents possible characters in alignment."), value = "Character"),
        textAreaInput("posEnrich_heatmap_legend", label = strong("Legend title - represents mean enrichment at the position."), value = "Avg. enrichment"),
        textAreaInput("posEnrich_heatmap_title", label = strong("Plot title."), value = "Enrichment per position per character"),
        
        # palette for tile fill
        selectInput(
          "posEnrich_heatmap_palette",
          label = strong("Fill palette."),
          choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
          selected = "magma",
          multiple = FALSE
        )
      ),
      
      # start button for abundance plot
      actionButton("posEnrich_heatStart", label = h5("Heat map"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("posEnrich_heatStart", "Shows the enrichment of each residue at each position of the alignment."),
      shinyBS::bsModal(
        id = "posEnrich_heatWindow",
        title = "Position enrichment heat map",
        trigger = "posEnrich_heatStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("posEnrich_heatmap_output", height = "650px"))
      )
    ),
    
    # show the data.table that you make in the server
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("posEnrichOutput")))
  )
)
