# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterMSATab <- tabPanel(
  "MSA",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "clusterMSAInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "Clustered FASTA file",
        accept = c('.fasta')
      ),
      
      # start button
      actionButton("clusterMSA_getClusters", label = h5("Get clusters:"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("clusterMSA_getClusters", "This button determines which clusters are available for the analysis."),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # slider for top number of clusters to plot
      selectizeInput("msa_cluster", label = strong("Perform MSA for which cluster?"), choices = "*", multiple = FALSE), 
      shinyBS::bsTooltip("msa_cluster", "Which single cluster should be aligned?"),
      
      # get sequence type
      radioButtons(
        "msa_seqType",
        label = strong("Type of sequences?"),
        choices = c("Nucleotide", "AminoAcid"),
        selected = "Nucleotide",
        inline = TRUE
      ),
      shinyBS::bsTooltip("msa_seqType", "Nucleotide or amino acid sequences?"),
      
      # select file type for download
      radioButtons(
        "clusterMSADownloadType",
        label = strong("FASTA or CSV download?"),
        choices = c("FASTA", "CSV"), selected = "FASTA",
        inline = TRUE
      ),
      shinyBS::bsTooltip(
        "clusterMSADownloadType", "Should results be downloaded as a FASTA or CSV?"
      ),
      
      # start button
      actionButton("clusterMSAStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("clusterMSADownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for entropy plot
      radioButtons(
        "clusterMSA_entropy_custom",
        label = strong("Adjust default entropy plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("clusterMSA_entropy_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.clusterMSA_entropy_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("clusterMSA_entropy_xaxis", label = strong("X-axis - represents position in MSA."), value = "MSA position"),
        textAreaInput("clusterMSA_entropy_yaxis", label = strong("Y-axis - represents entropy in nats."), value = "Entropy (nats)"),
        textAreaInput("clusterMSA_entropy_legend", label = strong("Legend title - represents nats (a unit of entropy)."), value = "Nats"),
        textAreaInput("clusterMSA_entropy_title", label = strong("Plot title."), value = "Entropy by position in MSA"),
        
        # colour input for bar outline and fill
        colourpicker::colourInput("clusterMSA_entropy_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("clusterMSA_entropy_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # start button
      actionButton("clusterMSA_entropyStart", label = h5("MSA entropy"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("clusterMSA_entropyStart", "Entropy per position of the MSA"),
      
      tags$head(tags$style("#clusterMSA_entropyWindow .modal-body{ min-height:650px}")),
      shinyBS::bsModal(
        id = "clusterMSA_entropyWindow",
        title = "Cluster MSA entropy",
        trigger = "clusterMSA_entropyStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("clusterMSAOutput_entropy", height = "600px"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for MI plot
      radioButtons(
        "clusterMSA_mutinfo_custom",
        label = strong("Adjust default mutual information plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("clusterMSA_mutinfo_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.clusterMSA_mutinfo_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("clusterMSA_mutinfo_xaxis", label = strong("X-axis - represents position in MSA."), value = "MSA position"),
        textAreaInput("clusterMSA_mutinfo_yaxis", label = strong("Y-axis - represents position in MSA."), value = "MSA position"),
        textAreaInput("clusterMSA_mutinfo_legend", label = strong("Legend title - mutual information (MI)."), value = "MI"),
        textAreaInput("clusterMSA_mutinfo_title", label = strong("Plot title."), value = "Pairwise mutual information in MSA"),
        
        # select input for fill palette
        selectInput(
          "clusterMSA_mutinfo_palette",
          label = strong("Fill palette."),
          choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
          selected = "magma",
          multiple = FALSE
        )
      ),
      
      # start button
      actionButton("clusterMSA_mutinfoStart", label = h5("MSA mutual information"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("clusterMSA_entropyStart", "Mutual information between positions of the MSA"),
      
      tags$head(tags$style("#clusterMSA_mutinfoWindow .modal-body{ min-height:650px}")),
      shinyBS::bsModal(
        id = "clusterMSA_mutinfoWindow",
        title = "Cluster MSA mutual information",
        trigger = "clusterMSA_mutinfoStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("clusterMSAOutput_mutinfo", height = "600px"))
      )
    ),
    
    # display count output as data.table
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("clusterMSAOutput")))
  )
)
