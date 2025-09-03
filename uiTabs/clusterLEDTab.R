# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterLEDTab <- tabPanel(
  
  "Cluster",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "clusterInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c('.fasta')
      ),
      
      # slider for minimum number of reads to cluster
      sliderInput("clusterSlider_minReads", label = strong("Min. number of reads to cluster:"), min = 0, max = 1000, value = 10, step = 5),
      shinyBS::bsTooltip("clusterSlider_minReads", "Min. number of reads for sequences to be clustered"),
      
      # slider for max LED
      sliderInput("clusterSlider_maxLED", label = strong("Max. LED:"), min = 1, max = 20, value = 7, step = 1),
      shinyBS::bsTooltip("clusterSlider_maxLED", "Max. edit distance from seed sequence"),
      
      # slider for total number of desired clusters
      sliderInput("clusterSlider_totalClusters", label = strong("Max. number of clusters to generate:"), min = 5, max = 1000, value = 20, step = 5),
      shinyBS::bsTooltip("clusterSlider_totalClusters", "Total number of desired clusters"),
      
      # button to optionally remove non-clustered sequences
      radioButtons("clusterButton_keepNC", label = strong("Keep non-clustered sequences?"), choices = c("Yes", "No"), selected = "No", inline = TRUE),
      shinyBS::bsTooltip("clusterButton_keepNC", "Non-clustered sequences will have NC in their IDs."),
      
      # select file type for download
      radioButtons("clusterDownloadType", label = strong("FASTA or CSV download?"), choices = c("FASTA", "CSV"), selected = "FASTA", inline = TRUE),
      shinyBS::bsTooltip("clusterDownloadType", "FASTA is required for subsequent modules; CSV retains all features from data table"),
      
      # start button
      actionButton("clusterStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button for summary
      downloadButton("clusterDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # show console output
      shinyjs::useShinyjs(),
      strong(textOutput("clusterTextOutput"))
    ),
    
    # display count output as datatable
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("clusterOutput")))
  )
)
