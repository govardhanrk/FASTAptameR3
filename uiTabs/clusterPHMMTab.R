# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterPHMMTab <- tabPanel(
  "PHMM",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "clusterPHMMInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "MSA FASTA file",
        accept = c('.fasta')
      ),
      
      # slider for minimum number of reads to cluster
      sliderInput("clusterPHMM_numSeq", label = strong("Simulate how many sequences?"), min = 10, max = 1000, value = 100, step = 5),
      shinyBS::bsTooltip(
        "clusterPHMM_numSeq",
        "Please note that the module will simulate this many sequences, but only unique sequences will be returned."
      ),
      
      # slider for max LED
      sliderInput(
        "clusterPHMM_seqLength",
        label = strong("Length of simulated sequence (including gaps):"),
        min = 10, max = 200, value = 60, step = 1
      ),
      shinyBS::bsTooltip("clusterPHMM_seqLength", "This is the largest possible length, but returned sequences may be shorter."),
      
      # start button
      actionButton("clusterPHMMStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download buttons
      downloadButton("clusterPHMMDownload_PHMM", label = h5("Download PHMM"), style='padding:2px; font-size:80%'),
      downloadButton("clusterPHMMDownload_seqs", label = h5("Download sequences"), style='padding:2px; font-size:80%')
    ),
    
    # display count output as datatable
    mainPanel(
      shinycssloaders::withSpinner(tableOutput("clusterPHMMOutput"))
    )
  )
)

