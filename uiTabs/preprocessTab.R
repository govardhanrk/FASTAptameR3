# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

preprocessTab <- tabPanel(
  
  "Preprocess",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "preprocessInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "FASTQ or FASTA file",
        accept = c(
          '.FASTQ', '.FQ', '.fastq', '.fq',
          '.FASTA', '.FA', '.fasta', '.fa',
          '.GZ', '.gz'
        )
      ),
      
      # ask for 5' constant region
      textInput("preprocess_const5p", label = strong("Constant 5' region:")),
      shinyBS::bsTooltip("preprocess_const5p", "Flanking sequence on left."),
      
      # ask for 3' constant region
      textInput("preprocess_const3p", label = strong("Constant 3' region:")),
      shinyBS::bsTooltip("preprocess_const3p", "Flanking sequence on right."),
      
      # set min and max lengths for motifs
      sliderInput(
        "preprocess_lengthRange",
        label = strong("Sequence length range:"),
        min = 3, max = 500,
        value = c(10, 100), step = 1
      ),
      shinyBS::bsTooltip("preprocess_lengthRange", "The minimum and maximum sequence lengths to retain after preprocessing."),
      
      # slider for minimum number of reads to cluster
      sliderInput("preprocess_maxError", label = strong("Max allowed error for sequences:"), min = 0.001, max = 1, value = 0.005, step = 0.001),
      shinyBS::bsTooltip("preprocess_maxError", "Maximum average base error per sequence (errors derived from Phred scores)."),
      
      # start button
      actionButton("preprocessStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button for summary
      downloadButton("preprocessDownload", label = h5("Download"), style='padding:2px; font-size:80%')
    ),
    
    # display count output as datatable
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("preprocessOutput")))
  )
)
