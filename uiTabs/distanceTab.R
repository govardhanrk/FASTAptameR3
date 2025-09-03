# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

distanceTab <- tabPanel(
  "Distance",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "distanceInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c('.fasta')
      ),
      
      # ask for query sequence
      textInput("querySequence", label = strong("Query sequence:")),
      shinyBS::bsTooltip("querySequence", "Compute the LED between this sequence and all other sequences in the data."),
      
      # select file type for download
      radioButtons("distanceDownloadType", label = strong("FASTA or CSV download?"), choices = c("FASTA", "CSV"), selected = "FASTA", inline = TRUE),
      shinyBS::bsTooltip("distanceDownloadType", "FASTA is required for subsequent modules; CSV retains all features from data table"),
      
      # start button
      actionButton("distanceStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("distanceDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for distance histograms
      radioButtons(
        "distance_histogram_custom",
        label = strong("Adjust default distance histograms?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("distance_histogram_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.distance_histogram_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("distance_histogram_xaxis", label = strong("X-axis - represents LED from query sequence."), value = "Distance from query"),
        textAreaInput("distance_histogram_yaxis1", label = strong("Y-axis 1 - represents count of unique sequences."), value = "Unique sequences"),
        textAreaInput("distance_histogram_yaxis2", label = strong("Y-axis 2 - represents count of total sequences."), value = "Read count"),
        textAreaInput("distance_histogram_title", label = strong("Plot title."), value = "Distance histograms"),
        
        # colour input for bar fill and outline
        colourpicker::colourInput("distance_histogram_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("distance_histogram_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # start button
      actionButton("distance_histStart", label = h5("Distance histogram"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("distance_histStart", "See a histogram of distances"),
      
      tags$head(tags$style("#distance_histWindow .modal-body{ min-height:650px}")),
      shinyBS::bsModal(
        id = "distance_histWindow",
        title = "Distance histogram",
        trigger = "distance_histStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("distance_histOutput", height = "600px"))
      )
    ),
    
    # display distance output as datatable
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("distanceOutput")))
  )
)
