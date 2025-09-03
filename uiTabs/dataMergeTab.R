# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

dataMergeTab <- tabPanel(
  "Data Merge",
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ask for input files
      shinyBS::tipify(
        fileInput(
          "dataMerge_input",
          label = strong("Input data:"),
          multiple = TRUE,
          placeholder = "FASTA files",
          accept = c(".fasta")
        ),
        "Holding ctrl (Windows) or command (Mac) will allow you to click multiple files."
      ),
      
      # reorder files because fileInput keeps them in alphabetical order
      selectizeInput("dataMerge_selectInput", label = strong("Select file order."), choices = "*", multiple = TRUE),
      
      # radio buttons to define desired merge type
      radioButtons(
        "dataMerge_mergeType",
        label = strong("How should the data be joined?"),
        choices = c("Union", "Intersection", "Left"),
        selected = "Union",
        inline = TRUE
      ),
      shinyBS::bsTooltip(
        "dataMerge_mergeType",
        "Do you want all sequences (Union), shared sequences (intersection), or only sequences from the first population (Left)?"
      ),
      
      # start button
      actionButton("dataMerge_start", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button for summary
      downloadButton("dataMerge_download", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for persistence plot
      radioButtons(
        "dataMerge_persistencePlot_custom",
        label = strong("Adjust default persistence plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("dataMerge_persistencePlot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.dataMerge_persistencePlot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("dataMerge_persistencePlot_xaxis", label = strong("X-axis - represents number of populations with sequence."), value = "Population count"),
        textAreaInput("dataMerge_persistencePlot_yaxis", label = strong("Y-axis - represents unique sequence count."), value = "Unique read count"),
        textAreaInput("dataMerge_persistencePlot_title", label = strong("Plot title."), value = "Sequence persistence analysis"),
        colourpicker::colourInput("dataMerge_persistencePlot_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("dataMerge_persistencePlot_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # start button for persistence plot
      actionButton("dataMerge_persistencePlotStart", label = h5("Persistence plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("dataMerge_persistencePlotStart", "Shows the number of populations in which sequences are found."),
      shinyBS::bsModal(
        id = "dataMerge_persistencePlotWindow",
        title = "Persistence plot",
        trigger = "dataMerge_persistencePlotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("dataMerge_sequencePersistence", height = "650px"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for UpSet plot
      radioButtons(
        "dataMerge_UpSetPlot_custom",
        label = strong("Adjust default UpSet plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("dataMerge_UpSetPlot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.dataMerge_UpSetPlot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("dataMerge_UpSetPlot_xaxis", label = strong("X-axis - represents number of unique sequences in set."), value = "Sequences per set"),
        textAreaInput("dataMerge_UpSetPlot_yaxis", label = strong("Y-axis - represents sets."), value = "Sequence intersections"),
        colourpicker::colourInput("dataMerge_UpSetPlot_barfill", "Color for bar fill.", "skyblue")
      ),
      
      # start button for UpSet plot
      actionButton("dataMerge_UpSetPlotStart", label = h5("UpSet plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("dataMerge_UpSetPlotStart", "Shows the sequence intersections between populations."),
      shinyBS::bsModal(
        id = "dataMerge_UpSetPlotWindow",
        title = "UpSet plot",
        trigger = "dataMerge_UpSetPlotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("dataMerge_UpSetR", height = "650px"))
      )
    ),
    
    # main panel
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("dataMerge_output")))
  )
)

